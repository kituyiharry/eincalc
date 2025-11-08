(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Parser;;
open Emitter;;

type spinplan = { 
        loopvars: einmatch list 
    ;   summvars: einmatch list
    ;   outshape: int list
    ;   charset : CharSet.t     [@opaque]
} [@@deriving show];;

let emptyspinplan = {
        loopvars = []
    ;   summvars = [] 
    ;   outshape = []
    ;   charset  = CharSet.empty
};;

let create_plan eincomps = 
    let (_, shp, _) = eincomps.outs in
    let plan = { emptyspinplan with outshape=(shp) } in
    List.fold_left (fun acc v -> 
        List.fold_left (fun acc' (w: einmatch) -> 
            if CharSet.mem w.label acc'.charset then
                acc'
            else
                (if w.outlc < 0 then
                    { acc' with summvars=acc'.summvars @ [ w ]; charset=(CharSet.add w.label acc'.charset) }
                else
                    { acc' with loopvars=acc'.loopvars @ [ w ]; charset=(CharSet.add w.label acc'.charset) }
                )
        ) acc v.elems
    ) plan eincomps.inps 
;;

(* prepare specifically einsum expressions *)
let prepare_eintree sidx ps controller x = 

    (* get the match and output shape and post execution masks *)
    let mtch, out, masks = x.outs in

    (* load kernel indexes onto the stack 
           return output and input kernel indexes *)

    (* TODO: represent param as shape transformation types so we don't need
           to recalculate it while masking! *)
    let (_outkidx, _kidxs, gl) = Emitter.genloop sidx controller ps 
        (List.map (fun y ->
            (*FIXME: redundant restore char indexes for mask check AGAIN. can be cached in genfunc *)
            let cl = List.map (fun c -> (c.label, c.index)) y.elems in
            (y.shape, y.param, y.masks, cl) 
        ) x.inps) out in 

    (* each parameter input with its associated kernel index added by genloop *)
    let  _mapidx = List.of_seq @@ Seq.zip (x.inps |> List.to_seq) (List.to_seq _kidxs)  in

    let plan = create_plan x in

    (*let _ = Format.printf "\nTree: %s \n\n" (show_eintree x) in*)
    (*let _ = Format.printf "\nPlan: %s \n\n" (show_spinplan plan) in*)

    (* loop vars first, sumvars last *)
    let vlist = plan.loopvars @ plan.summvars in

    let vl = (
        vlist
        |> (gl 
            (* before body on each iteration *)
            (fun _i _dcl x _y _z _a -> x) 
            (* after body on each iteration *)
            (fun _i _dcl x _y -> 
                x
            )) 
            (* body *)
            (fun _i _dcl islast _e ps -> 
                (* all vars have been loaded, *)
                if islast then 
                    (* for each parameter and input combined *)
                    let i = List.mapi (fun _idx ((e: eincomp), m) ->
                        let _vars = (List.map (fun v -> v.label) e.elems) in
                        (* first index - we wait for 2nd variable *)
                        if _idx = 0 then        
                            (
                                Funcs.load_arr_addr _vars ps.nmdvar
                                |> Funcs.fetch_arr_var m
                            )
                        else 
                            (
                                (
                                    Funcs.load_arr_addr _vars ps.nmdvar
                                    |> Funcs.fetch_arr_var m
                                ) @  [ IMul ]
                            )
                    ) _mapidx |> List.concat in 
                    (* are we summing only or multiply-add *)
                    match mtch with 
                    | None -> 
                        (* no output match means scalar value so no addr *)
                        let addr = [ Types.ILoadAddr 0; ] in
                        let fin = (Funcs.compose
                            (Funcs.fetch_arr_var _outkidx addr) 
                            IAdd
                            (Funcs.write_arr_var _outkidx addr)
                        )
                        in { ps with oprtns=ps.oprtns @ i @ fin; }
                    | Some e ->
                        let _vars = (List.map (fun v -> v.label) e) in
                        (* load the variables addressing the output kernel *)
                        let addr = Funcs.load_arr_addr _vars ps.nmdvar in
                        (* get the current value and add it to what was
                               already there on the stack and write it back onto
                               the kernel *)
                        let fin = (Funcs.compose
                            (addr |> Funcs.fetch_arr_var _outkidx) 
                            IAdd
                            (addr |> Funcs.write_arr_var _outkidx) 
                        )
                        in { ps with oprtns=ps.oprtns @ i @ fin; }
                else ps
            )
    ) in 
    (* print out the kernel at the end of execution *)
    (*let echokerns = List.map (Funcs.print_kern) (_kidxs) |> List.concat in*)
    (* place the output kernel on top of the stack after we finish executing *)
    Ok { 
        vl with oprtns=vl.oprtns @ [ Types.IApplyMasks ] 
            (*@ *)
            (*echokerns @*)
            (*(Funcs.print_kern _outkidx) *)
            @ [ IPush (SKern _outkidx); ]
        ; pmasks=masks
    } 
;;

let oplen x = List.length x.oprtns;;

let prepare_expression ps controller ex = 
    let rec _prepare sidx ps controller stm = 
        (match stm with 
            |  Literal (EinSpec (_ein, _par, _tree)) ->  
                (match _tree with 
                    | Some x -> 
                        let* ps' = prepare_eintree (sidx+1) ps controller x in 
                        Ok  { ps' with oprtns=[ Types.ISaveFrame ] @ ps'.oprtns @ [ Types.ILoadFrame ]}
                    | None -> 
                        Error (Format.sprintf "tried eval on unparsed einsum %s!" (show_einsum _ein))
                )
            | Literal (Number n) -> 
                Ok { ps with oprtns=(ps.oprtns @ [ IPush (SNumber n) ]) }
            | Literal (Tensor _n) -> 
                (* FIXME: its possible to sneak in a bad tensor operation *)
                let* ishp = Genfunc.calcshape _n in
                let g = range_to_ndarray controller _n ishp in 
                Ok { ps with oprtns=(ps.oprtns @ [ IPush g ]) }
                (*Error "tensor op not supported now"*)
            | Factor _ | Term _ ->
                Ok ps
            | Unary  (u, e) -> 
                let* e'  = _prepare sidx ps controller e in 
                (match u with 
                | Negate -> 
                    Ok { e' with oprtns=(e'.oprtns @ [ INeg ]) }
                )
            | Binary (l, op, r) -> 
                (* TODO: check whether left and right maps agree *)
                let* l'  = _prepare sidx ps controller l in 
                let* r'  = _prepare ((oplen l') + sidx) { l' with oprtns=[] } controller r  in 
                (match op with 
                | Factor Div -> 
                    Ok { r' with oprtns=(l'.oprtns @  r'.oprtns) @ [ IDiv ] }
                | Factor Mul -> 
                    Ok { r' with oprtns=(l'.oprtns @  r'.oprtns) @ [ IMul ] }
                | Term   Sub -> 
                    Ok { r' with oprtns=(l'.oprtns @  r'.oprtns) @ [ ISub ]  }
                | Term   Add -> 
                    Ok { r' with oprtns=(l'.oprtns @  r'.oprtns) @ [ IAdd ] }
                | _           ->
                    Error "unrecognized binary operation"
                )
            | Reduce (ex, ml) -> 
                let* ex' = _prepare sidx ps controller ex in 
                Ok { ex' with oprtns=(ex'.oprtns @ [ IApplyMaskList ml ]) }
            | Grouping grp      -> 
                let* grp' = _prepare sidx ps controller grp in
                Ok grp'
        )
    in
    match ex with 
    | Stmt stm -> 
        _prepare 0 ps controller stm
    | _ -> 
        Error "unhandled expression block"
;;



