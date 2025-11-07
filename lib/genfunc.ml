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
open Ndmodel;;

(*
 *
 * Checked Rules:
 *   -> subscripts with the same letter must have the same dimension size
 *   -> subscripts not appearing in the output will be summed over
 *   -> subcripts in the output cannot be repeated
 *
 *)

type ndshape = 
    | Col of int 
    | Row of int * ndshape list
[@@deriving show];;

(* add shape metadata *)
let metashape (ndarr) = 
    let rec count ndarr =
        match ndarr with 
        | Itemize i -> Col (List.length i)
        | Collect l -> (
            let c, d = (List.fold_left (fun (c, acc) ndarr' -> 
                (c + 1, (count ndarr') :: acc)
            ) (0,[]) l) in 
            Row (c, d)
        )
    in count ndarr
;;

(* check if ndarray is homogenous - return the dimension shapes *)
let homogenous (matshape) =  
    let rec check r shpl = 
        match r with 
        | Col i -> Ok (i :: shpl)
        | Row (c, n) -> (
            match n with 
            | [] -> Ok (c :: 0 :: shpl) 
            (* check inner shapes against the first one - halt at first non-conforming one *)
            | hd :: rest -> (
                (>>==) ((>>==) (check hd []) (fun x -> 
                    (* go through the rest of the rows and ensure they have the
                       same number of columns *)
                    List.fold_left (fun acc g -> 
                        ((>>==) (acc) (fun a ->
                            ((>>==) (check g []) (fun h ->
                                (* maybe i don't need to check the whole list ?? *)
                                if (List.equal (Int.equal) h a) then 
                                    Ok (a) 
                                else
                                    Error "Not homogenous"
                            ))
                        ))
                    ) (Ok (x)) rest
                )) (fun x' -> Ok (c :: (x' @ shpl)))
            )
        )
    in
    check matshape []
;;

let compfromshape param pidx einchars masks dimens = 
    let l' = List.length einchars in 
    let g' = List.length dimens in
    if  l' == g' then
        let chs = ref CharSet.empty in
        let g' = 
            List.combine einchars dimens 
            |> List.map (fun ((label, index), dimen) ->
                (* -1 is ommision by default *)
                chs := CharSet.add label !chs;
                { label; index; dimen; param=pidx; outlc=(-1); }
            )
        in 
        Ok ({ 
                (* add param *)
                shape=(dimens)(* x by y by .... *)
            ;   elems=(g')    (* each dimension *)
            ;   chset=(!chs)  (* Character set of the labels of each dimension *)
            ;   param
            ;   masks         (* masks applied AFTER parameter masks *)
        })
    (* TODO: EDGE case -> scalar will have no shape but we take it as a kind of 1
       length vector. is this a good approach ?? *)
    else if l' = 1 && List.is_empty dimens then
        let chs = ref CharSet.empty in
        let g' = 
            List.combine einchars [1] 
            |> List.map (fun ((label, index), dimen) ->
                (* -1 is ommision by default *)
                chs := CharSet.add label !chs;
                { label; index; dimen; param=pidx; outlc=(-1); }
            )
        in 
        Ok ({ 
                (* add param *)
                shape=(dimens)(* x by y by .... *)
            ;   elems=(g')    (* each dimension *)
            ;   chset=(!chs)  (* Character set of the labels of each dimension *)
            ;   param
            ;   masks         (* masks applied AFTER parameter masks *)
        })
    else
        (* TODO: handle 1+ dimension reshape e.g 3x2 can be reshaped to 1x3x2 *)
        Error (Format.sprintf "dimension subscript requests %d dimensions but argument %d has %d" l' (pidx+1) g')
;;

let plot_shape_valid plt shp = 
    match plt with 
    | (Scatter _) -> 
        Ok shp
    | _ -> 
        Error "Unhandled shape"
;;

let ensure_keys tbl klist = 
    let idx    = ref (-1) in
    let exists = List.for_all (fun k -> 
        ignore(incr idx);
        Option.is_some (Hashtbl.find_opt tbl k) 
    ) klist in 
    (!idx, exists)
;;

(* l holds the dimensions of the input, m is the mask and map is the infered shape *)
let shape_of_mask m map = 
    let rec findshape m map nest =
        match m with
        | MinMax (_, _) -> 
            Ok map
        | ZScore -> 
            Ok map
        | Mean ->
            Ok []
        | Sum ->
            Ok []
        | Mode -> 
            Ok []
        | Stddev ->
            Ok []
        | Cumsum ->
            Ok map
        | Map _ ->
            Ok map
        | Reshape shp -> 
            if nest > 0 then Error "reshape not currently supported within axis"
            else Ok shp
        | Write _cell -> 
            Ok map
        | Plot { oftype=plt; _ } -> 
            (* ideally the plot doesn't modify any data! *)
            (* for each type of plot we need to verify some dimension *)
            plot_shape_valid plt map
        | Draw ({ handle; elmnts }) -> 
            (* TODO: verify draw calls ?? *)
            List.fold_left (fun r e ->
                if Result.is_error r then r else 
                match e with 
                | Box pr  -> (
                    let keys = ["x";"y";"width";"height"] in
                    let i,t = ensure_keys pr keys in
                    (if t then r else 
                        Error (Format.sprintf "missing property for %s box: %s"
                        (handle)   (List.nth keys i))
                    )
                )
                | Circle pr -> (
                    let keys = ["x";"y";"radius"] in
                    let i,t = ensure_keys pr keys in
                    (if t then r else 
                        Error (Format.sprintf "missing property for %s circle: %s"
                        (handle)    (List.nth keys i))
                    )
                ) 
                | Line pr -> (
                    let keys = ["x";"y";"radius";"fx";"fy"] in
                    let i,t  = ensure_keys pr keys in
                    (if t then r else 
                        Error (Format.sprintf "missing property for %s Line: %s"
                        (handle)    (List.nth keys i))
                    )
                ) 
            ) (Ok map) elmnts
        | Slice _slices -> 
            let sllen = List.length _slices in 
            let mplen = List.length map in
            (if (sllen != mplen) then 
                Error (Format.sprintf "slice parameters do not correspond to input (slice: %d != inp: %d)!" sllen mplen)
            else
                (* TODO: should skipping over the whole slice return an empty
                   tensor - right now we report an error *)
                let rec shapeslice cons slice shape =
                    (match (slice, shape) with
                    | ((hd :: rem),(hd' :: rem')) ->  
                        (match hd with
                        | (Along num) ->  
                            (if (Int.abs num) > hd' then
                                Error "index into slice exceeds dimension"
                            else
                                shapeslice (1 :: cons) rem rem'
                            )
                        | (Select {start;len;skip;}) -> 
                            (match (start, len, skip) with 
                            | (None, None, None) -> 
                                shapeslice (hd' :: cons) rem rem'
                            | (Some st, None, None) -> 
                                if (Int.abs st) > hd' then 
                                    Error "selection into slice exceeds dimension"
                                else
                                    (if st > 0 then
                                        (* select everything items from st *)
                                        shapeslice ((hd' - st) :: cons) rem rem'
                                    else
                                        (* TODO: numpy allows negative start to go beyond the dimension at which the whole slice! *)
                                        shapeslice ((Int.abs st) :: cons) rem rem'
                                    )
                            | (Some st, Some ln, None) -> 
                                if (Int.abs st) > hd' || (ln > hd') then 
                                    Error "selection or length into slice exceeds dimension"
                                else
                                    (if ln > 0 then
                                        (* select ln items from st *)
                                        shapeslice (ln :: cons) rem rem'
                                    else
                                        Error "must be at least 1 length selection!"
                                    )
                            | (Some st, Some ln, Some sk) -> 
                                if (Int.abs st) > hd' || (ln > hd') || (sk >= hd') then 
                                    Error "selection or length or skip over slice exceeds dimension"
                                else
                                    (if ln > 0 && sk > 0 then
                                        (* select alternating including start items from st *)
                                        let sz = (Int.div ln sk)in
                                        let sz = if Int.rem ln sk = 0 then sz else sz + 1 in
                                        shapeslice (sz :: cons) rem rem'
                                    else
                                        Error "must be at least 1 length selection or step!"
                                    )
                            | (Some st, None, Some sk) -> 
                                if (Int.abs st) > hd' || (sk >= hd') then 
                                    Error "selection or length or skip over slice exceeds dimension"
                                else
                                    (if sk > 0 then
                                        (* select alternating including start items from st *)
                                        let sz = (Int.div hd' sk) in
                                        let sz = if Int.rem hd' sk = 0 then sz else sz + 1 in
                                        shapeslice (sz :: cons) rem rem'
                                    else
                                        Error "must be at least 1 length selection!"
                                    )
                            | (_,  _, Some sk) -> 
                                if (sk >= hd') then 
                                    Error "selection skip over slice exceeds dimension"
                                else
                                    (if sk > 0 then
                                        (* select alternating including start items from st *)
                                        let sz = (Int.div hd' sk) in
                                        let sz = if Int.rem hd' sk = 0 then sz else sz + 1 in
                                        shapeslice (sz :: cons) rem rem'
                                    else
                                        Error "must be at least 1 length selection!"
                                    )
                            | _ -> 
                                Error "unhandled selection format!"
                            )
                        )
                    | _ -> 
                        Ok (List.rev cons)
                    )
                in shapeslice [] _slices map
            )
        | Axis (_c, _masks) -> 
            if nest > 0 then Error "axis mask cannot be nested in another axis mask due to ambiguity!" else  
            (* TODO: verify semantics of axis with reshapes - it may be incoherent *)
            (match List.nth_opt map _c with
                | Some dimsize -> 
                        (*let dimsize = List.nth map idx in*)
                        (>>==) (List.fold_left (fun acc maskval -> 
                            match acc with 
                            | Ok a ->
                                (>>==) (findshape maskval a (nest + 1)) (fun rshp ->
                                    (match (Types.cardinal_of_shp rshp, Types.cardinal_of_shp a) with
                                        | (l, r) when l = r -> 
                                            Ok rshp 
                                        | (l, _d) when l = 1 -> 
                                            Ok rshp
                                        | _ -> 
                                            Error (Format.sprintf "improperly structured shape transformation: %s to %s!" 
                                                (Types.string_of_shape a) (Types.string_of_shape rshp)
                                            )
                                    )
                                )
                            | _ -> 
                                acc
                    ) (Ok [dimsize]) _masks) (fun idim -> 
                        match idim with 
                        | [] -> 
                            (* same effect as being summed over where the dimension disappears - nullify that dimension *)
                            List.mapi (fun i v -> if i = _c then -1 else v) map
                            |> List.filter (fun i -> i > 0)
                            |> Result.ok
                        | _vec :: []  ->
                            (* TODO: is this ok - it assumes the axis is wholly mapped in some sort ?? *)
                            let nmap =  List.mapi (fun i v ->
                                if i = _c then 
                                    _vec 
                                else
                                    v
                            ) map in
                            Ok nmap
                        | _ ->
                            Error "reshape across multiple dimensions not supported"
                            (*Ok idim*)
                    )
                | None -> Error "missing axis dimension in input (data could be lower dimension than expected!)!"
            ) 
        (*| Determ -> *)
            (*[]*)
        (*| Cumulative -> *)
            (*map*)
    in findshape m map 0
;;

let calcmaskshapes m lshp = 
    List.fold_left (fun acc maskval -> 
        match acc with 
        | Ok a ->
            (>>==) (shape_of_mask maskval a) (fun rshp -> 
                (match (Types.cardinal_of_shp rshp, Types.cardinal_of_shp a) with
                    | (l, r) when l = r -> 
                        Ok rshp 
                    | (l, _d) when l = 1 -> 
                        Ok rshp
                    | _ -> 
                        (match maskval with 
                        | Axis (_, _) | Slice (_) ->  
                            (* allow these transformations that can
                               disappear or truncate a dimension - errors will
                               be caught *)
                            Ok rshp
                        | _ ->
                            Error (Format.sprintf "improperly structured shape transformation: %s to %s!" 
                                (Types.string_of_shape a) (Types.string_of_shape rshp)
                            )
                        )
                )
            )
        | _ -> 
            acc
    ) (Ok lshp) m
;;

(* calculate shape from checking parameter structure *)
let rec calcshape l c = 
    match c with 
    | Range (_cellstart, _cellend) -> 
        (* get the grid indexes for the references e.g A10 -> (9, 0) *)
        let (sr, sc) = key_of_ref _cellstart in
        let (er, ec) = key_of_ref _cellend in
        (* extract a shape *)
        (match (Int.abs (er - sr), Int.abs (ec - sc)) with 
        | (0, 0)   -> Ok [ ] 
        | (0, c)   -> Ok [ c + 1 ] 
        | (r, c)   -> Ok [ r + 1; c + 1 ]) 
    | Scalar _cell ->
        Ok []
    | NdArray n ->
        (homogenous (metashape n))
    | Mask (r, m) -> 
        (>>==) (calcshape l r) (calcmaskshapes m)
    | Create c -> 
        (match c with
            | Diag  (_vl, shp) -> (
                Ok [shp;shp]
            )   (* matrix with diagonal values *) 
            | Zeros shp -> (
                Ok shp
            )   (* zero init with a shape  *)
            | Ones  shp  -> (
                Ok shp
            )   (* ones init with a shape  *)
            | Fill  (_vl, shp) -> (
                Ok shp
            )   (* fill with a certain value *)
            | Enum  (_vl, _inc, shp) -> (
                Ok shp
            )  (* enumerate from minvalue and increment with a shape *)
            | Rand  (_vl, shp) -> (
                Ok shp
            ) (* random with bound and shape *)
            | Alt (_slc, shp)  -> (
                Ok shp
            )
        )
    | _  -> 
        Error "unimplemented shape calculation!"
;;

let parammatch (({ inp; _ } as e), par) = 
    if List.length inp ==  List.length par then 
        (* Ensure input are unique - maybe use disjoint set *)
        let ins = (
            List.combine inp par
            |> List.mapi (fun pidx ((Shape (l, _m)), param) ->
                (* TODO: use Disjoint Set *)
                (>>==) (calcshape (l) (Mask (param, _m))) (compfromshape param pidx l _m)
            )
        ) in 
        if List.exists (Result.is_error) ins then
            List.filter (Result.is_error) ins 
            |> List.map (Result.get_error)
            |> String.concat ", and " 
            |> Result.error
        else
            Ok (ins |> List.map (Result.get_ok))
    else
        Error (Format.sprintf "Inputs don't correspond to parameters provided for %s" (show_einsum e))
;;

(* check for duplicates via cb and run onxst if duplicate found *)
let dupexist cb onexst lst =
    let rec foil rem =
        match rem with
        | [] -> 
            (* NB: ensure we return the whole list as it is fully valid!! *)
            Ok lst
        | hd::tl ->
            (>>==) (
                (* Does the head exist in its own tail ?? *)
                if List.exists (cb hd) tl then 
                    Error (onexst hd (List.find (cb hd) tl))
                else
                    Ok tl
            ) (foil)
    in foil lst
;;

let dedup cb l = 
    let rec foil rem = 
        (match rem with 
            | []     -> []
            | hd :: tl -> 
                if List.exists (cb hd) tl then
                    (foil tl)
                else
                    hd :: (foil tl)
        )
    in foil l
;;

(* verify stuff about a shape *)
let verify eino comps = 
    (* output shape parameters don't allow for duplicates *)
    (>>==) (dupexist 
        (fun (x,_) (y,_) -> Char.equal x y) 
        (fun (c,_) _ -> (Format.sprintf "Duplicated label %c in output" c)) 
        eino
    )
    (fun x -> 
        (* output elements must be in input *)
        let allc = List.fold_left (fun a x -> CharSet.union a x.chset) CharSet.empty comps in
        match (List.find_opt (fun (x, _) -> not (CharSet.mem x allc)) x) with
        | Some (h, _) ->  Error (Format.sprintf "Output %c not reflected in input" h)
        | None        ->  Ok x
    )
;;

(* Repeated elements must have the same dimension e.g. in ij,jk, j must have
   same dimen value! *)
let repeatcheck (g: eincomp list) = 
    (>>==) (
        List.map (fun x -> x.elems) g
        |> List.concat
        |> dupexist (fun x y -> 
                let v = x.label = y.label && not (Int.equal x.dimen y.dimen)
                (*in let _ = Format.printf "x: %c of %d = y: %c of %d is %b \n" x.label x.dimen y.label y.dimen v *)
                (*in let _ = Format.print_flush () *)
                in v
            ) 
            (fun x y ->
            (* Sometimes broadcasting or contractions happen here !! *)
            Format.sprintf "Repetition of %c with unequal dimensions (%d != %d) for params: %d and %d respectively"
                x.label x.dimen y.dimen x.param y.param)
    ) (fun _clst -> Ok g)
;;

let connect g l =
    (* connect reflected inputs to outputs = ommited items will be <0 *)
    (* NB: also discard label sets as we do not need them *)
    (List.map (fun (v: eincomp) -> 
        { v with elems=(List.map (fun w -> 
            match List.find_opt (fun (c, _) -> Char.equal w.label c) l with 
            | Some (_, i) ->  { w with outlc=i }
            | None        ->    w
        ) v.elems); }
    ) g)
    (* verify the dimensions *)
    |> repeatcheck
;;

(* Dimension checks *)
let correspondence (({out; _}, _) as g) = 
    (>>==) (parammatch g) (fun g' -> 
        match out with
        | None  -> 
            (* verify the repeated dimensions *)
            ((>>==) (repeatcheck g') (fun g'' -> 
                Ok (g'', None)
            ))
        | Some (Shape (_l, _m)) -> 
            (* TODO: verify if this also checks within inputs - done in connect function ! *)
            (>>==) ((>>==) (verify _l g') (connect g')) (fun g'' -> 
            let g' = 
                _l
                |> List.map (fun (label, index) ->
                    (* -1 is ommision by default *)
                    (* NB: ensure dimens are updated later *)
                    { label; index; dimen=(-1); param=(-1); outlc=(-1); }
                )
                in Ok (g'', (Some (g', _m)))
            )
    )
;;

let describe (lst: (einmatch list)) = 
    let buf = Buffer.create (256) in
    let _ = List.iter (fun ({ label=l; index=i; dimen=s; outlc=o; param=p; _ }) -> 
        Buffer.add_string buf 
            (Format.sprintf "\t* Label:%c at (%d -> %d) with size: %d (Param: %d)  |\n" l i o s p)
    ) lst in 
    Buffer.contents buf
;;

(* find dimens from  *)
let find_dimen varname (eincomps: eincomp list) = 
    let dim = ref  0 in
    let _ = List.iter (fun x -> 
        if CharSet.mem varname x.chset then 
            let m = List.find (fun c -> Char.equal varname c.label) x.elems in
            dim := m.dimen
        else
            ()
    ) eincomps
    in !dim
;;

let rec shape_of_expr stm = 
    (match stm with 
        |  Literal (EinSpec (_ein, _par, _tree)) ->  
            (match _tree with 
                | Some x -> 
                    let (_, shp, _) = x.outs in 
                    Ok shp
                | None -> 
                    Error "tried checking einsum shape before parsing finished!"
            )
        | Literal _ | Factor _ | Term _ ->
            Ok []
        | Unary  (_u, e) -> 
            shape_of_expr e
        | Binary (l, op, r) -> 
            (* TODO: check whether left and right maps agree and support
               broadcasted operations! *)
            let* l'  = shape_of_expr l in 
            let* r'  = shape_of_expr r in 
            (if (List.length l' != List.length r') then
                (match (l', r') with 
                    | ([], rem) -> 
                        Ok rem
                    | (1 :: [], rem) -> 
                        Ok rem
                    | (rem, []) -> 
                        Ok rem
                    | (rem, 1 :: []) -> 
                        Ok rem
                    |  _ -> 
                        Error "unmatched shapes cannot be broadcasted together"
                )
            else if List.for_all2 (=) l' r' then
                (match op with 
                    | (Factor Div) | (Factor Mul) | (Term   Add) | (Term   Sub) -> 
                        Ok l' 
                    | _ -> Error "unable to get final shape from binary operation"
                )
            else
                (* TODO: maybe other operations like cross and dot product work?? *)
                Error "binary operation correspondence error!"
            )
        | Reduce (ex, ml) -> 
            let* ex' = shape_of_expr ex in 
            calcmaskshapes ml ex'
        | Grouping grp      -> 
            shape_of_expr grp
    )
;;

let transform (e: formula)  = 

    let rec walkformulae stm = 
        (match stm with 
            |  Literal (EinSpec (_ein, _par, _tree)) ->  
                (match _tree with 
                | Some _x -> Ok stm
                | None -> 
                    let* (lin, lout) = (correspondence (_ein, _par)) in 
                    (match lout with 
                        | Some (vout, maskl) -> 
                            let upd = List.map (fun (x) -> { x with dimen = (find_dimen x.label lin) }) vout in
                            let eq = List.map (fun x -> x.dimen) upd in
                            let newtree = ({ inps=lin; outs=(Some upd, eq, maskl); }) in 
                            let (_, _shp, _ ) = newtree.outs in 
                            Ok (Literal (EinSpec (_ein, _par, Some newtree)))
                        | None -> 
                            let newtree = { inps=lin; outs=(None, [], []); } in
                            Ok (Literal (EinSpec (_ein, _par, Some newtree)))
                    )
                )

            | Literal _ | Factor _ | Term _ ->
                Ok stm
            | Unary  (u,e) -> 
                let* e'  = walkformulae e in 
                Ok (Unary (u, e'))
            | Binary (l, op, r) -> 
                (* TODO: check whether left and right maps agree *)
                let* l'  = walkformulae l  in 
                let* op' = walkformulae op in 
                let* r'  = walkformulae r  in 
                Ok (Binary (l', op', r'))
            | Reduce (ex, ml) -> 
                let* ex' = walkformulae ex in 
                Ok (Reduce (ex', ml))
            | Grouping grp      -> 
                let* grp' = walkformulae grp in
                Ok (Grouping grp')
        )
    in
    match e with 
    | Stmt stm -> 
        let* stm' = (walkformulae stm) in 
        let* shp  = (shape_of_expr stm') in
        let  _    = Format.printf "final shape: %s\n" (Types.string_of_shape shp) in
        Ok (Stmt stm')
    | _ -> 
        failwith "Unimplemented formula transform"
;;

