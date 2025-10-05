(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Parser;;
open Emitter;;
open Genfunc;;
open Planner;;
open Types;;

let (>>==) = Result.bind;;

type vm = {
        spine:  spinval array
    ;   source: source
    ;   mutable stkidx: int 
    ;   mutable frmptr: int
};;

let debug_stack { spine; stkidx; _ } = 
    let _ = Format.printf "Stacktrace ============\n" in
    Array.iteri (fun x y ->
        (* NB: stkidx is just pointing to where we will write the next value! we
           skip it when printing! *)
        if x >= stkidx then
            ()
        else
            let _ = Format.printf "%d -> %s\n" x (Types.show_spinval y) in
            Format.print_flush ()
    ) spine 
;;

let get_stack idx ({ frmptr; spine; _ }) = 
    Array.get spine (frmptr + idx)
;;

let set_stack idx sval { frmptr; spine; _ } = 
    Array.set spine (frmptr + idx) sval
;;

let push s sval = 
    let _ = Array.set s.spine (s.stkidx) sval in 
    s.stkidx <- (s.stkidx + 1)
;;

(* pop a value from the execution stack by changing the stack pointer - it
   is worth remembering that the stack pointer just points where a value will be
   WRITTEN it doesn't necessarily mean that value being pointed is in use so
   when execution starts it points at nil but at the end it will likely have
   been overwritten by some other value :-) *)
let pop s = 
    let v = Array.get s.spine (s.stkidx - 1) in 
    let _ = s.stkidx <- (s.stkidx - 1) in 
    v
;;

let peek s = 
    Array.get s.spine (s.stkidx - 1)
;;

let load_kernel_addr vm count = 
    (*let _ = debug_stack vm in*)
    (*let _ = Format.printf "collecting %d vars\n" count in *)
    let rec collect add indx = 
        if indx == count then 
            let add' = List.rev add in
            (*let _ = Format.printf "load addr: %s\n" (Genfunc.string_of_shape add') in*)
            (*let _ = Format.print_flush () in*)
            push vm (SAddr (Array.of_list add'))
        else
            (match pop vm with 
            | SIndex idx -> 
                collect (idx :: add) (indx + 1)
            | sp -> 
                let _ = debug_stack vm in 
                failwith (Format.sprintf "Expected index, found %s" (show_spinval sp))
            )
    in 
    collect [] 0
;;

(* TODO: standardize order *)
let print_kernel vm = 
    let indx = peek vm in
    match (indx) with 
    | (SKern _i) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), _modl) as _g -> 
                (
                    let n = show_spinval (vm.source.kernels.(_i)) in 
                    let b = Buffer.create 256 in
                    let _ = Buffer.add_string b ("      " ^ n) in 
                    let _ = Buffer.add_char b '\n' in 
                    let _ = M.iteris (fun _ -> 
                        Buffer.add_string b "\r    "
                    ) (fun _d v -> 
                        Buffer.add_string b (Format.sprintf " |% 7.2f|" v)
                    ) 
                    (fun _ -> 
                        Buffer.add_string b "    \n"
                    ) _modl in
                    Format.printf "%s\n" (Buffer.contents b)
                )
            | _ -> failwith "invalid kernel!"
        )
    | _s -> 
        let _ = debug_stack vm in 
        failwith (Format.sprintf "Unable to print kernel using %s " (show_spinval indx))
;;

(* TODO: standardize order *)
let write_kernel_val vm = 
    let addr = pop vm in
    let indx = pop vm in
    let data = pop vm in
    match (indx, addr, data) with 
    | (SKern _i, SAddr _a, SNumber _f) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), _modl) as _g -> 
                let _ = M.set _modl _a _f in
                ()
            | _ -> failwith "invalid kernel!"
        )
    | (SAddr _a, SKern _i, SNumber _f) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), _modl) as _g -> 
                let _ = M.set _modl _a _f in
                ()
            | _ -> failwith "invalid kernel!"
        )
    | _  -> 
        let _ = debug_stack vm in 
        failwith (Format.sprintf "Unable to load kernel using params %s and %s " (show_spinval indx) (show_spinval addr))
;;

(* TODO: standardize order *)
let load_kernel_val vm = 
    let addr = pop vm in
    let indx = pop vm in
    match (indx, addr) with 
    | (SKern _i, SAddr _a) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), _modl) as _g -> 
                let _fv = M.get _modl _a in
                let _ = push vm (SNumber _fv) in 
                ()
            | _ -> failwith "invalid kernel!"
        )
    | (SAddr _a, SKern _i) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), _modl) as _g -> 
                let _fv = M.get _modl _a in
                let _ = push vm (SNumber _fv) in 
                ()
            | _ -> failwith "invalid kernel!"
        )
    | _  -> 
        let _ = debug_stack vm in 
        failwith (Format.sprintf "Unable to load kernel using params %s and %s " (show_spinval indx) (show_spinval addr))
;;

let binop s f = 
    match f with 
    | IAdd -> push s @@ Types.sadd (pop s) (pop s)
    | IMul -> push s @@ Types.smul (pop s) (pop s)
    | _    -> failwith "Unhandled binary op"
;;

let reset_vm v =
    v.source.cursor <- 0; 
    { v with stkidx = 0; spine=(Array.make 16 SNil) }
;;

(* consume instructions and return the number of places to jump *)
let rec consume ({ Types.oprtns; cursor; _ } as s) apply = 
    if cursor >= Array.length oprtns then 
        ()
    else
        let _ = s.cursor <- s.cursor + apply oprtns.(cursor) in
        consume s apply
;;

let handle_op vm op = 
    (*let _ = Format.printf "handling: %s\n" (show_instr op) in*)
    (*let _ = Format.print_flush () in*)
    (*let _ = debug_stack vm in*)
    (*let _ = Unix.sleepf 0.05 in*)
    match op with
    | INop          -> 1 
    | IPop          -> let _ = pop  vm in 1 
    | IPush v       -> let _ = push vm v in 1
    | ILoop x       -> let _ = vm.source.cursor <- x in 0 
    | IJump y       -> y 
    | IJumpFalse z  -> if (Types.strue @@ Types.seql (pop vm) (SBool false)) then !z else 1
    | IAdd          -> let _ = binop vm (IAdd) in 1 
    | IMul          -> let _ = binop vm (IMul) in 1 
    | INot          -> let _ = push  vm (Types.snot (pop vm)) in 1 
    | ILess         -> let _ = push  vm ((Types.sless   ) (pop vm) (pop vm)) in 1 
    | IGreater      -> let _ = push  vm ((Types.sgreater) (pop vm) (pop vm)) in 1 
    | IConst  _c    -> let _ = push  vm (get_const _c vm.source) in 1 
    | IGetVar _g    -> let _ = push  vm (get_stack _g vm) in 1 
    | ISetVar _g    -> let _ = set_stack _g (pop vm) vm in 1 
    | IEchoNl       -> let _ = (Format.printf " %s\n" (Types.show_spinval (peek vm))) in 1
    | IEcho         -> let _ = (Format.printf " %s"   (Types.show_spinval (peek vm))) in 1
    | IGetKern      -> let _ = load_kernel_val  vm in 1
    | ISetKern      -> let _ = write_kernel_val vm in 1
    | IEchoKern     -> let _ = print_kernel vm in 1
    | ILoadAddr _a  -> let _ = load_kernel_addr vm _a in 1 
;;

let eval (pr: vm) = 
    consume pr.source (handle_op pr)
;;

let tosource (vw: program) = 
    (>>==) (Genfunc.transform vw) (fun x -> 

        let mtch, out = x.outs in

        (* load kernel indexes onto the stack 
           return output and input kernel indexes *)
        let (_outkidx, _kidxs, gl) = Emitter.genloop (presempty "") (List.map (fun y -> (y.shape, y.param) ) x.inps) out in 

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
                (fun _i _dcl x _y -> x)) 
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
                        match mtch with 
                        | None -> 
                            (* no output match means scalar value so no addr *)
                            let addr = [ ILoadAddr 0; ] in
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
        let echokerns = List.map (Funcs.print_kern) (_kidxs) |> List.concat in
        Ok { 
            vl with oprtns=vl.oprtns @ echokerns @ (Funcs.print_kern _outkidx)
        } 
    )
;;

let mkvm src = {
    spine  = Array.make 256 SNil
    ;   stkidx = 0
    ;   frmptr = 0 
    ;   source = src
}

