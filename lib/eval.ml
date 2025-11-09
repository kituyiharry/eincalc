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
open Planner;;
open Types;;

let (>>==) = Result.bind;;

(* TODO: constant time VM operations *)

type vm = {
        spine:  spinval array
    ;   source: source
    ;   mutable stkidx: int 
    ;   mutable frmptr: int
    ;   mutable oldframe: int list
    ;   sheet:  Ndcontroller.gridcontroller
};;

let debug_stack { spine; stkidx; source; _ } = 
    let _ = Format.printf "Stacktrace ============\n" in
    for i = (stkidx - 1) downto 0 do 
        let y = spine.(i) in
        (match y with 
            | SNdim _n ->  (show_kernel y) 
            | SKern ix -> (show_kernel source.kernels.(ix))
            | _ -> 
                let _ = Format.printf "%d -> %s\n" i (Types.show_spinval y) in
                Format.print_flush ()
        )
    done
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
    let v = Array.unsafe_get s.spine (s.stkidx - 1) in 
    let _ = s.stkidx <- (s.stkidx - 1) in 
    v
;;

(* Auto resolves pointers to kernels *)
let popresolve s = 
    let v = Array.unsafe_get s.spine (s.stkidx - 1) in 
    let _ = s.stkidx <- (s.stkidx - 1) in 
    (match v with 
        | SKern idx ->
            s.source.kernels.(idx)
        | _ -> v 
    )
;;

let peek s = 
    Array.get s.spine (s.stkidx - 1)
;;

(* FIXME: based on implementation this would only be called for einsum outputs
   so needs to be such that we don't need hardcoding but for now it works!  *)
let apply_masks pr = 
    let s = Emitter.transform_mask pr.sheet (pr.source.kernels.(0)) pr.source.pmasks in 
    let _ = pr.source.kernels.(0) <- s in 
    (* TODO: decide on proper stack behavior -> this could clash with how
       genloop also emits this instruction causing for artifacts on the stack *)
    (*push pr (SKern 0)*)
    ()
;;

let apply_masks_list pr ml = 
    (match (pop pr) with 
    | SKern idx -> 
        let s = Emitter.transform_mask pr.sheet (pr.source.kernels.(idx)) ml in 
        let _ = pr.source.kernels.(idx) <- s in 
        push pr (SKern idx)
    |  SIndex i -> 
        let sc = (Ndarray.Scalar.make [||] (float_of_int i)) in
        let s = Emitter.transform_mask pr.sheet (SNdim((module Ndarray.Scalar), sc)) ml in 
        push pr (s)
    |  SNumber n -> 
        let sc = (Ndarray.Scalar.make [||] (n)) in
        let s = Emitter.transform_mask pr.sheet (SNdim((module Ndarray.Scalar), sc)) ml in 
        push pr (s)
    |  SNdim n -> 
        let s = Emitter.transform_mask pr.sheet (SNdim n) ml in 
        push pr (s)
    | _ ->
        (failwith "Mask variable cannot be applied on operand")
    )
;;

let load_kernel_addr vm count = 
    let rec collect add indx = 
        if indx == count then 
            let add' = List.rev add in
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
        (show_kernel (vm.source.kernels.(_i)))
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
    | IAdd -> push s @@ Types.sadd (popresolve s) (popresolve s)
    | IMul -> push s @@ Types.smul (popresolve s) (popresolve s)
    | ISub -> push s @@ Types.ssub (popresolve s) (popresolve s)
    | IDiv -> push s @@ Types.sdiv (popresolve s) (popresolve s)
    | _    -> failwith "Unhandled binary op"
;;

let unaryop s f = 
    match f with 
    | INeg -> push s @@ Types.sneg (popresolve s)
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
    | ISub          -> let _ = binop vm (ISub) in 1 
    | IDiv          -> let _ = binop vm (IDiv) in 1 
    | INeg          -> let _ = unaryop vm (INeg) in 1 
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
    | IApplyMasks   -> let _ = apply_masks vm in 1
    | IApplyMaskList ml -> let _ = apply_masks_list vm ml in 1
    | ISaveFrame ->  let _ = 
        vm.oldframe <- (vm.frmptr :: vm.oldframe); 
        (vm.frmptr <- vm.stkidx) 
    in 1
    | ILoadFrame    -> let _ = (
        match vm.oldframe with
        | [] -> vm.frmptr <- 0 
        | hd :: rem -> 
            (vm.frmptr <- hd); vm.oldframe <- rem
    ) in 1
;;

let timeonly f =
    let t = Unix.gettimeofday () in
    let _res = f () in
    (Unix.gettimeofday () -. t)
;;

(* access grid via controller *)
let eval (pr: vm) = 
    (*let _ = pprint_instr pr.source.oprtns in*)
    let tval = timeonly (fun _ -> consume pr.source (handle_op pr))
    in let _ = debug_stack pr
    in 
    (*let _ =*)
        (*for i = Array.length pr.source.kernels downto 1 do *)
            (*(show_kernel) pr.source.kernels.(i - 1);*)
        (*done;*)
    (*in*)
    Format.printf "\nexec %f secs\n" tval
;;

let tosource (controller) (vw: program) = 
    (>>==) (Genfunc.transform vw) (prepare_expression (presempty "") controller)
;;

let mkvm controller src = {
        spine    = Array.make 256 SNil
    ;   stkidx   = 0
    ;   frmptr   = 0 
    ;   oldframe = []
    ;   source   = src
    ;   sheet    = controller
}

