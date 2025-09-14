(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Emitter;;

type vm = {
        spine:  Emitter.spinval array
    ;   source: Emitter.source
    ;   mutable stkidx: int 
    ;   mutable frmptr: int
};;

let get_stack idx { frmptr; spine; _ } = 
    Array.get spine (frmptr + idx)
;;

let push s sval  = 
    let _ = Array.set s.spine (s.stkidx) sval in 
    s.stkidx <- (s.stkidx + 1)
;;

let pop s = 
    let v = Array.get s.spine (s.stkidx - 1) in 
    let _ = s.stkidx <- (s.stkidx - 1) in 
    v
;;

let binop s f = 
    match f with 
    | IAdd -> push s @@ Emitter.sadd (pop s) (pop s)
    | IMul -> push s @@ Emitter.smul (pop s) (pop s)
    | _    -> failwith "Unhandled binary op"
;;

let reset_vm v =
    v.source.cursor <- 0; 
    { v with stkidx = 0; spine=(Array.make 3 SNil) }
;;

(* consume instructions and return the number of places to jump *)
let rec consume ({ Emitter.oprtns; cursor; _ } as s) apply = 
    if cursor >= Array.length oprtns then 
        ()
    else
        let _ = s.cursor <- s.cursor + apply oprtns.(cursor) in
        consume s apply
;;

let handle_op vm = (function 
    | INop          -> 1 
    | IPop          -> let _ = pop vm in 1 
    | ILoop x       -> x 
    | IJump y       -> y 
    | IJumpFalse z  -> z 
    | IAdd          -> let _ = binop vm (IAdd) in 1 
    | IMul          -> let _ = binop vm (IMul) in 1 
    | INot          -> let _ = push  vm (Emitter.snot (pop vm)) in 1 
    | ILess         -> let _ = push  vm ((Fun.flip Emitter.sless   ) (pop vm) (pop vm)) in 1 
    | IGreater      -> let _ = push  vm ((Fun.flip Emitter.sgreater) (pop vm) (pop vm)) in 1 
    | IConst  _c    -> let _ = push  vm (get_const _c vm.source) in 1 
    | IGetVar _g    -> let _ = push  vm (get_stack _g vm) in 1 
    | ITrue         -> let _ = push  vm (SBool true)  in 1 
    | IFalse        -> let _ = push  vm (SBool false) in 1 
);;

let eval (pr: vm) = 
    consume pr.source (handle_op pr)
;;

let tstsrc =  {
        spine  = Array.make 3 SNil
    ;   stkidx = 0
    ;   frmptr = 0 
    ;   source =
    {
            oprtns= [| IConst 0; IConst 1; IAdd; IGetVar 0; |] 
        ;   consts= [| 10.; 20. |] |> Array.map (fun x -> SNumber x)
        ;   cursor=0
    }
};;
