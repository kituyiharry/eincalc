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
    ;   stkidx: int 
    ;   frmptr: int
};;

let get_stack idx { frmptr; spine; _ } = 
    Array.get spine (frmptr + idx)
;;

let reset_vm v =
    v.source.cursor <- 0; 
    { v with stkidx = 0 }
;;

(* consume instructions and return the number of places to jump *)
let rec consume ({ Emitter.oprtns; cursor; _ } as s) apply = 
    if cursor >= Array.length oprtns then 
        ()
    else
        (*let _  = Format.printf "???\n" in*)
        let _ = s.cursor <- s.cursor + apply oprtns.(cursor) in
        consume s apply
;;

let eval (pr: vm) = 
    consume pr.source (function 
        | INop          -> Format.printf "Nop\n"; 1
        | IPop          -> Format.printf "Pop stack!\n"; 1
        | ILoop x       -> Format.printf "Loop\n"; x
        | IJump y       -> Format.printf "Jump\n"; y
        | IJumpFalse z  -> Format.printf "Jump-if-False\n"; z
        | IAdd          -> Format.printf "Add\n"; 1 
        | IMul          -> Format.printf "Mul\n"; 1 
        | INot          -> Format.printf "Not\n"; 1 
        | ILess         -> Format.printf "Less\n"; 1 
        | IGreater      -> Format.printf "Greater\n"; 1 
        | IConst  _c    -> Format.printf "Const %d -> %s\n" _c  (show_spinval @@ get_const _c pr.source); 1 
        | IGetVar _g    -> Format.printf "GetVar %d -> %s\n" _g (show_spinval @@ get_stack _g pr); 1 
        | ITrue         -> Format.printf "true\n";  1
        | IFalse        -> Format.printf "false\n"; 1
    )
;;

let as_num v = 
    Emitter.SNumber v 
;;

let tstsrc =  {
        spine=  [| 0.; 0.; 0. |] |> Array.map (as_num)
    ;   stkidx= 0
    ;   frmptr= 0 
    ;   source =
    {
            oprtns= [| IGetVar 0; INop; IJumpFalse 1; INop; INop;  |] 
        ;   consts= [| 1.; 2. |] |> Array.map as_num
        ;   cursor=0
    }
};;
