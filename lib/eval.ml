(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* cartillege for the spine ?? *)
type disc = 
    | Number of float
    | None
;;

type instr = 
    | Nop               (* No operation *)
    | Pop               (* Pop of the stack *)
    | Jump of int       (* Jump  *)
    | JumpFalse of int  (* Jump if false *)
    | Loop of int       (* Jump to a specific location by subtracting? *)
;;

type source = {
        oprtns: instr array
    ;   cursor: int
    ;
};;

type vm = {
        spine:  disc array
    ;   stkidx: int 
    ;   source: source
};;

(* consume instructions and return the number of places to jump *)
let rec consume { oprtns; cursor } apply = 
    if cursor > Array.length oprtns then 
        ()
    else
        consume { oprtns; cursor=(cursor + (apply oprtns.(cursor))) } apply
;;

let eval (pr: vm) = 
    consume pr.source (function 
        | Nop          -> Format.printf "Nop\n"; 1
        | Pop          -> Format.printf "Pop stack!\n"; 1
        | Loop x       -> Format.printf "Loop\n"; x
        | Jump y       -> Format.printf "Jump\n"; y
        | JumpFalse z  -> Format.printf "Jump-if-False\n"; z
    )
;;
