(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

open Ndarray;;
(*open Genfunc;;*)

(* stack values *)
type spinval = 
    | SNil
    | SNumber of float
    | SIndex  of int
    | SBool   of bool
    | SStr    of string
    | SKern   of int   (* point to kernel area *)
    | SAddr   of int array
    | SNdim:  ((module NDarray with type t = 'cont) * 'cont) -> spinval 
;; 

let shape_of_module (type n) (module N: NDarray with type t = n) v = 
    N.shape v
;;

let name_of_shape =  function 
    | 0 -> "scalar "
    | 1 -> "vector "
    | 2 -> "matrix "
    | 3 -> "batches " 
    | _ -> "bigarray "
;;

let string_of_shape x = 
    List.map (string_of_int) x
    |> String.concat " x "
    |> (^) (name_of_shape (List.length x))
;;

let string_of_dim x = 
    Array.to_seq x 
    |> List.of_seq 
    |> string_of_shape
;;



let show_spinval s = 
    match s with
    | SNil      -> "nil"
    | SNumber f -> Format.sprintf "num: %.2f" f
    | SIndex  i -> Format.sprintf "idx: %d" i 
    | SBool   b -> Bool.to_string b 
    | SStr    s -> s 
    | SKern   k -> Format.sprintf "kern: %d" k
    | SAddr   a -> Format.sprintf "addr: [ %s ]" (string_of_dim a)
    | SNdim  (d, n) -> string_of_dim @@  (shape_of_module d n)
;;

let pp_spinval _f _s = 
    (Format.fprintf _f "%s" (show_spinval _s))
;;

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | SIndex x',  SIndex  y' -> SIndex  (x' +  y')
    | _ -> failwith (Format.sprintf "Invalid add operands: %s + %s" (show_spinval x) (show_spinval y))
;;

let seql x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.equal x' y')
    | SIndex  x', SIndex  y' -> SBool (Int.equal   x' y')
    | SBool   x', SBool   y' -> SBool (Bool.equal  x' y')
    | _ -> failwith (Format.sprintf "Invalid equality operands: %s = %s" (show_spinval x) (show_spinval y))
;;

let strue x = 
    match x with 
    | SBool y' -> (Bool.equal y' true)
    | _ -> failwith "Expected bool operand!"
;;

let smul x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' *. y')
    | SIndex  x', SIndex  y' -> SIndex  (x' *  y')
    | _ -> failwith "Invalid mul operands"
;;

let snot x = 
    match x with 
    | SBool b -> SBool (not b)
    | _ -> failwith "invalid boolean not operand"
;;

let sless x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (-1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (-1))
    | _ -> failwith (Format.sprintf "Invalid less operands: %s < %s" (show_spinval x) (show_spinval y))
;;

let sgreater x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (1))
    | _ -> failwith (Format.sprintf "Invalid greater operands: %s > %s" (show_spinval x) (show_spinval y))
;;

(* if all values are close *)
let sallclose x y = 
    match (x, y) with 
    | SNdim ((module M), _dat), SNdim ((module M'), _dat') -> 

        let dim  = M.shape  _dat  in 
        let dim' = M'.shape _dat' in

        if not (Array.length dim = Array.length dim') then 
            SBool false 
        else
            (
                if not (Array.for_all2 (Int.equal) dim dim') then 
                    SBool false 
                else
                    (* TODO: take_while so we don't traverse the whole thing ?? *)
                    let issame = ref true in
                    let _ = M.iteri (fun _d v -> 
                        issame := !issame && (Float.equal (M'.get _dat' _d) v)
                    ) _dat in
                    SBool (!issame)
            )
    | _ -> failwith (Format.sprintf "Invalid greater operands: %s > %s" (show_spinval x) (show_spinval y))
;;

type instr = 
    (* arith *)
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    | IPush      of spinval (* TODO: not really needed -> load via IConst directly *)
    (* logic *)
    (*| ITrue*)
    (*| IFalse*)
    | INot               (* boolean inversion *)
    | ILess              (* Lesser than *)
    | IGreater           (* Greater than *)
    (* jumping *)
    | IJump      of int  (* Jump  *)
    | IJumpFalse of int ref  (* Jump if false *)
    | ILoop      of int  (* Jump to a specific location directly ? *)
    (* var ops *)
    | IConst     of int  (* load constant from position int onto the stack *)
    | IGetVar    of int  (* get the variable at a certain displacement from the stack index *)
    | ISetVar    of int  (* set value at certain displacement from stack index *)
    (* effects *)
    | IEcho              (* print value at the top of the stack *)
    | IEchoNl
    | IGetKern           (* load value from a kernel *)
    | ISetKern           (* write value to a kernel *)
    | IEchoKern          (* dereference and print out kernel values *)
    | ILoadAddr  of int  (* create an address from the next n values on the stack *)
[@@deriving show];;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
    ;   kernels: spinval array 
};;
