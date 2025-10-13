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

let cardinal_of_shp shp = 
    List.fold_left (fun acc v -> acc * v) 1 shp
;;


let cardinal_of_dim shp = 
    Array.fold_left (fun acc v -> acc * v) 1 shp
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

let iterndarray f nda = 
    let rec iternd nest f nda =
        match nda with 
        (* remember -> indices are in reverse so we need to reverse again *)
        | Parser.Itemize a -> List.iteri (fun l c ->  (f (List.rev (l :: nest)) c)) a
        | Parser.Collect c -> List.iteri (fun i c' -> (iternd (i :: nest) f c') ) c
    in 
    iternd [] f nda
;;

let ndarray_of_dim shp =  
    match shp with 
    | [] -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.make [||] 0. in
        (SNdim (_scal, _sdat))
    (* All ones is a scalar! *)
    | shp when (List.for_all (Int.equal 1) shp) -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.make [||] (0.) in
        (SNdim (_scal, _sdat))
    | hd :: [] -> 
        let _scal = (module Vector: NDarray with type t = float vector) in 
        let (module Vector) = _scal in
        let _sdat = Vector.make [|hd|] 0. in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: [] -> 
        let _scal = (module Matrix: NDarray with type t = float matrix) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.make [|hd;hd1|] 0. in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: hd2 :: [] -> 
        let _scal = (module BatchMatrix: NDarray with type t = batches) in 
        let (module BatchMatrix) = _scal in
        let _sdat = BatchMatrix.make [|hd;hd1;hd2|] 0. in
        (SNdim (_scal, _sdat))
    | rem -> 
        let _scal = (module MulDim: NDarray with type t = bigfloatarray) in 
        let (module MulDim) = _scal in
        let _sdat = MulDim.make (Array.of_list rem) 0. in
        (SNdim (_scal, _sdat))
;;

let ndarray_of_dim_init shp f =  
    match shp with 
    | [] -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.init [||] f in
        (SNdim (_scal, _sdat))
    | hd :: [] -> 
        let _scal = (module Vector: NDarray with type t = float vector) in 
        let (module Vector) = _scal in
        let _sdat = Vector.init [|hd|] f in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: [] -> 
        let _scal = (module Matrix: NDarray with type t = float matrix) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.init [|hd;hd1|] f in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: hd2 :: [] -> 
        let _scal = (module BatchMatrix: NDarray with type t = batches) in 
        let (module BatchMatrix) = _scal in
        let _sdat = BatchMatrix.init [|hd;hd1;hd2|] f in
        (SNdim (_scal, _sdat))
    | rem -> 
        let _scal = (module MulDim: NDarray with type t = bigfloatarray) in 
        let (module MulDim) = _scal in
        let _sdat = MulDim.init (Array.of_list rem) f in
        (SNdim (_scal, _sdat))
;;

let incrindex len indx dims = 
    let brk = ref false in
    for i = (len - 1) downto 0 do
        (if !brk then () else 
            (if indx.(i) = (dims.(i) - 1) then 
                indx.(i) <- 0
                else
                    let _ = indx.(i) <- ((indx.(i)) + 1) in 
                    let _ = brk := true in 
                    ()
            )
        )
    done
;;

(* sequence of indexes into the array *)
let indexsequence dimens = 
    let len = Array.length dimens in
    if len = 0 then 
        (* likely a scalar so a single value *)
        Seq.return [||]
    else
        let idx   = Array.make len 0 in
        (* this simplifies our exit condition in the unfold sequence *)
        let count = Array.make len 0 in
        let _     = Array.unsafe_set idx (len - 1) 1 in
        Seq.append (Seq.return (Array.make len 0)) (Seq.unfold (fun b -> 
            if Array.for_all2 (fun x y -> x = (y - 1)) b dimens then 
                let _ = incrindex len count dimens in
                None
            else 
                let _ = incrindex len count dimens in
                let _ = Array.blit count 0 b 0 len in
                Some (b, count)
        ) idx)
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
