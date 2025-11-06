(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
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


let rec show_spinval s = 
    match s with
    | SNil      -> "nil"
    | SNumber f -> Format.sprintf "num: %.2f" f
    | SIndex  i -> Format.sprintf "idx: %d" i 
    | SBool   b -> Bool.to_string b 
    | SStr    s -> s 
    | SKern   k -> Format.sprintf "kern: %d" k
    | SAddr   a -> Format.sprintf "addr: [ %s ]" (string_of_dim a)
    | SNdim  (d, n) -> (string_of_dim (shape_of_module d n))
and show_kernel sk = 
    (match sk with 
        | SNdim ((module M), _modl) as _g -> 
            (
                let n = show_spinval (sk) in 
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
;;

let pp_spinval _f _s = 
    (Format.fprintf _f "%s" (show_spinval _s))
;;

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | SIndex x',  SIndex  y' -> SIndex  (x' +  y')
    (* Broadcast operations *)
    | SIndex  x', SNdim ((module Y), d) -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v +. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SIndex  x' -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v +. fx')) d in 
        SNdim ((module Y), d)
    | SNumber  fx', SNdim ((module Y), d) -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v +. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SNumber fx' -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v +. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module X), x), SNdim ((module Y), y) -> 
        let z = Y.make (Y.shape y) 0. in
        let _ = X.iteri (fun _dim v -> 
            let v' = Y.get y _dim in 
            Y.set z _dim (v +. v')
        ) x in 
        SNdim ((module Y), z)
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
    (* Broadcast operations *)
    | SIndex  x', SNdim ((module Y), d) -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v *. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SIndex  x' -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v *. fx')) d in 
        SNdim ((module Y), d)
    | SNumber  fx', SNdim ((module Y), d) -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v *. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SNumber fx' -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v *. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module X), x), SNdim ((module Y), y) -> 
        let z = Y.make (Y.shape y) 0. in
        let _ = X.iteri (fun _dim v -> 
            let v' = Y.get y _dim in 
            let _ = Format.printf "setting %f\n" (v *. v) in
            Y.set z _dim (v *. v')
        ) x in 
        SNdim ((module Y), z)
    | _ -> failwith "Invalid mul operands"
;;


let sdiv x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' /. y')
    | SIndex  x', SIndex  y' -> SIndex  (x' /  y')
    (* Broadcast operations *)
    | SIndex  x', SNdim ((module Y), d) -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v /. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SIndex  x' -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v /. fx')) d in 
        SNdim ((module Y), d)
    | SNumber  fx', SNdim ((module Y), d) -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v /. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SNumber fx' -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v /. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module X), x), SNdim ((module Y), y) -> 
        let z = Y.make (Y.shape y) 0. in
        let _ = X.iteri (fun _dim v -> 
            let v' = Y.get y _dim in 
            Y.set z _dim (v /. v')
        ) x in 
        SNdim ((module Y), z)
    | _ -> failwith "Invalid div operands"
;;

let ssub x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' -. y')
    | SIndex  x', SIndex  y' -> SIndex  (x' -  y')
    (* Broadcast operations *)
    | SIndex  x', SNdim ((module Y), d) -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v -. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SIndex  x' -> 
        let fx' = float_of_int x' in
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v -. fx')) d in 
        SNdim ((module Y), d)
    | SNumber  fx', SNdim ((module Y), d) -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v -. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module Y), d), SNumber fx' -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v -. fx')) d in 
        SNdim ((module Y), d)
    | SNdim ((module X), x), SNdim ((module Y), y) -> 
        let z = Y.make (Y.shape y) 0. in
        let _ = X.iteri (fun _dim v -> 
            let v' = Y.get y _dim in 
            Y.set z _dim (v -. v')
        ) x in 
        SNdim ((module Y), z)
    | _ -> failwith "Invalid sub operands"
;;

let sneg x = 
    match x with 
    | SNumber x' -> SNumber (-. x')
    | SIndex  x' -> SIndex  (-  x')
    (* Broadcast operations *)
    | SNdim ((module Y), d) -> 
        let _ = Y.iteri (fun _dim v -> Y.set d _dim (v *. -1.)) d in 
        SNdim ((module Y), d)
    | _ -> failwith "Invalid sub operands"
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
    | _ -> failwith (Format.sprintf "Invalid operands: %s > %s" (show_spinval x) (show_spinval y))
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

let ndarray_of_dimshape shp =  
    match shp with 
    | [||] -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.make [||] 0. in
        (SNdim (_scal, _sdat))
    (* All ones is a scalar! *)
    | shp when (Array.for_all (Int.equal 1) shp) -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.make [||] (0.) in
        (SNdim (_scal, _sdat))
    | [|_hd|] -> 
        let _scal = (module Vector: NDarray with type t = float vector) in 
        let (module Vector) = _scal in
        let _sdat = Vector.make shp 0. in
        (SNdim (_scal, _sdat))
    | [|_hd;_hd1;|] -> 
        let _scal = (module Matrix: NDarray with type t = float matrix) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.make shp 0. in
        (SNdim (_scal, _sdat))
    | [|_hd;_hd1;_hd2|] -> 
        let _scal = (module BatchMatrix: NDarray with type t = batches) in 
        let (module BatchMatrix) = _scal in
        let _sdat = BatchMatrix.make shp 0. in
        (SNdim (_scal, _sdat))
    | rem -> 
        let _scal = (module MulDim: NDarray with type t = bigfloatarray) in 
        let (module MulDim) = _scal in
        let _sdat = MulDim.make rem 0. in
        (SNdim (_scal, _sdat))
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

(* TODO: move to Ndarray *)
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

let decrindex len indx dims = 
    let brk = ref false in
    for i = (len - 1) downto 0 do
        (if !brk then () else 
            (if indx.(i) = 0 then 
                indx.(i) <- (dims.(i) - 1)
                else
                    let _ = indx.(i) <- ((indx.(i)) - 1) in 
                    let _ = brk := true in 
                    ()
            )
        )
    done
;;

(* direction based offset with negative values flipping the direction to offset
   while maintaing bounds *)
(* TODO: lastoffset dual for this which gets the final index along a dimension
   for use in a decreasebyselection variant *)
let offset indx i dims direc = 
    if direc >= 0 then 
        (indx.(i) + direc) mod (dims.(i) - 1)
    else
        (dims.(i) + direc) mod (dims.(i) - 1)
;;

(* TODO: needs testing!! *)
(* TODO: decrease dual for this *)

(* increase but within bounds in selections *)
let incrbyselection len indx dims selec = 
    let brk = ref false in
    for i = (len - 1) downto 0 do
        let (start, count, step) = selec.(i) in 
        (if !brk then () else 
            (if indx.(i) >= (start + count) then 
                indx.(i) <- (offset indx i dims start) 
                else
                    (* check if we went past the bounds in which case we reset *)
                    let n = ((indx.(i)) + step) in
                    if n > (dims.(i) - 1) then 
                        (* simulate a blank slate - otherwise the index gets
                           stuck. we don't break to increase the next dimen *)
                        let _ = indx.(i) <- 0 in
                        let _ = indx.(i) <- (offset indx i dims start) in 
                        ()
                    else 
                        let _ = indx.(i) <- (n) in 
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
    | ISub               (* binary subtract *)
    | IDiv               (* binary divide *)
    | INeg               (* unary negate *)
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
    | IApplyMasks
    | IApplyMaskList of Parser.mask list
    | ILoadFrame
    | ISaveFrame         (* simulates a function call by saving the base pointer allowing the return of a function call to remain on the stack *)
[@@deriving show];;


let pprint_instr il = 
    Array.iter (fun x -> print_endline (show_instr x)) il 
;;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
    ;   kernels: spinval array 
    ;   pmasks:  Parser.mask list
};;
