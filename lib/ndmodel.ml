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
open Types;;

type spinmodel = 
    | TNumber   of float  (* = followed by number literal *)
    | TFormulae of string (* = followed by string literal -> should reference other value in the model *)
    | TValue    of string (* all other values *)
[@@deriving show];;

(* eg D1, E100 ... etc. format is column first then row number *)
type gridref = (string * int) 

(* convert a pair like ("DD", 100) -> to referencable 0 indexed cell (100, ) *)
(* TODO: Support greek letters or other alphabet types and numeral systems *)
let key_of_ref ((col, row): gridref) = 
    let col = String.fold_left (fun acc c -> 
        (acc * ((Char.code 'Z' - Char.code 'A') + 1))
            + 
        ((Char.code c - Char.code 'A') + 1)
    ) 0 (String.uppercase_ascii col) in 
    (row - 1, col - 1)
;;

(* this is a cell intersection *)
type gridptr = (gridref * gridref)

(* final row and column ids in that order for a cell *)
type gridkey = (int * int) [@@deriving eq, show];;

(* 
   TODO:
    - https://stackoverflow.com/questions/682438/hash-function-providing-unique-uint-from-an-integer-coordinate-pair
    - https://burtleburtle.net/bob/hash/doobs.html 
*)
module Grid = Hashtbl.Make (struct 
    type t    = gridkey 
    let equal = equal_gridkey
    let hash (x, y) = (Int.hash (((x + y) * (x + y + 1) / 2) + y))
end)

(* insert grid of rows of values *)
let grid_of_rows g (r, c) l = 
    List.iteri (fun ridx il -> 
        let r' = r + ridx in
        List.iteri (fun cidx v -> 
            Grid.add g (r' , c + cidx) v 
        ) il
    ) l
;;

let rand_grid b (r, c) = 
    let g = Grid.create (r * c) in 
    let _ = grid_of_rows g (0, 0) (
        Seq.ints 0 
        |> Seq.take_while (fun i -> i < r) 
        |> Seq.map (fun _m -> 
            Seq.ints 0 
            |> Seq.take_while (fun i' -> i' < c) 
            |> Seq.map (fun _m' -> TNumber (Random.float b))
            |> List.of_seq
        )
        |> List.of_seq
    ) in 
    g
;;


let enum_grid (r, c) = 
    let n = ref 0. in
    let g = Grid.create (r * c) in 
    let _ = grid_of_rows g (0, 0) (
        Seq.ints 0 
        |> Seq.take_while (fun i -> i < r) 
        |> Seq.map (fun _m -> 
            Seq.ints 0 
            |> Seq.take_while (fun i' -> i' < c) 
            |> Seq.map (fun _m' -> 
                let _ = n := !n +. 1. in
                TNumber (!n)
            )
            |> List.of_seq
        )
        |> List.of_seq
    ) in 
    g
;;

let plain_grid size = 
    Grid.create size
;;

let collectrow g range r sparse apply = 
    range
    |> Seq.map (fun c'' -> 
        let _ = Format.printf "grid: %d-%d\n" r, c'' in 
        (match Grid.find_opt g (r, c'') with 
            | Some  v -> 
                (match v with 
                    | TNumber   f -> 
                        ((r, c''), f)
                    | TFormulae _s -> 
                        ((r, c''), sparse (r, c''))
                    | TValue    _s -> 
                        ((r, c''), sparse (r, c''))
                )
            | None -> 
                ((r, c''), sparse (r, c''))
        )
    ) 
    |> Seq.iter (fun ((r, c), v) -> 
        apply (r, c) v
    ) 
;;

(*  
    creates a sequence from values -> inclusive range
    [0, 0] -> [ 0 ]
    [0, 3] -> [ 0, 1, 2, 3 ]
    [3, 0] -> [ 3, 2, 1, 0 ]
*)
let genrange sc ec = 
    (* only works for natural numbers!! *)
    let _ = assert (sc > -1 && ec > -1) in
    if sc <= ec then 
        Seq.ints sc
        |> Seq.take_while (fun i -> i <= ec) 
    else
        Seq.ints ec
        |> Seq.map (fun m -> (sc - m, m))
        |> Seq.take_while (fun (d, _m) -> d != -1)
        |> Seq.map (fst)
;;

(* fetch from 2d grid with a sparse function if it is not available  - the
   second pairs are like displacements from the first *)
(* TODO: assert r < r' and c < c' ?? or leave commutative and reverse if neg ?? *)
let fetch_grid g (r, c) (r', c') sparse = 
    match (r' - r, c' - c) with
    | (0,  0) ->
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        (match Grid.find_opt g (r, c) with 
            | Some v -> 
                (match v with 
                    | TNumber   f -> 
                        let _sdat = Scalar.make [||] f in
                        (SNdim (_scal, _sdat))
                    | _ -> 
                        let _sdat = Scalar.make [||] (sparse (r, c)) in
                        (SNdim (_scal, _sdat))
                )
            | None -> 
                let _sdat = Scalar.make [||] (sparse (r, c)) in
                (SNdim (_scal, _sdat))
        )
    | (0, _len) ->
        let _scal = (module Vector: NDarray with type t = float vector) in 
        let (module Vector) = _scal in
        let _sdat = Vector.make [|(Int.abs _len) + 1|] 0. in
        (* TODO: can we eliminate this counter to allow for out of order computation *)
        let _clmn = ref 0 in
        let _ = collectrow g (genrange c c') r sparse (fun (_r, _ce) v -> 
            let _ = Vector.set _sdat [|!_clmn|] v in 
            incr _clmn
        ) in  
        (SNdim(_scal, _sdat))
    | (_lenr, _lenc) -> 
        let _scal = (module Matrix: NDarray with type t = float matrix) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.make [|(Int.abs _lenr) + 1;(Int.abs _lenc) + 1|] 0. in
        (* TODO: can we eliminate this counter to allow for out of order computation *)
        let _cntr = ref 0 in
        let _ = 
            (genrange r r')
            |> Seq.map (fun r'' -> 
                (* TODO: can we eliminate this counter - maybe recover it from
                   the row and column offsets - somehow?? *)
                let _cntc = ref 0 in
                let _ = collectrow g (genrange c c') r'' sparse (fun (_r, _c) v -> 
                    let _ = Matrix.set _sdat [|(!_cntr);(!_cntc)|] v in 
                    incr _cntc
                ) in 
                incr _cntr
            )
            |> Seq.iter (ignore)
        in 
        (SNdim(_scal, _sdat))
;;
