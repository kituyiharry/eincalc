(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* TODO: support axis in the future - maybe add a 'sub' function like substring
   in ndarray to select values between slice indices generically *)
let mean (type data) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let sum, count = ref 0., ref 0 in
    let _ = M.iteri (fun _dim v -> 
        let _ = sum := !sum +. v in 
        incr count
    ) d in 
    !sum /. (float_of_int !count)
;;

(* population standard deviation *)
let stddev (type data) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let mval = mean (module M) d in
    let sum, count = ref 0., ref 0 in
    let _ = M.iteri (fun _dim v -> 
        let _ = sum := !sum +. (Float.pow (v -. mval) 2.) in 
        incr count
    ) d in 
    Float.sqrt (!sum /. (float_of_int !count))
;;

(* smallest value *)
let minmaxvalue (type data) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let mnmx = ref None in
    let _ = M.iteri (fun _dim v -> 
        (* TODO: maybe we don't need to set it on every call ? *)
        match !mnmx with 
        | Some (mn, mx) -> mnmx := (Some (Float.min v mn, Float.max v mx))
        | None -> mnmx := Some (v, v)
    ) d in 
    Option.get !mnmx
;;

module ModeTable = Hashtbl.Make (Float) ;;
(* mode value or most frequent *)
let frequencies (type data) (module M: Ndarray.NDarray with type t = data) (tbl) (d: data) = 
   let _ = M.iteri (fun _dim v -> 
        match ModeTable.find_opt tbl v with 
        | None   -> ModeTable.add tbl v 1
        | Some c -> ModeTable.replace tbl v (c + 1)
    ) d in 
    tbl
;;

(* return modal value and its frequency *)
let mode tbl = 
    fst @@ Option.get @@ ModeTable.fold (fun k v a -> 
        match a  with 
        | Some (k', v') -> 
            if v > v' then Some (k, v) else Some (k', v')
        | None -> 
            Some (k, v)
    ) tbl None
;;

(* measures of central tendencies *)
let tendencies (type data) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let mval = mean (module M) d in
    let sum, count = ref 0., ref 0 in
    let tbl = ModeTable.create (Types.cardinal_of_dim (M.shape d))  in
    let _ = M.iteri (fun _dim v -> 
        let _ = sum := !sum +. (Float.pow (v -. mval) 2.) in 
        let _ =match ModeTable.find_opt tbl v with 
        | None   -> ModeTable.add tbl v 1
        | Some c -> ModeTable.replace tbl v (c + 1)
        in incr count
    ) d in 
    let stddev = Float.sqrt (!sum /. (float_of_int !count)) in 
    let modval = mode tbl in
    (mval, stddev, modval)
;;

(* make a new array with zscore values *)
let zscore (type data) (module M: Ndarray.NDarray with type t = data) (d: data) =
    let (mnv, std, _) = tendencies (module M) d in
    let d' = M.make (M.shape d) 0. in
    let _ = M.iteri (fun dim v -> M.set d' dim ((v -. mnv) /. (std))) d in 
    d'
;;

let minmaxscale (type data) (module M: Ndarray.NDarray with type t = data) (d: data) (a, b) =
    let mn, mx = minmaxvalue (module M) d in
    let rdiff  = b  -. a   in 
    let mdiff  = mx -. mn in
    let d' = M.make (M.shape d) 0. in
    let _ = M.iteri (fun dim v -> 
        M.set d' dim (a +. (((v -. mn) *. rdiff) /. mdiff ))
    ) d 
    in d'
;;

let reshape (type adata bdata) (module S1: Ndarray.NDarray with type t = adata) (module S2: Ndarray.NDarray with type t = bdata) (a: adata) (b: bdata) = 
    let (ashape, bshape) = (S1.shape a, S2.shape b) in 
    Seq.zip (Types.indexsequence ashape) (Types.indexsequence bshape)
    |> Seq.iter (fun (aidx, bidx) -> S2.set b bidx (S1.get a aidx))
;; 

let write (type data) (module S: Ndarray.NDarray with type t = data) (row, col) (data: data) (grid: Ndmodel.spinmodel Ndmodel.Grid.t) =
    let offsetrow, offsetcol = ref row, ref col in
    S.iteris (ignore) (fun _dim value -> 
        let _ = Ndmodel.Grid.add grid (!offsetrow, !offsetcol) (Ndmodel.TNumber value) in
        incr offsetcol
    ) (fun _ -> 
            incr offsetrow;
            offsetcol := col;
        ) data
;;

