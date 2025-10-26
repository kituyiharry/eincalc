(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)


(* make a new array with zscore values *)
let sum (type data) (module M: Ndarray.NDarray with type t = data) (d: data) =
    let sum = ref 0. in
    let _ = M.iteri (fun _dim v -> 
        sum := !sum +. v
    ) d in 
    !sum
;;

let sumaxis (type data newdata) axis (module Mnew: Ndarray.NDarray with type t = newdata) (dnew: newdata) (module M: Ndarray.NDarray with type t = data) (d: data) = 

    let sum   = ref 0. in
    let shp   = Mnew.shape dnew in
    let len   = Array.length shp in
    let iseq  = Array.make len 0 in
    M.iteriaxis axis
        (fun _ -> 
            sum   := 0.; 
        )
        (fun _dim v -> 
            sum := !sum +. v 
        ) 
        (fun _ -> 
            Mnew.set dnew iseq !sum;
            Types.incrindex len iseq shp;
        ) d 
;;



(* TODO: support slicing or partitioning along arbitrary axies *)
let mean (type data) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let sum, count = ref 0., ref 0 in
    let _ = M.iteri (fun _dim v -> 
        let _ = sum := !sum +. v in 
        incr count
    ) d in 
    !sum /. (float_of_int !count)
;;

let meanaxis (type data newdata) axis (module Mnew: Ndarray.NDarray with type t = newdata) (dnew: newdata) (module M: Ndarray.NDarray with type t = data) (d: data) = 

    let sum, count, mean = ref 0., ref 0, ref 0. in
    let shp              = Mnew.shape dnew in
    let len              = Array.length shp in
    let iseq             = Array.make len 0 in
    M.iteriaxis axis
        (fun _ -> 
            sum   := 0.; 
            count := 0;
        )
        (fun _dim v -> 
            let _ = sum := !sum +. v in 
            incr count;
        ) 
        (fun _ -> 
            mean := !sum /. (float_of_int !count);
            Mnew.set dnew iseq !mean;
            Types.incrindex len iseq shp;
        ) d 
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

let stddevaxis (type data newdata) axis (module Mnew: Ndarray.NDarray with type t = newdata) (dnew: newdata) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let _ = meanaxis axis (module Mnew) dnew (module M) d in
    let sum, count = ref 0., ref 0 in
    let shp        = Mnew.shape dnew in
    let len        = Array.length shp in
    let iseq       = Array.make len 0 in
    let activemean = ref 0. in
    M.iteriaxis axis
        (fun _ -> 
            sum        := 0.; 
            count      := 0;
            activemean := Mnew.get dnew iseq;
        )
        (fun _dim v -> 
            let _ = sum := !sum +. (Float.pow (v -. !activemean) 2.) in 
            incr count
        ) 
        (fun _ -> 
            let stdv = Float.sqrt (!sum /. (float_of_int !count)) in
            Mnew.set dnew iseq stdv;
            Types.incrindex len iseq shp;
        ) d 
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

(* smallest value and largest value along the axes *)
let minmaxvalueaxis (type data newdata) axis 
        (module Mmin: Ndarray.NDarray with type t = newdata) (dmin: newdata) 
        (module Mmax: Ndarray.NDarray with type t = newdata) (dmax: newdata) 
        (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let mnmx = ref None in
    let shp        = Mmin.shape dmin in
    let len        = Array.length shp in
    let iseq       = Array.make len 0 in
    M.iteriaxis axis 
        (fun _ -> 
            mnmx := None;
        )
        (fun _dim v -> 
            (* TODO: maybe we don't need to set it on every call ? *)
            match !mnmx with 
            | Some (mn, mx) -> mnmx := (Some (Float.min v mn, Float.max v mx))
            | None -> mnmx := Some (v, v)
        ) 
        (fun _ -> 
            let (mn, mx) = Option.get !mnmx in
            Mmin.set dmin iseq mn;
            Mmax.set dmax iseq mx;
            Types.incrindex len iseq shp;
        ) d 
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

(* TODO: mode does not behave well when value does not include a real mode *)
let modeaxis (type data newdata) axis (module Mnew: Ndarray.NDarray with type t = newdata) (dnew: newdata) (module M: Ndarray.NDarray with type t = data) (d: data) = 
    let tbl = ModeTable.create 16 in
    let shp  = Mnew.shape dnew in
    let len  = Array.length shp in
    let iseq = Array.make len 0 in
    M.iteriaxis axis (fun _ -> 
        ModeTable.clear tbl
    )
    (fun _dim v -> 
        match ModeTable.find_opt tbl v with 
        | None   -> ModeTable.add tbl v 1
        | Some c -> ModeTable.replace tbl v (c + 1)
    ) 
    (fun _ -> 
        let mode = fst @@ Option.get @@ ModeTable.fold (fun k v a -> 
            match a  with 
                | Some (k', v') -> 
                    if v > v' then Some (k, v) else Some (k', v')
                | None -> 
                    Some (k, v)  
        ) tbl None in
        Mnew.set dnew iseq mode;
        Types.incrindex len iseq shp;
    ) d
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
let cumsum (type data) (module M: Ndarray.NDarray with type t = data) (d: data) =
    let sum = ref 0. in
    M.iteri (fun dim v -> 
        sum := !sum +. v;
        M.set d dim (!sum)
    ) d
;;

let cumsumaxis (type data newdata) axis (module M: Ndarray.NDarray with type t = data) (d: data)  =
    let sum = ref 0. in
    M.iteriaxis axis 
        (ignore) 
        (fun dim v -> 
            sum := !sum +. v;
            M.set d dim (!sum)
        )
    (fun _ -> sum := 0.) d
;;

(* make a new array with zscore values *)
let zscore (type data) (module M: Ndarray.NDarray with type t = data) (d: data) =
    let (mnv, std, _) = tendencies (module M) d in
    let d' = M.make (M.shape d) 0. in
    let _ = M.iteri (fun dim v -> M.set d' dim ((v -. mnv) /. (std))) d in 
    d'
;;

let zscoreaxis (type data newdata) axis 
    (module Meanbuf: Ndarray.NDarray with type t = newdata) (dmin: newdata) 
    (module Stdvbuf: Ndarray.NDarray with type t = newdata) (dstd: newdata) 
    (module M: Ndarray.NDarray with type t = data) (d: data) 
=
    let _  = meanaxis axis 
            (module Meanbuf) dmin
            (module M) d
    in

    let _  = stddevaxis axis 
            (module Stdvbuf) dstd
            (module M) d
    in

    let mnbuf, stdbuf = ref 0., ref 0. in

    (* both mnbuf and stdbuf can be addressed in the same manner so this works
       for both *)
    let shp        = Stdvbuf.shape  dstd  in
    let len        = Array.length   shp   in
    let iseq       = Array.make     len 0 in

    M.iteriaxis axis
        (fun _ -> 
            mnbuf  := Meanbuf.get dmin iseq;
            stdbuf := Stdvbuf.get dstd iseq;
        )
        (fun dim v -> M.set d dim ((v -. !mnbuf) /. (!stdbuf)))
        (fun _ -> 
            Types.incrindex len iseq shp;
        )
    d 
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

let minmaxscaleaxis (type data newdata) axis 
    (* buffers to hold some intermediary results *)
    (module Mnbuf: Ndarray.NDarray with type t = newdata) (dmin: newdata) 
    (module Mxbuf: Ndarray.NDarray with type t = newdata) (dmax: newdata) 
    (module M: Ndarray.NDarray with type t = data) (d: data) (a, b) 
=

    (* find min and max value along axes and buffer them *)
    let _ = minmaxvalueaxis axis 
        (module Mnbuf) dmin 
        (module Mxbuf) dmax 
        (module M) d 
    in

    let shp        = Mnbuf.shape  dmin  in
    let len        = Array.length shp   in
    let iseq       = Array.make   len 0 in

    let rdiff  = b  -. a  in 

    let mx, mn = ref 0., ref 0. in
    let mdiff  = ref (!mx -. !mn) in

    M.iteriaxis axis 
    (fun _ -> 
        mn := Mnbuf.get dmin iseq;
        mx := Mxbuf.get dmax iseq;
        mdiff :=  (!mx -. !mn);
    )
    (fun dim v -> 
        M.set d dim (a +. (((v -. !mn) *. rdiff) /. !mdiff ))
    ) 
    (fun _ ->
        Types.incrindex len iseq shp
    ) d 
;;

(* TODO: reshape of Genarray and Bigarray structures can be more efficient *)
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

let writeaxis (type data) axis (module S: Ndarray.NDarray with type t = data) (row, col) (data: data) (grid: Ndmodel.spinmodel Ndmodel.Grid.t) =
    let offsetrow, offsetcol = ref row, ref col in
    S.iteriaxis axis (ignore) 
        (fun _dim value -> 
            let _ = Ndmodel.Grid.add grid (!offsetrow, !offsetcol) (Ndmodel.TNumber value) in
            incr offsetcol
        ) (fun _ -> 
            incr offsetrow;
            offsetcol := col;
        ) data
;;


