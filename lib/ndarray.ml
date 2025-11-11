(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

type 'a wrap = {  
        cont: 'a
    ;   dims:  int array 
    ;
};;

type 'a vector     = 'a array wrap
type 'a matrix     = 'a array array wrap
type batches       = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array3.t ;;
type bigfloatarray = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t ;;

(* a minimal interface for better code sharing *)
(* TODO: add slice and axis iterators here as well *)
module type Viewable = sig 
    type t
    val  set:     t -> int array -> float -> unit
    val  get:     t -> int array -> float
    val  iteri:   (int array -> float -> unit) -> t -> unit
    val  shape:   t -> int array
end

(* only support float data at this point *)
(*
   TODO: define a tape protocol which describes the order of the elements when
   iterated over. implicit at the moment. can be useful once we allow for out of
   order executions 
*)
module type NDarray = sig 
    include Viewable
    (*type t*)
    val  make:    int array -> float -> t
    (*val  set:     t -> int array -> float -> unit*)
    (*val  get:     t -> int array -> float*)
    (*val  iteri:   (int array -> float -> unit) -> t -> unit*)
    (*val  shape:   t -> int array*)
    val  iteris:  (unit -> unit) -> (int array -> float -> unit) -> (unit -> unit) -> t -> unit
    val  iteriaxis: int -> (unit -> unit) -> (int array -> float -> unit) -> (unit -> unit) -> t -> unit
    val  init:    int array -> (int array -> float) -> t
end

module Scalar: NDarray with type t = float ref = struct 
    
    type t = float ref


    let make (_dims: int array) (v) =
        ref v
    ;;

    let set _cont (_dims) (v) = 
        _cont := v
    ;;

    let get _cont _dims = 
        !_cont
    ;;

    let iteri apply _cont =
        apply [|0|] !_cont
    ;;

    let iteris (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let _ = onbeginslice () in 
        let _ = apply [|0|] !_cont in 
        onendslice ()
    ;;

    let iteriaxis (_axisid) (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let _ = onbeginslice () in 
        let _ = apply [|0|] !_cont in 
        onendslice ()
    ;;

    let shape _c = 
        [|1|]
    ;;

    let init _dims f = 
        ref (f _dims) 
    ;;

end

(* n * n *)

module Vector: NDarray with type t = (float array wrap) = struct 

    type t = float vector

    let make (_dims: int array) (v) =
        let () = assert (Array.length _dims > 0) in
        { cont=(Array.make (Array.unsafe_get _dims 0) v); dims=(_dims) }
    ;;

    let set (_cont: t) (_dims: int array) (v) = 
        let () = assert (Array.length _dims > 0 && Array.for_all2 (<) _dims _cont.dims) in
        Array.unsafe_set (_cont.cont) (Array.unsafe_get _dims 0) v
    ;;

    let get (_cont: t) (_dims: int array) = 
        let () = assert (Array.length _dims > 0 && Array.for_all2 (<) _dims _cont.dims) in
        Array.unsafe_get (_cont.cont) (Array.unsafe_get _dims 0)
    ;;

    let iteri (apply: int array -> float -> unit) _cont =
        Array.iteri (fun i v -> apply [|i|] v) (_cont.cont)
    ;;

    let iteris (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let _ = onbeginslice () in
        let _ = Array.iteri (fun i v -> apply [|i|] v) (_cont.cont) in
        onendslice ()
    ;;

    let iteriaxis (_axisid) (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let _ = onbeginslice () in
        let _ = Array.iteri (fun i v -> apply [|i|] v) (_cont.cont) in
        onendslice ()
    ;;

    let shape _c =
        _c.dims
    ;;

    let init (_dims: int array) (f) =
        let () = assert (Array.length _dims > 0) in
        { cont=(Array.init (Array.unsafe_get _dims 0) (fun i -> f [|i|])); dims=(_dims) }
    ;;

end

(* n * n *)
module Matrix: NDarray with type t = float array array wrap = struct 

    type t = float matrix

    let make (_dims: int array) (v) =
        let () = assert (Array.length _dims > 1) in
        { cont=(Array.make_matrix (Array.unsafe_get _dims 0) (Array.unsafe_get _dims 1) v); 
          dims=(_dims) 
        }
    ;;

    let set (_cont: t) (_dims: int array) (v) = 
        let () = assert (Array.length _dims > 1 && Array.for_all2 (<) _dims _cont.dims) in
        Array.unsafe_set (Array.unsafe_get _cont.cont (Array.unsafe_get _dims 0)) (Array.unsafe_get _dims 1) v
    ;;

    let get (_cont: t) (_dims: int array) = 
        let () = assert (Array.length _dims > 1 && Array.for_all2 (<) _dims _cont.dims) in
        Array.unsafe_get (Array.unsafe_get _cont.cont (Array.unsafe_get _dims 0)) (Array.unsafe_get _dims 1)
    ;;

    let iteri (apply: int array -> float -> unit) _cont =
        Array.iteri (fun  i a -> Array.iteri (fun j v -> apply [|i;j|] v) a) _cont.cont
    ;;

    let iteris (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let _ = onbeginslice () in
        let _ = Array.iteri (fun  i a -> 
            let _ = onbeginslice () in
            let _ = Array.iteri (fun j v -> apply [|i;j|] v) a in 
            onendslice ()
        ) _cont.cont
        in onendslice ()
    ;;

    let shape _c =
        _c.dims
    ;;


    let iteriaxis axis (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let shp   = shape _cont in 
        let axisid = axis in
        let bound  = (Array.get shp axisid) - 1 in
        let start = [|0;0|] in 
        (* increase dimension while ignoring axis. len is number of dimensions,
           dims is the final shape, indx is the current indexing values *)
        let incrindex len indx dims = 
            let brk = ref false in
            for i = (len - 1) downto 0 do
                (if !brk then () else if i = axisid then () else 
                    (if indx.(i) = (dims.(i) - 1) then 
                        indx.(i) <- 0
                    else
                        let _ = indx.(i) <- ((indx.(i)) + 1) in 
                        let _ = brk := true in 
                        ()
                    )
                )
            done
        in
        start.(axisid) <- bound;
        while (not (Array.for_all (fun x -> x = 0) start))  do 
            onbeginslice ();
            start.(axisid) <- 0;
            for _i = 0 to bound do
                let _ = apply start (get _cont start) in
                start.(axisid) <- (start.(axisid) + 1)
            done;
            incrindex 2 start shp;
            start.(axisid) <- 0;
            onendslice ()
        done
    ;;

    let init (_dims: int array) (f) =
        let () = assert (Array.length _dims > 1) in
        { cont=(Array.init_matrix (Array.unsafe_get _dims 0) (Array.unsafe_get _dims 1) (fun x y -> (f [|x;y|]))); 
          dims=(_dims) 
        }
    ;;

end

(* n * n * n *)
module BatchMatrix: NDarray with type t = batches = struct 
    open! Bigarray;;

    type e = float  
    type t = batches

    let make (_dims: int array) (_v: e) =
        let () = assert (Array.length _dims > 2) in
        Bigarray.Array3.create Bigarray.Float64 Bigarray.c_layout (_dims.(0)) (_dims.(1)) (_dims.(2))
    ;;

    let set (_cont: t) (_dims: int array) (v: e) = 
        Array3.set _cont (_dims.(0)) (_dims.(1)) (_dims.(2)) v
    ;;

    let get (_cont: t) (_dims: int array) =  
        Array3.get _cont (_dims.(0)) (_dims.(1)) (_dims.(2))
    ;;

    let shape _cont = 
        [|Array3.dim1 _cont; Array3.dim2 _cont; Array3.dim3 _cont |]
    ;;

    let iteri (apply: int array -> e -> unit) _cont = 
        let ndims = shape _cont in 
        let rec iterate_recursive arr indices dims depth =
            (if depth = Array.length dims then
                (* Base case: we have all indices, access the element *)
                (apply indices @@ get arr indices)
                else
                    (* Recursive case: iterate through current dimension *)
                    for i = 0 to dims.(depth) - 1 do
                        indices.(depth) <- i;
                        iterate_recursive arr indices dims (depth + 1)
                    done
            )
        in let iterate_genarray_recursive arr =
            let indices = Array.make (Array.length ndims) 0 in
            iterate_recursive arr indices ndims 0
        in iterate_genarray_recursive _cont
    ;;

    let iteris (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let ndims = shape _cont in 
        let rec iterate_recursive arr indices dims depth =
            (if depth = Array.length dims then
                (* Base case: we have all indices, access the element *)
                (apply indices @@ get arr indices)
                else
                    (* Recursive case: iterate through current dimension *)
                    let _ = onbeginslice () in    
                    let _ = (for i = 0 to dims.(depth) - 1 do
                        indices.(depth) <- i;
                        iterate_recursive arr indices dims (depth + 1)
                    done) in
                    onendslice ()
            )
        in let iterate_genarray_recursive arr =
            let indices = Array.make (Array.length ndims) 0 in
            let _ = onbeginslice () in    
            let _ = iterate_recursive arr indices ndims 0 in 
            onendslice ()
        in iterate_genarray_recursive _cont
    ;;

    let iteriaxis axisid (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let shp   = shape _cont in 
        let bound  = (Array.get shp axisid) - 1 in
        let start = [|0;0;0|] in 
        (* increase dimension while ignoring axis. len is number of dimensions,
           dims is the final shape, indx is the current indexing values *)
        let incrindex len indx dims = 
            let brk = ref false in
            for i = (len - 1) downto 0 do
                (if !brk then () else if i = axisid then () else 
                    (if indx.(i) = (dims.(i) - 1) then 
                        indx.(i) <- 0
                        else
                            let _ = indx.(i) <- ((indx.(i)) + 1) in 
                            let _ = brk := true in 
                            ()
                    )
                )
            done
        in
        start.(axisid) <- bound;
        while (not (Array.for_all (fun x -> x = 0) start))  do 
            onbeginslice ();
            start.(axisid) <- 0;
            for _i = 0 to bound do
                let _ = apply start (get _cont start) in
                start.(axisid) <- (start.(axisid) + 1)
            done;
            incrindex 3 start shp;
            start.(axisid) <- 0;
            onendslice ()
        done
    ;;
  

    let init (_dims: int array) (f) =
        let () = assert (Array.length _dims > 1) in
        Bigarray.Array3.init Bigarray.Float64 Bigarray.c_layout (_dims.(0)) (_dims.(1)) (_dims.(2))
        (fun x y z -> f [|x;y;z|])
    ;;

end


module MulDim: NDarray with type t = bigfloatarray = struct 
    open! Bigarray;;

    type e = float
    type t = bigfloatarray

    (* only upto 16 dimensions! *)
    let make (_dims: int array) (_v: e) =
        let () = assert (Array.length _dims >= 1) in
        Bigarray.Genarray.create Bigarray.Float64 Bigarray.c_layout _dims
    ;;

    let set = Bigarray.Genarray.set
    ;;

    let get = Bigarray.Genarray.get
    ;;

    let shape _cont = 
       Genarray.dims _cont
    ;;

    let iteri (apply: int array -> e -> unit) _cont = 
        let ndims = Genarray.dims _cont in 
        let rec iterate_recursive arr indices dims depth =
            (if depth = Array.length dims then
                (* Base case: we have all indices, access the element *)
                (apply indices @@ Genarray.get arr indices)
                else
                    (* Recursive case: iterate through current dimension *)
                    for i = 0 to dims.(depth) - 1 do
                        indices.(depth) <- i;
                        iterate_recursive arr indices dims (depth + 1)
                    done
            )
        in let iterate_genarray_recursive arr =
            let indices = Array.make (Array.length ndims) 0 in
            iterate_recursive arr indices ndims 0
        in iterate_genarray_recursive _cont
    ;;

    let iteris (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let ndims = shape _cont in 
        let rec iterate_recursive arr indices dims depth =
            (if depth = Array.length dims then
                (* Base case: we have all indices, access the element *)
                (apply indices @@ get arr indices)
                else
                    (* Recursive case: iterate through current dimension *)
                    let _ = onbeginslice () in    
                    let _ = (for i = 0 to dims.(depth) - 1 do
                        indices.(depth) <- i;
                        iterate_recursive arr indices dims (depth + 1)
                    done) in
                    onendslice ()
            )
        in let iterate_genarray_recursive arr =
            let indices = Array.make (Array.length ndims) 0 in
            let _ = onbeginslice () in    
            let _ = iterate_recursive arr indices ndims 0 in 
            onendslice ()
        in iterate_genarray_recursive _cont
    ;;

    let iteriaxis axisid (onbeginslice: unit -> unit) (apply: int array -> float -> unit) (onendslice: unit -> unit) _cont =
        let shp   = shape _cont in 
        let bound  = (Array.get shp axisid) - 1 in
        let len   = Array.length shp in
        let start = Array.make len 0 in 
        (* increase dimension while ignoring axis. len is number of dimensions,
           dims is the final shape, indx is the current indexing values *)
        let incrindex len indx dims = 
            let brk = ref false in
            for i = (len - 1) downto 0 do
                (if !brk then () else if i = axisid then () else 
                    (if indx.(i) = (dims.(i) - 1) then 
                        indx.(i) <- 0
                        else
                            let _ = indx.(i) <- ((indx.(i)) + 1) in 
                            let _ = brk := true in 
                            ()
                    )
                )
            done
        in
        start.(axisid) <- bound;
        while (not (Array.for_all (fun x -> x = 0) start))  do 
            onbeginslice ();
            start.(axisid) <- 0;
            for _i = 0 to bound do
                let _ = apply start (get _cont start) in
                start.(axisid) <- (start.(axisid) + 1)
            done;
            incrindex len start shp;
            start.(axisid) <- 0;
            onendslice ()
        done
    ;;

    let init (_dims: int array) (f) =
        let () = assert (Array.length _dims > 1) in
        Bigarray.Genarray.init Bigarray.Float64 Bigarray.c_layout _dims (f)
    ;;

end

