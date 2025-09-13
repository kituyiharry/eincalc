(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
module type NDArray = sig 
    type value
    type container
    val  make: int array -> value -> container
    val  set:  container -> int array -> value -> unit
    val  get:  container -> int array -> value
    val  iteri: (int array -> value -> unit) -> container -> unit
end

module Scalar(Ord: Set.OrderedType): NDArray with type value := Ord.t = struct 

    type value     = Ord.t
    type container = value ref

    let make (_dims: int array) (v: value) =
        ref v
    ;;

    let set (_cont: container) (_dims: int array) (v: value) = 
        _cont := v
    ;;

    let get (_cont: container) (_dims: int array) = 
        !_cont
    ;;

    let iteri (apply: int array -> value -> unit) _cont =
        apply [|0|] !_cont
    ;;

end

(* n * n *)
module Vector(Ord: Set.OrderedType): NDArray with type value := Ord.t = struct 

    type value     = Ord.t
    type container = value array

    let make (_dims: int array) (v: value) =
        let () = assert (Array.length _dims > 0) in
        Array.make (Array.unsafe_get _dims 0) v
    ;;

    let set (_cont: container) (_dims: int array) (v: value) = 
        let () = assert (Array.length _dims > 0) in
        Array.unsafe_set _cont (Array.unsafe_get _dims 0) v
    ;;

    let get (_cont: container) (_dims: int array) = 
        let () = assert (Array.length _dims > 0) in
        Array.unsafe_get _cont (Array.unsafe_get _dims 0)
    ;;

    let iteri (apply: int array -> value -> unit) _cont =
        Array.iteri (fun i v -> apply [|i|] v) _cont
    ;;

end

(* n * n *)
module Matrix(Ord: Set.OrderedType): NDArray with type value := Ord.t = struct 

    type value     = Ord.t
    type container = value array array

    let make (_dims: int array) (v: value) =
        let () = assert (Array.length _dims > 1) in
        Array.make_matrix (Array.unsafe_get _dims 0) (Array.unsafe_get _dims 1) v
    ;;

    let set (_cont: container) (_dims: int array) (v: value) = 
        let () = assert (Array.length _dims > 1) in
        Array.unsafe_set (Array.unsafe_get _cont (Array.unsafe_get _dims 0)) (Array.unsafe_get _dims 1) v
    ;;

    let get (_cont: container) (_dims: int array) = 
        let () = assert (Array.length _dims > 1) in
        Array.unsafe_get (Array.unsafe_get _cont (Array.unsafe_get _dims 0)) (Array.unsafe_get _dims 1)
    ;;

    let iteri (apply: int array -> value -> unit) _cont =
        Array.iteri (fun  i a -> Array.iteri (fun j v -> apply [|i;j|] v) a) _cont
    ;;

end

(* n * n * n *)
module BatchMatrix(Ord: Set.OrderedType): NDArray with type value := Ord.t = struct 

    type value     = Ord.t
    type container = value array array array

    let make (_dims: int array) (v: value) =
        let () = assert (Array.length _dims > 2) in
        Array.init (Array.unsafe_get _dims 2) (fun _ ->
            Array.make_matrix (Array.unsafe_get _dims 0) (Array.unsafe_get _dims 1) v
        )
    ;;

    let set (_cont: container) (_dims: int array) (v: value) = 
        let () = assert (Array.length _dims > 2) in
        Array.unsafe_set (
            Array.unsafe_get (
                Array.unsafe_get _cont (Array.unsafe_get _dims 0)
            ) (Array.unsafe_get _dims 1)
        ) (Array.unsafe_get _dims 2) v
    ;;

    let get (_cont: container) (_dims: int array) = 
        let () = assert (Array.length _dims > 2) in
        Array.unsafe_get (
            Array.unsafe_get (
                Array.unsafe_get _cont (Array.unsafe_get _dims 2)
            ) (Array.unsafe_get _dims 1)
        ) (Array.unsafe_get _dims 0)
    ;;

    let iteri (apply: int array -> value -> unit) _cont =
        Array.iteri (fun  i a -> 
            Array.iteri (fun j b -> 
                Array.iteri (fun k v -> 
                    apply [|i;j;k|] v
                ) b
            ) a
        ) _cont
    ;;

end

(* only support float dimens at this point *)

module MulDim: NDArray with type value := float = struct 
    open! Bigarray;;

    type value     = float
    type container = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Genarray.t

    (* only upto 16 dimensions! *)
    let make (_dims: int array) (_v: value) =
        let () = assert (Array.length _dims >= 1) in
        Bigarray.Genarray.create Bigarray.Float64 Bigarray.c_layout _dims
    ;;

    let set = Bigarray.Genarray.set
    ;;

    let get = Bigarray.Genarray.get
    ;;

    let iteri (apply: int array -> value -> unit) _cont = 
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
end


module MulIntDim: NDArray with type value := int = struct 
    open! Bigarray;;

    type value     = int
    type container = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Genarray.t

    (* only upto 16 dimensions! *)
    let make (_dims: int array) (_v: value) =
        let () = assert (Array.length _dims >= 1) in
        Bigarray.Genarray.create Bigarray.Int Bigarray.c_layout _dims
    ;;

    let set = Bigarray.Genarray.set
    ;;

    let get = Bigarray.Genarray.get
    ;;

    let iteri (apply: int array -> value -> unit) _cont = 
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
end



