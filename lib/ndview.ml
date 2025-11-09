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

let (let*) = (Result.bind);;

(* views into an underlying ndarray *)
module type NDView = sig 
    type t
    type base
    type gen

    val  make:    base -> gen -> t
    val  get:     t -> int array -> float
    val  shape:   t -> int array
    val  iteri:   (int array -> float -> unit) -> t -> unit
end

(* TODO: add assertions relating slices with the underlying data *)
module MakeSliceView(N: NDarray)  = struct 

    type base = N.t
    type gen  = Parser.slice list
    type t    = { 
            (*slicedef: Parser.slice list*)
            sliceval: (int * int * int) array
        ;   basedata: N.t
        ;   shape   : int array
    }

    let make data sdef = 
        match Genfunc.shapeslice [] sdef (Array.to_list (N.shape data)) with 
        | Error e -> failwith ("viewslice error! " ^ e)
        | Ok shape ->
            { 
                    (*slicedef=sdef*)
                    sliceval=(Genfunc.slicetoarr (Array.of_list shape) sdef)
                ;   basedata=data
                ;   shape   = Array.of_list shape
            }
    ;;

    let shape { shape; _ } = shape 

    (* INFO: in this case , the index is following the slice and not the underlying
       shape *)
    let get  { shape; sliceval; basedata; _ } (indx: int array)  = 
        let _ = assert (Array.length indx  = Array.length shape && Array.for_all2 (<) indx shape) in
        let dmaindim = N.shape basedata in
        let dlen     = Array.length dmaindim in
        let dmainidx = Array.make dlen 0 in
        let _ =  
            (* set the start offset for each dimension *)
            Array.mapi_inplace (fun i _ -> 
                let (start, _, skip) = sliceval.(i) in 
                let off = Types.offset dmainidx i dmaindim start in
                let add = skip * indx.(i) in
                off + add
            ) dmainidx
        in N.get basedata dmainidx
    ;;

    let iteri apply { basedata;sliceval; shape;_ } = 
        let dmaindim = N.shape basedata in
        let dlen     = Array.length dmaindim in
        let slen     = Array.length shape in
        let dmainidx = Array.make dlen 0 in
        let sliceidx = Array.make slen 0 in
        let max      = Array.fold_left (fun x a -> x * a) 1 shape in
        let _ =  
            (* set the start offset for each dimension *)
            Array.mapi_inplace (fun i _ -> 
                let (start, _, _) = sliceval.(i) in 
                Types.offset dmainidx i dmaindim start 
            ) dmainidx
        in
        (for _i = 1 to (max) do 
            (* INFO: note that dmainidx will be for the underlying since the
               slice hasn't been realized. sliceidx will be tracking the slice
               itself*)
            let _ = apply sliceidx (N.get basedata dmainidx) in
            Types.incrindex slen sliceidx shape;
            Types.incrbyselection dlen dmainidx dmaindim sliceval
        done)
    ;;

end 
