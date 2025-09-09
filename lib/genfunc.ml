(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Parser;;

type ndshape = 
    | Col of int 
    | Row of int * ndshape list
[@@deriving show];;

(* add shape metadata *)
let metashape (ndarr) = 
    let rec count ndarr =
        match ndarr with 
        | Itemize i -> Col (List.length i)
        | Collect l -> (
            let c, d = (List.fold_left (fun (c, acc) ndarr' -> 
                (c + 1, (count ndarr') :: acc)
            ) (0,[]) l) in 
            Row (c, d)
        )
    in count ndarr
;;

(* check if ndarray is homogenous *)
let homogenous (matshape) =  
    let rec check r shpl = 
        match r with 
        | Col i -> Ok (i :: shpl)
        | Row (c, n) -> (
            match n with 
            | [] -> Ok (c :: 0 :: shpl) 
            (* check inner shapes against the first one - halt at first non-conforming one *)
            | hd :: rest -> (
                (>>==) ((>>==) (check hd []) (fun x -> 
                    (* go through the rest of the rows and ensure they have the
                       same number of columns *)
                    List.fold_left (fun acc g -> 
                        ((>>==) (acc) (fun a ->
                            ((>>==) (check g []) (fun h ->
                                (* maybe i don't need to check the whole list ?? *)
                                if (List.equal (Int.equal) h a) then 
                                    Ok (a) 
                                else
                                    Error "Not homogenous"
                            ))
                        ))
                    ) (Ok (x)) rest
                )) (fun x' -> Ok (c :: (x' @ shpl)))
            )
        )
    in
    check matshape []
;;

let string_of_shape x = 
    List.map (string_of_int) x
    |> String.concat " x "
;;

module CharSet = Set.Make (Char);;

type einmatch = {
        label: char 
    ;   param: int
    ;   index: int 
    ;   dimen: int
    ;   outlc: int
} [@@deriving show];;

type eincomp = {
       shape: int list
    ;  elems: einmatch list
    ;  chset: CharSet.t
};;

let parammatch ((({ inp; _ }, par): formula)) = 
    if List.length inp ==  List.length par then 
        (* Ensure input are unique - maybe use disjoint set *)
        let ins = (
            List.combine inp par
            |> List.mapi (fun pidx (i, p) ->
                (* TODO: use Disjoint Set *)
                match (i, p) with
                | (Shape l, NdArray n) -> 
                    (>>==) (homogenous (metashape n)) (fun g ->
                        let l' = List.length l in 
                        let g' = List.length g in
                        if  l' == g' then
                            let chs = ref CharSet.empty in
                            let g' = 
                                List.combine l g 
                                |> List.map (fun ((label, index), dimen) ->
                                    (* -1 is ommision by default *)
                                    chs := CharSet.add label !chs;
                                    { label; index; dimen; param=pidx; outlc=(-1) }
                                )
                            in 
                            Ok ({ 
                                    shape=(g)
                                ;   elems=(g')
                                ;   chset=(!chs)
                            })
                        else
                            Error (Format.sprintf "dimension subscript requests %d dimensions but argument %d has %d" (pidx+1) l' g')
                    )
                | _ -> Error "only shape with ndarray handled"
            )
        ) in 
        if List.exists (Result.is_error) ins then
            List.filter (Result.is_error) ins 
            |> List.map (Result.get_error)
            |> String.concat ", and " 
            |> Result.error
        else
            Ok (ins |> List.map (Result.get_ok))
    else
        Error "Inputs don't correspond to parameters provided"
;;

(* check for duplicates *)
let dupexist cb onerr lst =
    let rec foil rem =
        match rem with
        | [] -> 
            (* NB: ensure we return the whole list as it is fully valid!! *)
            Ok lst
        | hd::tl ->
            (>>==) (
                (* Does the head exist in its own tail ?? *)
                if List.exists (cb hd) tl then 
                    Error (onerr hd (List.find (cb hd) tl))
                else
                    Ok tl
            ) (foil)
    in foil lst
;;

(* verify stuff about a shape *)
let verify eino comps = 
    (* output shape parameters don't allow for duplicates *)
    (>>==) (dupexist (fun (x,_) (y,_) -> Char.equal x y) (fun (c,_) _ -> (Format.sprintf "Duplicated label %c in output" c)) eino)
    (fun x -> 
        (* output elements must be in input *)
        let allc = List.fold_left (fun a x -> CharSet.union a x.chset) CharSet.empty comps in
        match (List.find_opt (fun (x, _) -> not (CharSet.mem x allc)) x) with
        | Some (h, _) ->  Error (Format.sprintf "Output %c not reflected in input" h)
        | None        ->  Ok x
    )
;;

(* Repeated elements must have the same dimension *)
let repeatcheck (g: eincomp list) = 
    (>>==) (
        List.map (fun x -> x.elems) g
        |> List.concat
        |> dupexist (fun x y -> 
            x.label = y.label && not (Int.equal x.dimen y.dimen)
        ) (fun x y -> Format.sprintf "Repetition of %c with unequal dimensions (%d != %d) for params: %d and %d respectively" x.label x.dimen y.dimen x.param y.param)
    ) (fun _clst -> Ok g)
;;

let connect g l =
    (* connect reflected inputs to outputs = ommited items will be <0 *)
    (* NB: also discard label sets as we do not need them *)
    (List.map (fun (v: eincomp) -> 
        { v with elems=(List.map (fun w -> 
            match List.find_opt (fun (c, _) -> Char.equal w.label c) l with 
            | Some (_, i) ->  { w with outlc=i }
            | None        ->    w
        ) v.elems); chset=CharSet.empty }
    ) g)
    |> repeatcheck
;;

(* Dimension check *)
let correspondence (({out; _}, _) as g) = 
    (>>==) (parammatch g) (fun g' -> 
        match out with
        | None  -> 
            Ok g'
        | Some (Shape _l) ->  
            ((>>==) (verify _l g') ((connect g')))
    )
;;

let describe (lst: (einmatch list)) = 
    let buf = Buffer.create (256) in
    let _ = List.iter (fun ({ label=l; index=i; dimen=s; outlc=o; param=p }) -> 
        Buffer.add_string buf (Format.sprintf "\t* Label:%c at (%d -> %d) with size: %d (Param: %d) \n" l i o s p)
    ) lst in 
    Buffer.contents buf
;;

let debug_print (l) =
    let _ = Format.printf "\t------------------------------\n" in
    let _ = List.iter (fun (l, i) -> 
            let _ = Format.printf "\t| Mat: %s\n\t------------------------------\n\t|\n%s\t|" i (describe l)
            in
            Format.printf "\n\t------------------------------\n" 
    ) l in ()
;;

let transform (_e: formula)  = 
    ()
    (*match (correspond e) with *)
    (*| Ok l    -> debug_print l*)
    (*| Error v -> Format.printf "%s" v*)
;;

