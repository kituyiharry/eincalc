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
    ;  chset: CharSet.t      [@opaque]
    ;  param: crange
} [@@deriving show];;

type eintree = {
        inps: eincomp list 
    ;   outs: (einmatch list option * int list) (* *)
} [@@deriving show];;

let parammatch ({ inp; _ }, par) = 
    if List.length inp ==  List.length par then 
        (* Ensure input are unique - maybe use disjoint set *)
        let ins = (
            List.combine inp par
            |> List.mapi (fun pidx (paridx, param) ->
                (* TODO: use Disjoint Set *)
                match (paridx, param) with
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
                                    { label; index; dimen; param=pidx; outlc=(-1); }
                                )
                            in 
                            Ok ({ 
                                    (* add param *)
                                    shape=(g)     (* x by y by .... *)
                                ;   elems=(g')    (* each dimension *)
                                ;   chset=(!chs)  (* Character set of the labels of each dimension *)
                                ;   param
                                ;
                            })
                        else
                            Error (Format.sprintf "dimension subscript requests %d dimensions but argument %d has %d" l' (pidx+1) g')
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

(* check for duplicates via cb and run onxst if duplicate found *)
let dupexist cb onexst lst =
    let rec foil rem =
        match rem with
        | [] -> 
            (* NB: ensure we return the whole list as it is fully valid!! *)
            Ok lst
        | hd::tl ->
            (>>==) (
                (* Does the head exist in its own tail ?? *)
                if List.exists (cb hd) tl then 
                    Error (onexst hd (List.find (cb hd) tl))
                else
                    Ok tl
            ) (foil)
    in foil lst
;;

let dedup cb l = 
    let rec foil rem = 
        (match rem with 
            | []     -> []
            | hd :: tl -> 
                if List.exists (cb hd) tl then
                    (foil tl)
                else
                    hd :: (foil tl)
        )
    in foil l
;;

(* returns the final shape of the einsum operation *)
let equation (x: eincomp list) = 
    List.map (fun w -> w.elems) x
    |> List.concat
    |> List.filter (fun w -> w.outlc > -1)
    |> dedup       (fun x y -> Char.equal x.label y.label)
    |> List.map    (fun w -> w.dimen)
    |> List.rev
;;

(* verify stuff about a shape *)
let verify eino comps = 
    (* output shape parameters don't allow for duplicates *)
    (>>==) (dupexist 
        (fun (x,_) (y,_) -> Char.equal x y) 
        (fun (c,_) _ -> (Format.sprintf "Duplicated label %c in output" c)) 
        eino
    )
    (fun x -> 
        (* output elements must be in input *)
        let allc = List.fold_left (fun a x -> CharSet.union a x.chset) CharSet.empty comps in
        match (List.find_opt (fun (x, _) -> not (CharSet.mem x allc)) x) with
        | Some (h, _) ->  Error (Format.sprintf "Output %c not reflected in input" h)
        | None        ->  Ok x
    )
;;

(* Repeated elements must have the same dimension e.g. in ij,jk, j must have
   same dimen value! *)
let repeatcheck (g: eincomp list) = 
    (>>==) (
        List.map (fun x -> x.elems) g
        |> List.concat
        |> dupexist (fun x y -> 
                let v = x.label = y.label && not (Int.equal x.dimen y.dimen)
                (*in let _ = Format.printf "x: %c of %d = y: %c of %d is %b \n" x.label x.dimen y.label y.dimen v *)
                (*in let _ = Format.print_flush () *)
                in v
            ) 
            (fun x y ->
            (* Sometimes broadcasting or contractions happen here !! *)
            Format.sprintf "Repetition of %c with unequal dimensions (%d != %d) for params: %d and %d respectively"
                x.label x.dimen y.dimen x.param y.param)
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
    (* verify the dimensions *)
    |> repeatcheck
;;

(* Dimension checks *)
let correspondence (({out; _}, _) as g) = 
    (>>==) (parammatch g) (fun g' -> 
        match out with
        | None  -> 
            (* verify the repeated dimensions *)
            ((>>==) (repeatcheck g') (fun g'' -> 
                Ok (g'', None)
            ))
        | Some (Shape _l) -> 
            (* TODO: verify if this also checks within inputs - done in connect function ! *)
            (>>==) ((>>==) (verify _l g') (connect g')) (fun g'' -> 
            let chs = ref CharSet.empty in
            let g' = 
                _l
                |> List.map (fun (label, index) ->
                    (* -1 is ommision by default *)
                    (* NB: ensure dimens are updated later *)
                    chs := CharSet.add label !chs;
                    { label; index; dimen=(-1); param=(-1); outlc=(-1); }
                )
                in Ok (g'', Some g')
            )
    )
;;

let describe (lst: (einmatch list)) = 
    let buf = Buffer.create (256) in
    let _ = List.iter (fun ({ label=l; index=i; dimen=s; outlc=o; param=p; _ }) -> 
        Buffer.add_string buf 
            (Format.sprintf "\t* Label:%c at (%d -> %d) with size: %d (Param: %d)  |\n" l i o s p)
    ) lst in 
    Buffer.contents buf
;;

let debug_print ({ inps; outs=(_,o); _ }) =
    let _ = List.iter (fun l -> 
        let _ = Format.printf "\t Mat: %s  -> %s   
\t+-----------------------------------------------+
\t|                                               |\n%s" (string_of_shape l.shape) (string_of_shape o) (describe l.elems)
        in
        Format.printf "\t|                                               |
\t+-----------------------------------------------+\n" 
    ) inps in ()
;;

let transform (e: formula)  = 
    (*let _ = Format.printf "transforming: %s !!\n" (show_program e) in*)
    match e with 
    | Stmt (Ein _e) ->  (>>==) (correspondence _e) (fun (lin, lout) -> 
        let eq = equation lin in
        match lout with 
        | Some v -> 
            let upd = List.combine v eq |> List.map (fun (x,y) -> { x with dimen = y }) in
            Ok ({ inps=lin; outs=(Some upd, equation lin); })
        | None -> 
            Ok ({ inps=lin; outs=(lout, equation lin); })
    )
    | _ -> 
        failwith "Unimplemented transform"
;;

