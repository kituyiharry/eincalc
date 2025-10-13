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
open Ndmodel;;

(*
 *
 * Checked Rules:
 *   -> subscripts with the same letter must have the same dimension size
 *   -> subscripts not appearing in the output will be summed over
 *   -> subcripts in the output cannot be repeated
 *
 *)

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

(* check if ndarray is homogenous - return the dimension shapes *)
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

let compfromshape param pidx einchars dimens  = 
    let l' = List.length einchars in 
    let g' = List.length dimens in
    if  l' == g' then
        let chs = ref CharSet.empty in
        let g' = 
            List.combine einchars dimens 
            |> List.map (fun ((label, index), dimen) ->
                (* -1 is ommision by default *)
                chs := CharSet.add label !chs;
                { label; index; dimen; param=pidx; outlc=(-1); }
            )
        in 
        Ok ({ 
                (* add param *)
                shape=(dimens)(* x by y by .... *)
            ;   elems=(g')    (* each dimension *)
            ;   chset=(!chs)  (* Character set of the labels of each dimension *)
            ;   param
            ;
        })
    (* EDGE case -> scalar will have no shape but we take it as a kind of 1
       length vector. is this a good approach ?? *)
    else if l' = 1 && List.is_empty dimens then
        let chs = ref CharSet.empty in
        let g' = 
            List.combine einchars [1] 
            |> List.map (fun ((label, index), dimen) ->
                (* -1 is ommision by default *)
                chs := CharSet.add label !chs;
                { label; index; dimen; param=pidx; outlc=(-1); }
            )
        in 
        Ok ({ 
                (* add param *)
                shape=(dimens)(* x by y by .... *)
            ;   elems=(g')    (* each dimension *)
            ;   chset=(!chs)  (* Character set of the labels of each dimension *)
            ;   param
            ;
        })
    else
        Error (Format.sprintf "dimension subscript requests %d dimensions but argument %d has %d" l' (pidx+1) g')
;;

let shape_of_mask m map = 
    match m with
    | MinMax (_, _) -> 
        map
    | ZScore -> 
        map
    | Mean ->
        []
    | Mode -> 
        []
    | Stddev ->
        []
    | Reshape shp -> 
        shp
    (*| Determ -> *)
        (*[]*)
    (*| Cumulative -> *)
        (*map*)
;;

(* calculate shape from checking parameter structure *)
let rec calcshape c = 
    match c with 
    | Range (_cellstart, _cellend) -> 
        (* get the grid indexes for the references e.g A10 -> (9, 0) *)
        let (sr, sc) = key_of_ref _cellstart in
        let (er, ec) = key_of_ref _cellend in
        (* extract a shape *)
        (match (Int.abs (er - sr), Int.abs (ec - sc)) with 
        | (0, 0)   -> Ok [ ] 
        | (0, c)   -> Ok [ c + 1 ] 
        | (r, c)   -> Ok [ r + 1; c + 1 ]) 
    | Scalar _cell ->
        Ok []
    | NdArray n ->
        (homogenous (metashape n))
    | Mask (r, m) -> 
        (>>==) (calcshape r) (fun lshp -> 
            List.fold_left (fun acc maskval -> 
                match acc with 
                | Ok a ->
                    let rshp = shape_of_mask maskval a in
                    (match (Types.cardinal_of_shp rshp, Types.cardinal_of_shp a) with
                    | (l, r) when l = r -> 
                        Ok rshp 
                    | (l, _d) when l = 1 -> 
                        Ok rshp
                    | _ -> 
                        Error (Format.sprintf "improperly structured shape transformation: %s to %s!" 
                            (Types.string_of_shape a) (Types.string_of_shape rshp)
                        )
                    )
                | _ -> 
                    acc
            ) (Ok lshp) m
        )
    | Create c -> 
        (match c with
            | Diag  (_vl, shp) -> (
                Ok [shp;shp]
            )   (* matrix with diagonal values *) 
            | Zeros shp -> (
                Ok shp
            )   (* zero init with a shape  *)
            | Ones  shp  -> (
                Ok shp
            )   (* ones init with a shape  *)
            | Fill  (_vl, shp) -> (
                Ok shp
            )   (* fill with a certain value *)
            | Enum  (_vl, _inc, shp) -> (
                Ok shp
            )  (* enumerate from minvalue and increment with a shape *)
            | Rand  (_vl, shp) -> (
                Ok shp
            ) (* random with bound and shape *)
            | Alt (_slc, shp)  -> (
                Ok shp
            )
        )
    | _  -> 
        Error "unimplemented shape calculation!"
;;

let parammatch ({ inp; _ }, par) = 
    if List.length inp ==  List.length par then 
        (* Ensure input are unique - maybe use disjoint set *)
        let ins = (
            List.combine inp par
            |> List.mapi (fun pidx ((Shape l), param) ->
                (* TODO: use Disjoint Set *)
                (>>==) (calcshape param) (compfromshape param pidx l)
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
        ) v.elems); }
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
            let g' = 
                _l
                |> List.map (fun (label, index) ->
                    (* -1 is ommision by default *)
                    (* NB: ensure dimens are updated later *)
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

(* find dimens from  *)
let find_dimen varname (eincomps: eincomp list) = 
    let dim = ref  0 in
    let _ = List.iter (fun x -> 
        if CharSet.mem varname x.chset then 
            let m = List.find (fun c -> Char.equal varname c.label) x.elems in
            dim := m.dimen
        else
            ()
    ) eincomps
    in !dim
;;

let transform (e: formula)  = 
    let _ = Format.print_flush () in 
    match e with 
    | Stmt (Ein _e) ->  (>>==) (correspondence _e) (fun (lin, lout) -> 
        match lout with 
        | Some vout -> 
            let upd = List.map (fun (x) -> { x with dimen = (find_dimen x.label lin) }) vout in
            let eq = List.map (fun x -> x.dimen) upd in
            Ok ({ inps=lin; outs=(Some upd, eq); })
        | None -> 
            Ok ({ inps=lin; outs=(lout, []); })
    )
    | _ -> 
        failwith "Unimplemented transform"
;;

