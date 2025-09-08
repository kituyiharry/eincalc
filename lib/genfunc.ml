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

let correspondence ((({ inp; _ }, par): formula)) = 
    if List.length inp ==  List.length par then 
        Result.ok @@ (
            List.combine inp par
            |> List.map (fun (i, p) ->
                match (i, p) with
                | (Shape l, NdArray n) -> 
                    (>>==) (homogenous (metashape n)) (fun g ->
                        if List.length l == List.length g then
                            (* label, index, count *)
                            Ok ((List.combine l g, string_of_shape g))
                        else
                            Error "subscript mismatch"
                    )
                | _ -> Error "expected a shape with ndarray"
            )
        )
    else
        Error "Inputs don't correspond to outputs"
;;

let describe (lst: ((char * int) * int) list) = 
    let buf = Buffer.create (256) in
    let _ = List.iter (fun ((l, i), s) -> 
        Buffer.add_string buf (Format.sprintf "\t* Label:%c at %d with size: %d\n" l i s)
    ) lst in 
    Buffer.contents buf
;;

let debug_print (l) =
    let _ = Format.printf "\t------------------------------\n" in
    let _ = List.iter (fun x -> 
        (match x with
            | Ok   (l, i) -> 
                let _ = Format.printf "\t| Mat: %s\n\t------------------------------\n\t|\n%s\t|" i (describe l)
                in
                Format.printf "\n\t------------------------------\n" 
            | Error v -> Format.printf "Error: %s" v
        )
    ) l in ()
;;

let transform (e: formula)  = 
    match (correspondence e) with 
    | Ok l    -> debug_print l
    | Error v -> Format.printf "%s" v
;;

