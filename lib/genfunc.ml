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
            ) (0,[]) l) 
            in 
            Row (c, d)
        )
    in match ndarr with 
    | NdArray nd -> count nd
    | _ -> failwith "Expected ndarray"
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

let shape m = 
    (>>==) (homogenous m) (fun x -> 
        List.map (string_of_int) x
        |> String.concat " x "
        |> Result.ok
    )
;;

let transform (e: formula)  = 
    let ({ Parser.inp; _ }, args) = e in
    List.to_seq inp 
    |> Seq.zip (List.to_seq args)
    |> Seq.iter (fun (x, y) ->
        match x with 
        | NdArray _ ->  
            let b = (metashape x) in
            let s = (Result.value ~default:"??" (shape b)) in
            Format.printf "\nInp: %s and Arg(%s): %s" (show_lit y) s (show_ndshape b)
        | _ -> 
            Format.printf "\nInp: %s and argument: %s" (show_lit y) (show_crange x)
    )
;;

