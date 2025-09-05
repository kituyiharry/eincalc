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

let transform (e: formula)  = 
    let ({ Parser.inp; _ }, args) = e in
    List.to_seq inp 
    |> Seq.zip (List.to_seq args)
    |> Seq.iter (fun (x, y) ->
        Format.printf "Inp: %s and argument: %s" (show_lit y) (show_crange x)
    )
;;
