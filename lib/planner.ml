(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Genfunc;;

type spinplan = { 
        loopvars: einmatch list 
    ;   summvars: einmatch list
    ;   outshape: int list
    ;   charset : CharSet.t     [@opaque]
} [@@deriving show];;

let emptyspinplan = {
        loopvars = []
    ;   summvars = [] 
    ;   outshape = []
    ;   charset  = CharSet.empty
};;

let create_plan eincomps = 

    let plan = { emptyspinplan with outshape=eincomps.outs } in
    List.fold_left (fun acc v -> 
        List.fold_left (fun acc' (w: einmatch) -> 
            if CharSet.mem w.label acc'.charset then
                acc'
            else
                (if w.outlc < 0 then
                    { acc' with summvars=acc'.summvars @ [ w ]; charset=(CharSet.add w.label acc'.charset) }
                else
                    { acc' with loopvars=acc'.loopvars @ [ w ]; charset=(CharSet.add w.label acc'.charset) }
                )
        ) acc v.elems
    ) plan eincomps.inps 
;;

