(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* 
 * - Track updated cell locations (track write calls and adds along with shape sizes)
 * - Run event callbacks e.g. at parsing to suggest stuff
 * - Notify view of errors
 * - manage execution instances
 * - maintain history of executed trees
 * - reuse tensors on re-execute once change subscribers are implemented
 * - handle serialization or materialization in the future
 *)
open Ndmodel;;

module GridTable = Hashtbl.Make (String);;

type gridmodel = {
        index: int              (* creation number for this grid *)
    ;   grid : spinmodel Grid.t (* actual data belonging to this grid *)
    ;   code : string           (* code for this grid *)
};;

type gridcontroller = { 
        count: int                    (* count with new additional sheets *)
    ;   sheets: gridmodel GridTable.t (* Grids and their order and labels *)
};;

let create_controller () = 
    { 
        count= 0 
    ;   sheets=GridTable.create 4 
    }
;;

let new_sheet controller label = 
    let _ = GridTable.add controller.sheets label 
        {
            index = controller.count
        ;   grid  = plain_grid 100
        ;   code  = ""
        }
    in
    {
        controller with 
        count = controller.count+1
    }
;;

let fetch_grid controller label = 
    GridTable.find_opt controller.sheets label 
;;
