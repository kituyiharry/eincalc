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
    ;   active: string
    ;   plotcb: ((string * int list * Plotter.shape list) -> unit) 
};;

let create_controller () = 
    { 
        count= 0 
    ;   sheets=GridTable.create 4 
    ;   active=""
    ;   plotcb =ignore
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
        ;   active=label
    }
;;

let add_plot_cb controller cb = 
    { controller with plotcb=cb }
;;

let create_default_controller label cb = 
    new_sheet ({ 
        count= 0 ; sheets=GridTable.create 16; 
        active=""; plotcb=cb  
    }) label
;;

let fetch_grid_label controller label = 
    GridTable.find_opt controller.sheets label 
;;

let fetch_active_grid controller = 
    GridTable.find controller.sheets controller.active
;;

let erase_grid controller row rowend col colend = 
    let grid = (fetch_active_grid controller).grid in
    let colrange = genrange col colend in
    genrange row rowend 
    |> Seq.iter (fun row' -> 
        Seq.iter (fun col' -> 
            Grid.remove grid (row', col')
        ) colrange
    )
;;

let paste_values controller label separator (_row, _col) data = 
    let _ = Format.printf "pasting values to %d,%d !\n" _row _col in

    let buffer = Buffer.create 16 in

    (*  aarrrgh!!! *)
    let remove_char char_to_remove original_string =
        let len = String.length original_string in
        for i = 0 to len - 1 do
            let current_char = String.get original_string i in
            if current_char <> char_to_remove then
                Buffer.add_char buffer current_char
        done;
        let word =  Buffer.contents buffer in 
        let _ = Buffer.clear buffer in
        word
    in

    match fetch_grid_label controller label with 
    | Some { grid; _ } -> 
        data 
        |> String.split_on_char ('\n')
        |> List.map (String.split_on_char (separator))
        |> List.fold_left (fun offset line -> 
            let _ = List.fold_left (fun acc word -> 
                let word' = (
                    if String.ends_with ~suffix:"%" word then 
                        String.sub word 0 (String.length word - 2)
                    else if String.ends_with ~suffix:"°" word then 
                        String.sub word 0 (String.length word - 2)
                    else if String.starts_with ~prefix:"$" word then 
                        String.sub word 1 (String.length word - 1)
                    else 
                        remove_char ',' word
                ) in
                let _ = ( 
                    (match int_of_string_opt word' with 
                    | Some v -> 
                        Grid.add grid (offset, acc) (TNat v)
                    | _ -> 
                        (match Float.of_string_opt word' with 
                        | Some v -> 
                            Grid.add grid (offset, acc) (TNumber v)
                        | None ->
                            Grid.add grid (offset, acc) (TValue word))
                    )
                ) in
                acc + 1
            ) _col line in 
            offset + 1
        ) _row
        |> Result.ok
    | None -> 
        Error ""
;;
