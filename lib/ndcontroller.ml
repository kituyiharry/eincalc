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
open Parser;;
open Fungi;;

(* TODO: make Hashtbls randomized to prevent ddos attacks on web *)
module GridTable = Hashtbl.Make (String);;
module FormGraph = Graph.MakeGraph (struct 
    type edge   = float           (* the stamp from the program *)
    type t      = Parser.program  (* the program *)
    let compare (Parser.Stmt ast1) (Parser.Stmt ast2) = (Float.compare ast1.stamp ast2.stamp)
end);;

type gridmodel = {
        index: int              (* creation number for this grid *)
    ;   grid : spinmodel Grid.t (* actual data belonging to this grid *)
    ;   code : string           (* code for this grid *)
};;

type loglevel = 
    | Debug
    | Info
    | Warn 
    | Error 
;;

type fgraph =  FormGraph.adj FormGraph.NodeMap.t
;;

type gridcontroller = { 
        count:   int                   (* count with new additional sheets *)
    ;   sheets:  gridmodel GridTable.t (* Grids and their order and labels *)
    ;   active:  string
    ;   plotcb:  ((string * int list * Plotter.shape list) -> unit) 
    ;   onlog:   ((string * loglevel) -> unit)
    ;   frmlst:  (program list) ref 
    ;   frmgrph: fgraph ref 
};;

let create_controller () = 
    { 
        count  = 0 
    ;   sheets = GridTable.create 4 
    ;   active = ""
    ;   plotcb = ignore
    ;   onlog  = (fun (b, _) -> Format.printf " %s\n" b)
    ;   frmlst = ref []
    ;   frmgrph= ref FormGraph.empty
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

let create_default_controller label cb logger  = 
    new_sheet ({ 
            count= 0; sheets=GridTable.create 16
        ;   active=label; plotcb=cb; onlog=logger 
        ;   frmlst = ref []
        ;   frmgrph= ref FormGraph.empty
    }) label
;;

(* calculate projected size of any shape on a 2d grid. e.g [2,2] = 2 rows and 2
   columns, [1,2,3] = 2 rows and 3 columns, ....*)
let span_of_shape shp =
    let len = List.length shp in 
    let rec calc ln =
        function 
        | [] -> 
            (0, 0)
        | col :: [] -> 
            (0, col)
        | row :: col :: [] ->  
            (row, col)
        | batch :: row :: col :: [] -> 
            (* project extra dimensions along the row and account for gaps from
               slice iteration 
               ln - 2 gives the number of gaps between slices on a row *)
            (batch * row + ((batch) - (ln - 2)), col)
        | mult :: rem -> 
            let (row, col) = calc (ln - 1) rem in 
            (* restore the extra gap from the previous frame *)
            ((mult * row) + ((mult - 1) * (ln - 2)), col)
    in calc len shp
;;

let get_column_label col_num =
    let label  = Buffer.create 3 in
    let colnum = ref (col_num + 1) in
    let _ = while !(colnum) > 0 do
        colnum := !colnum - 1;
        Buffer.add_char label (Char.chr ((!colnum mod 26) + 65));
        (* TODO: investigate for possible bugs!! *)
        (*colnum := int_of_float (Float.floor (float_of_int (!colnum) /. 26.));*)
        colnum := !colnum / 26;
    done in
    Buffer.contents label
;;

(* convert a pair like ("DD", 100) -> to referencable 0 indexed cell (100, ) *)
let ref_of_key (row, col) = 
    (get_column_label col, (row + 1))
;;

(* check if a write writes into a given region *)
(* WARN: for now we only consider 2d shapes for writes but have generalized over
   multiple dimensions by projecting along the row. see span_of_shape to figure
   out how this would work *)
let overlaps wrt shp reg = match (wrt, reg)  with  
    |  Write w, Parser.Range (startc, endc) ->
        let (rsp,   csp) = span_of_shape shp in
        (* write top and bottom rows and columns *)

        let (wsr,   wsc) = key_of_ref w in 
        let (wer,   wec) = (wsr + rsp, wsc + csp) in 

        (* read top row and column - check if this section has been modified *)
        let (rsr,   rsc) = key_of_ref startc in
        let (rer, rendc) = key_of_ref endc in

        (* /rectangle-intersection/ *)
        ((wsc <= rendc) && (wec >= rsc) && wsr <= rer && wer >= rsr)

    |  Write w, Parser.Span (startc, shc) ->
        let (rsp,   csp) = span_of_shape shp in
        let (rs',   cs') = span_of_shape shc in

        let (wsr,   wsc) = key_of_ref w in (* top left *)
        let (wer,   wec) = (wsr + rsp, wsc + csp) in 

        let (rsr,   rsc) = key_of_ref startc in
        let (rer, rendc) = (rsr + rs', rsc + cs') in (* bottom right *)

        (* /rectangle-intersection/ *)
        ((wsc <= rendc) && (wec >= rsc) && wsr <= rer && wer >= rsr)

    |  Write w, Parser.Scalar (startc) ->
        let (rsp,   csp) = span_of_shape shp in

        let (wsr,   wsc) = key_of_ref w in (* top left *)
        let (wer,   wec) = (wsr + rsp, wsc + csp) in 

        let (rsr,   rsc) = key_of_ref startc in
        let (rer, rendc) = (rsr, rsc) in (* bottom right *)

        (* /rectangle-intersection/ *)
        ((wsc <= rendc) && (wec >= rsc) && wsr <= rer && wer >= rsr)

    | _ -> 
        false
;;

let add_link controller fromnode tonode = 
    controller.frmgrph := FormGraph.ensureof fromnode tonode !(controller.frmgrph)
;;

(* build graph on addition of a new function line ast *)
(* WARN: we take care to avoid cycles by making new programs only be referenced
   by existing program. This should ideally make the graph acyclic *)
let dependants controller (Parser.Stmt ast as prog) = 
    match ast.inputs with 
    | [] -> 
        controller.onlog ("No inputs", Warn)
    | _ ->
        controller.onlog ("Some inputs", Warn);
        let _ = controller.frmgrph := (FormGraph.add prog !(controller.frmgrph)) in
        let _ = List.iter (fun (rnge: crange) -> 
            match rnge with
            | Range _ | Span  _ as s -> 
                List.iter (fun (Parser.Stmt v' as prog') -> 
                    (* check if it writes over our input region *)
                    if List.exists (fun (msk, shp) -> overlaps msk shp s) v'.writes then 
                        (* we call this when our input region has been affected *)
                        controller.frmgrph := FormGraph.add_weight (v'.stamp) prog' prog !(controller.frmgrph)
                    else ()
                ) !(controller.frmlst)
            | _ -> ()
        ) ast.inputs in
        controller.onlog ("Updating with new formular", Warn);
        controller.frmlst := (prog :: !(controller.frmlst))
;;

let affected controller start = 
    FormGraph.bfs 
        (fun _stck ctx -> { ctx  with acc=(ctx.elt :: ctx.acc) }) 
        (fun _stck' ctx' -> ctx') !(controller.frmgrph) 
    start []
;;

(* notify when a region is accessed. we just changed a functions input so we
   check whos input has changed and notify it *)
let notify controller (region: mask) (shp: int list) = 
    match region with
    | Write c ->
        List.fold_left (fun acc' (Parser.Stmt v' as prog') -> 
            (*(* check if it writes over our input region *)*)
            (*let _ = controller.onlog (Format.sprintf "Looking for overlap on %s: %d!" (Parser.show_cell c) (List.length v'.inputs), Warn) in*)
            if List.exists (overlaps region shp) v'.inputs then 
                (*let _ = controller.onlog ("Found overlap with fx!", Info) in*)
                (*(* we call this when our input region has been affected *)*)
                prog' :: acc'
            else acc'
        ) [] (List.rev !(controller.frmlst))
    | _ -> []
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

let remove_char buffer char_to_remove original_string =
    let len = String.length original_string in
    for i = 0 to len - 1 do
        let current_char = String.get original_string i in
        if current_char <> char_to_remove then
            Buffer.add_char buffer current_char
    done;
    let word =  Buffer.contents buffer in 
    let _ = Buffer.clear buffer in
    word
;;

let paste_values controller label separator (_row, _col) data = 
    let buffer = Buffer.create 16 in
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
                        (* for numbers with a comma in them  e.g 1,000 *)
                        remove_char buffer ',' word
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
                            Grid.add grid (offset, acc) (TValue word)
                        )
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
