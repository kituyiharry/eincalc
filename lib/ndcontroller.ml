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
module FormSer = struct 
    let string_of_elt = fun (Stmt p) -> Format.sprintf "\"%s\"" (String.escaped @@ p.source);;
    let string_of_wgt = Float.to_string;;
    let wgt_of_string = Float.of_string;;
    let elt_of_string = fun _ -> prattempty.prog
end
module FormGraphSerializer = FormGraph.Serialize (FormSer);;

(* global attributes *)
let gt   = FormGraphSerializer.StyleTbl.create 1;;
(* per edge style attributes *)
let et   = FormGraphSerializer.AttrbTbl.create 1;;
(* per node style attributes *)
let nt   = FormGraphSerializer.AttrbTbl.create 1;;
(* add some attributes *)
FormGraphSerializer.StyleTbl.add gt "rankdir" "TB";;
FormGraphSerializer.StyleTbl.add gt "color" "green";;

(* collect information about formulae for display *)
type formulactx = {
        text: string
    ;   inps: ((int * int) * (int * int)) array (* start and span *)
    ;   wrts: ((int * int) * (int * int)) array (* start and span *)
    ;   indx: int
}

type fgraph =  FormGraph.adj FormGraph.NodeMap.t
;;

type dotctx = { 
        global:  string FormGraphSerializer.StyleTbl.t
    ;   prnode: (string FormGraphSerializer.StyleTbl.t) FormGraphSerializer.AttrbTbl.t
    ;   predge: (string FormGraphSerializer.StyleTbl.t) FormGraphSerializer.AttrbTbl.t
}

type gridmodel = {
        index: int              (* creation number for this grid *)
    ;   grid : spinmodel Grid.t (* actual data belonging to this grid *)
    ;   display: string
    ;   frmlst:  (program list) ref 
    ;   frmgrph: fgraph ref 
};;

type loglevel = 
    | Debug
    | Info
    | Warn 
    | Error 
;;

type gridcontroller = { 
        sheets:  gridmodel GridTable.t (* Grids and their order and labels *)
    ;   active:  string ref
    ;   plotcb:  ((string * int list * Plotter.shape list) -> unit) 
    ;   onlog:   ((string * loglevel) -> unit)
};;

let create_controller () = 
    { 
        sheets = GridTable.create 4 
    ;   active = ref ""
    ;   plotcb = ignore
    ;   onlog  = (fun (b, _) -> Format.printf " %s\n" b)
    }
;;

let new_sheet controller label = 
    let _ = GridTable.add controller.sheets label 
        {
            index = (GridTable.length controller.sheets)
        ;   grid  = plain_grid 100
        ;   display = label
        ;   frmlst = ref []
        ;   frmgrph= ref FormGraph.empty
        }
    in
    {
        controller with active=ref label
    }
;;

let clear_sheet controller label = 
    match GridTable.find_opt controller.sheets label with
    | Some grid -> 
        Ok (Grid.clear grid.grid)
    | None -> 
        Stdlib.Error ("sheet " ^ label ^ " not found!")
;;

let delete_sheet controller label = 
    let _ = GridTable.remove controller.sheets label in 
    GridTable.to_seq_keys controller.sheets 
    |> Seq.take 1
    |> Seq.uncons 
    |> (function 
        | Some x -> 
            fst x
        | None ->
            (* make a new sheet and make it default *)
            let _ = new_sheet controller "Default" in
            "Default"
    )
;;

let rename controller label newlabel = 
    match GridTable.find_opt controller.sheets label with
    | Some grid -> 
        (match GridTable.find_opt controller.sheets newlabel with 
        | Some _ ->
            (Stdlib.Error (newlabel ^ " already exists"))
        | _ -> 
            let _ = GridTable.remove controller.sheets label in
            Ok (GridTable.add controller.sheets newlabel grid)
        )
    | None -> 
        Error ("sheet " ^ label ^ " not found!")
;;

let available_sheets controller = 
    GridTable.to_seq controller.sheets 
    |> Seq.map (fun (k, v) ->  (k, v.index))
    |> List.of_seq 
    |> List.sort (fun (_, x) (_, y) -> Int.compare x y)
    |> List.map (fun (x, _)  -> x)
;;

let add_plot_cb controller cb = 
    { controller with plotcb=cb }
;;

let create_default_controller label cb logger  = 
    new_sheet ({ 
            sheets=GridTable.create 16
        ;   active=label; plotcb=cb; onlog=logger 
    }) !label
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
            (0, col - 1)
        | row :: col :: [] ->  
            (row - 1, col - 1)
        | batch :: row :: col :: [] -> 
            (* project extra dimensions along the row and account for gaps from
               slice iteration 
               ln - 2 gives the number of gaps between slices on a row *)
            ((batch * row + ((batch) - (ln))) + 1, col - 1)
        | mult :: rem -> 
            let (row, col) = calc (ln - 1) rem in 
            (* restore the extra gap from the previous frame *)
            ((mult * row) + ((mult - 1) * (ln)) - 2, col)
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
let dependants contr (Parser.Stmt ast as prog) = 
    let controller = GridTable.find contr.sheets !(contr.active) in
    (* check for similar source *)
    if List.exists (fun (Parser.Stmt x) -> String.equal x.source ast.source) !(controller.frmlst) then 
        contr.onlog ("Already exists", Warn)
    else
        (match ast.inputs with 
            | [] -> 
                (match ast.writes  with
                    | [] -> 
                        ()
                    | _ -> 
                        (* writes can write into the input of another formula *)
                        (* TODO: if a write doesn't write into another input zone we can prune it from the graph *)
                        contr.onlog ("Some writes adding to graph", Warn);
                        controller.frmgrph := (FormGraph.add prog !(controller.frmgrph));
                        controller.frmlst := (prog :: !(controller.frmlst))
                )
            | _ ->
                let _ = controller.frmgrph := (FormGraph.add prog !(controller.frmgrph)) in
                let _ = List.iter (fun (rnge: crange) -> 
                    match rnge with
                    | Range _ | Span  _ | Scalar _ as s -> 
                        List.iter (fun (Parser.Stmt v' as prog') -> 
                            (* check if it writes over our input region *)
                            if 
                            List.exists (fun (msk, shp) -> overlaps msk shp s) v'.writes 
                            then 
                                let _ = contr.onlog ("connected", Warn) in
                                (* we call this when our input region has been affected *)
                                controller.frmgrph := FormGraph.add_weight (v'.stamp) prog' prog !(controller.frmgrph)
                            else ()
                        ) !(controller.frmlst)
                    | _ -> ()
                ) ast.inputs in
                contr.onlog ("Updating with new formulae", Warn);
                controller.frmlst := (prog :: !(controller.frmlst))
        )
;;


let plaindctx () = 
    let _ = FormGraphSerializer.AttrbTbl.clear nt in
    let _ = FormGraphSerializer.AttrbTbl.clear et in
    {
        global = gt; prnode = nt; predge = et
    }
;;

let affected contr dctx start = 

    let controller = GridTable.find contr.sheets !(contr.active) in
    (*let _ = FormGraphSerializer.AttrbTbl.clear nt in*)
    (*let _ = FormGraphSerializer.AttrbTbl.clear et in*)

    (* make start node blue *)

    let aff = FormGraph.dfs 
        (fun _stck ctx   -> 
            (* some local attributes for the digraph rendering for debugging *)
            let _ = 
                (match ctx.prev with 
                | Some (prevprog, _prevadj) -> 

                    (* make node green *)
                    let lt = FormGraphSerializer.StyleTbl.create 1 in
                    let _ = FormGraphSerializer.StyleTbl.add lt "color" "green" in
                    let ckey  = (FormSer.string_of_elt ctx.elt) in
                    let _ = FormGraphSerializer.AttrbTbl.add dctx.prnode ckey lt in

                    let le = FormGraphSerializer.StyleTbl.create 1 in
                    let _ = FormGraphSerializer.StyleTbl.add le "color" "green" in
                    let pstr = FormSer.string_of_elt prevprog in
                    (* in fungi graph - this represents an edge with attributes *)
                    let ekey = pstr ^ "-" ^ ckey in 
                    FormGraphSerializer.AttrbTbl.add dctx.predge ekey le
                | _ -> 
                    (* make start node blue and rect *)
                    let st = FormGraphSerializer.StyleTbl.create 1 in
                    let _  = FormGraphSerializer.StyleTbl.add st "color" "blue" in
                    let _  = FormGraphSerializer.StyleTbl.add st "shape" "rect" in

                    let _  = FormGraphSerializer.AttrbTbl.add dctx.prnode (FormSer.string_of_elt start) st in
                    ()
                )
            in 
            { ctx  with acc=(ctx.elt :: ctx.acc) }
        ) 
        (fun _stck' ctx' -> ctx') !(controller.frmgrph) 
    start [] in 

    List.rev aff
;;

(* notify when a region is accessed. we just changed a functions input so we
   check whos input has changed and notify it *)
let notify contr (region: mask) (shp: int list) = 
    let controller = GridTable.find contr.sheets !(contr.active) in
    match region with
    | Write _c ->
        List.fold_left (fun acc' (Parser.Stmt v' as prog') -> 
            (*(* check if it writes over our input region *)*)
            (*let _ = controller.onlog (Format.sprintf "Looking for overlap on %s: %d!" (Parser.show_cell c) (List.length v'.inputs), Warn) in*)
            if List.exists (overlaps region shp) v'.inputs then 
                (*let _ = controller.onlog ("Found overlap with fx!", Info) in*)
                (*(* we call this when our input region has been affected *)*)
                prog' :: acc'
            else 
                acc'
        ) [] (List.rev !(controller.frmlst))
    | _ -> []
;;

let formulaes controller  = 
    (match GridTable.find_opt controller.sheets !(controller.active) with 
    | Some v -> 
        !(v.frmlst)
        |> List.mapi (fun indx (Parser.Stmt r) -> 
            {
                    text = r.source
                ;   inps = (
                        r.inputs
                        |> List.map (function 
                            |  (Range (r, e)) -> 
                                ((key_of_ref r), (key_of_ref e))
                            |  (Span  (s, r)) ->
                                let (r', e') = key_of_ref s in
                                let (cr, ce) = span_of_shape r in
                                ((r', e'), (r' + cr, e' + ce))
                            |  (Scalar s) -> 
                                let s'       = key_of_ref s in
                                (s', s')
                            | _  ->
                                ((0, 0), (0, 0))
                        ) |> Array.of_list
                    )
                ;   wrts = (
                        r.writes
                        |> List.map (function 
                            | (Write w, shp) -> 
                                let (s,  e)  = key_of_ref w in
                                let (cs, ce) = span_of_shape shp in
                                ((s, e), (s+cs, e+ce))
                            | _ -> 
                               ((0, 0), (0, 0))
                        ) |> Array.of_list
                    )
                ;   indx
            }
        )
        |> Result.ok
    | None -> 
        Stdlib.Error "Fatal!: Missing grid")

;;

let fetch_grid_label controller label = 
    GridTable.find_opt controller.sheets label 
;;

let fetch_active_grid controller = 
    GridTable.find controller.sheets !(controller.active)
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
    let rc, cc = (ref (-1), ref (-1)) in
    match fetch_grid_label controller label with 
    | Some { grid; _ } -> 
        data 
        |> String.split_on_char ('\n')
        |> List.map (String.split_on_char (separator))
        |> List.fold_left (fun offset line -> 
            let _ = incr rc in
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
                    let _ = incr cc in
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
        |> (function _n -> 
            (* NOTE: may be funky..needs testing *)
            if !rc > 0 then
                Ok (!rc, !cc / !rc)
            else
                Ok (!rc, 0))
    | None -> 
        Error ""
;;
