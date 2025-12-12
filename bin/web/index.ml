(* TODO: eventually remove svelte canvas with preference to raw Canvas manipulation  *)
(*module Html = Js_of_ocaml.Dom_html*)
(*module Dom  = Js_of_ocaml.Dom*)
(*module G    = Graphics_js*)
module Js   = Js_of_ocaml.Js
module Con  = Js_of_ocaml.Console

(*let js_str  = Js.string*)
(*(*let js_num  = Js.number_of_float*)*)
open Draw

let _ =
    let pltstate   = ref None in
    let ntfclbck   = ref None in
    let refcntr    = ref 0.   in
    let plotcb     = (fun (label, bounds, shapes)  -> 
        (match !pltstate with 
        | Some p -> 
            let _ = Draw.draw_on_canvas label bounds p shapes 
            in ()
        | _ -> 
            Con.console##error "Parent node missing more rendering"
        )
    )  in
    let logger    = (fun (msg, lglvl) ->  
        refcntr  := !refcntr +. 1.;
        (match !ntfclbck with 
            | Some cb -> 
                let obj = Js.Unsafe.obj [||] in
                Js.Unsafe.set obj (Js.string "id")  (js_num !refcntr);
                Js.Unsafe.set obj (Js.string "msg") (js_str msg);
                (match lglvl with 
                    | Eincalc.Ndcontroller.Error -> 
                        Js.Unsafe.set obj (Js.string "level") (js_str "error");
                        let _ = Js.Unsafe.fun_call cb [|obj|] in
                        Con.console##error msg;
                    | Eincalc.Ndcontroller.Info -> 
                        Js.Unsafe.set obj (Js.string "level") (js_str "info");
                        let _ = Js.Unsafe.fun_call cb [|obj|] in
                        Con.console##log msg;
          | _ -> 
                        Con.console##log msg;
                )
            | None    -> 
                Con.console##error msg;
        )
    ) in 

    (* FIXME: active sheet should be a bit more explicit in calls as its possible to
       overwrite it silently *) 
    let sheet   = ref (Eincalc.Ndcontroller.create_default_controller (ref "Default") plotcb logger) in
    let buf     = Buffer.create 2048 in

    (* NB: method names cant have underscores!! *)
    (* TODO: use a view interface to manage this object and the controller *)
    (* TODO: implement undo buffer *)
    let _ = Js.export_all (object%js (_self)

        method renderarea node = (
            let plts  = Draw.init node in 
            pltstate := Some plts;
            Js._true
        )

        method notificationcallback (cb: Js.Unsafe.any Js.callback) = (
            ntfclbck := Some cb;
            Js._true
        )

        method create (sheetname: Js.js_string Js.t) = (
            let shstr = Js.to_string sheetname in
            match Eincalc.Ndcontroller.fetch_grid_label !sheet shstr with
            | Some _ -> 
                Js._false
            | _    -> 
                sheet := Eincalc.Ndcontroller.new_sheet !sheet shstr;
                Js._true
        )

        method activate (sheetname: Js.js_string Js.t) = (
            let shstr = Js.to_string sheetname in
            match Eincalc.Ndcontroller.fetch_grid_label !sheet shstr with
            | Some _ -> 
                (!sheet).active := shstr;
                Js._true
            | _    -> 
                Js._false
        )

        method delete (sheetname: Js.js_string Js.t) = (
            let shstr = Js.to_string sheetname in
            let nsh = Eincalc.Ndcontroller.delete_sheet !sheet shstr in
            let _ = (!sheet).active := nsh in
            js_str nsh
        )

        method rename (sheetname: Js.js_string Js.t) (newname: Js.js_string Js.t) = (
            let shstr = Js.to_string sheetname in
            let shnew = Js.to_string newname in
            match Eincalc.Ndcontroller.rename !sheet shstr shnew with
            | Ok _ -> 
                Js._true
            | Error e   -> 
                (!sheet).onlog (e, Eincalc.Ndcontroller.Error);
                Js._false
        )

        method available (_)  = (
            Eincalc.Ndcontroller.available_sheets !sheet
            |> List.map (js_str)
            |> Array.of_list
        )

        method formulaes (_)  = (
            match Eincalc.Ndcontroller.formulaes !sheet with
            | Ok frms -> 
                frms 
                |> List.map (fun x -> 
                    (object%js 
                        val indx = x.Eincalc.Ndcontroller.indx;
                        val text = x.Eincalc.Ndcontroller.text;
                        val inps =
                            (x.Eincalc.Ndcontroller.inps
                                |> Array.map (fun ((sr, sc), (er, ec)) -> 
                                    (object%js 
                                        val startrow = sr
                                        val startcol = sc
                                        val endrow   = er
                                        val endcol   = ec
                                    end)
                                )
                        );
                        val wrts = 
                            (x.Eincalc.Ndcontroller.wrts
                                |> Array.map (fun ((sr, sc), (er, ec)) -> 
                                    (object%js 
                                        val startrow = sr
                                        val startcol = sc
                                        val endrow   = er
                                        val endcol   = ec
                                    end)
                                )
                        );
                        end)
                )
                |> Array.of_list
            | Error e   -> 
                (!sheet).onlog (e, Eincalc.Ndcontroller.Error);
                [||]
        )

        method get row col  = (
            match Eincalc.Ndcontroller.fetch_grid_label !sheet !((!sheet).active) with
            | Some { grid=_g; _ } -> 
                (match Eincalc.Ndmodel.Grid.find_opt _g (row, col) with 
                    | Some Eincalc.Ndmodel.TValue  s -> (js_str s)
                    | Some Eincalc.Ndmodel.TNumber f -> (js_str (Format.sprintf "%.4f" f))
                    | Some Eincalc.Ndmodel.TNat f    -> (js_str (string_of_int f))
                    | Some Eincalc.Ndmodel.TCover (_f, s) -> (js_str s)
                    | None   -> js_str "")
            | None -> 
                let _ = Con.console##error "Missing grid!!!" in
                js_str ""
        )

        (* TODO: use OptDef or Opt for null checks *)
        method gridaddnumber row col (value: Js.number Js.t) = (
            let vstr = Js.to_float value in
            (*let _ = Con.console##log (Format.sprintf "adding %f to %d*)
                    (*%d\n" vstr row col) in*)
            let act = Eincalc.Ndcontroller.fetch_active_grid !sheet in
            (match Eincalc.Ndcontroller.fetch_grid_label !sheet !((!sheet).active) with
                | Some { grid=_g; _ } -> 
                    let cell = (Eincalc.Parser.Write (Eincalc.Ndcontroller.ref_of_key (row, col))) in 
                    let dotc = Eincalc.Ndcontroller.plaindctx () in 
                    let _ = Eincalc.Ndmodel.Grid.add _g (row, col) (TNumber vstr) in 
                    let _ = (
                        Eincalc.Ndcontroller.notify !sheet cell []
                        |> List.map (Eincalc.Ndcontroller.affected !sheet dotc)
                        |> List.concat
                        |> List.sort_uniq (fun (Eincalc.Parser.Stmt x) (Eincalc.Parser.Stmt y) ->
                            Float.compare x.stamp y.stamp
                        )
                        |> List.iter (fun fml -> 
                                (!sheet).onlog ("Found affected formulae!", Eincalc.Ndcontroller.Warn);
                                Eincalc.Repl.handle_transform_formulae !sheet fml
                           ) 
                    ) in
                    let _ = Buffer.clear buf in
                    let _ = Eincalc.Ndcontroller.FormGraphSerializer.to_dot ~dir:true "Affected" dotc.global dotc.prnode dotc.predge 
                        !(act.frmgrph) |> Seq.concat |> Seq.iter (fun s -> 
                            Buffer.add_string buf (s ())
                        ) in 
                    let s = Buffer.contents buf in
                    let _ = !(sheet).onlog (Format.sprintf "%s" s, Eincalc.Ndcontroller.Warn) in
                    ()
                | None ->
                    Con.console##error "cant add number - Missing grid!!!"
            )
        )

        (* TODO: use OptDef or Opt for null checks *)
        method gridaddstring row col (value: Js.js_string Js.t) = (
            let vstr = Js.to_string value in
            (*let _ = Con.console##log (Format.sprintf "adding %s to %d %d*)
                    (*\n" vstr row col)  in*)
            (match Eincalc.Ndcontroller.fetch_grid_label !sheet !(!sheet.active) with
                | Some { grid=_g; _ } -> 
                    Eincalc.Ndmodel.Grid.add _g (row, col) (TValue vstr)
                | _ -> 
                    Con.console##error "cant add value - Missing grid!!!"
            )
        )

        method griderase row col rend cend = (
            Eincalc.Ndcontroller.erase_grid !sheet row rend col cend
        )

        (* TODO: use OptDef or Opt for null checks *)
        method executecode (value: Js.js_string Js.t) = (
            (* TODO: if it starts with `=` we evaluate it *)
            let vstr = Js.to_string value in
            let act = Eincalc.Ndcontroller.fetch_active_grid !sheet in
            match Eincalc.Repl.simple_scan_exp !sheet vstr with  
            | Ok cell ->
                (* transform the ast to capture any reads and writes so we can
                   form dependencies *)
                (match Eincalc.Eval.tosource !sheet cell with
                    | Ok ({ ast=(Eincalc.Parser.Stmt s); _ } as cell) ->
                        let _ = Eincalc.Ndcontroller.dependants !sheet cell.ast in
                        let _ = Eincalc.Repl.handle_eval !sheet cell in
                        let dotc = Eincalc.Ndcontroller.plaindctx () in 
                        (* see if we wrote over other formulaes *)
                        let _ = (
                            List.iter (fun (msk, shp) ->
                                Eincalc.Ndcontroller.notify !sheet msk shp
                                |> List.map (Eincalc.Ndcontroller.affected !sheet dotc)
                                |> List.concat
                                |> List.sort_uniq (fun (Eincalc.Parser.Stmt x) (Eincalc.Parser.Stmt y) ->
                                    Float.compare x.stamp y.stamp
                                )
                                |> List.iter (fun fml -> 
                                    let _ =
                                        Eincalc.Repl.handle_transform_formulae !sheet fml in
                                    (!sheet).onlog ("Found affected formulae!", Eincalc.Ndcontroller.Warn);
                                )
                            ) s.writes
                        ) in
                        let _ = Buffer.clear buf in
                        let _ = Eincalc.Ndcontroller.FormGraphSerializer.to_dot ~dir:true "Affected" dotc.global dotc.prnode dotc.predge 
                            !(act.frmgrph) |> Seq.concat |> Seq.iter (fun s -> 
                                Buffer.add_string buf (s ())
                            ) in 
                        let s = Buffer.contents buf in
                        let _ = !(sheet).onlog (Format.sprintf "%s" s, Eincalc.Ndcontroller.Warn) in
                        Js._true
                    | Error s -> 
                        !(sheet).onlog (s, Eincalc.Ndcontroller.Error);
                        Js._false
                )
            | Error s -> 
                !(sheet).onlog (s, Eincalc.Ndcontroller.Error);
                Js._false
        )

        (* TODO: use OptDef or Opt for null checks *)
        (* TODO: figuring out structure here is very rudimentary - make updates *)
        method paste row col (value: Js.js_string Js.t) = (
            let vstr = Js.to_string value in
            let act = Eincalc.Ndcontroller.fetch_active_grid !sheet in
            let sep = if String.contains vstr '\t' then '\t' else if (not @@ String.contains vstr ',') then ' ' else ',' in
            (match Eincalc.Ndcontroller.paste_values !sheet !(!sheet.active) sep (row, col) vstr with 
                | Ok    (r,c) -> 
                    let cell = (Eincalc.Parser.Write (Eincalc.Ndcontroller.ref_of_key (row, col))) in 
                    let dotc = Eincalc.Ndcontroller.plaindctx () in 
                    Eincalc.Ndcontroller.notify !sheet cell [r;c]
                    |> List.map (Eincalc.Ndcontroller.affected !sheet dotc)
                    |> List.concat
                    |> List.sort_uniq (fun (Eincalc.Parser.Stmt x) (Eincalc.Parser.Stmt y) ->
                        Float.compare x.stamp y.stamp
                    )
                    |> List.iter (fun fml -> 
                        let _ = Eincalc.Repl.handle_transform_formulae !sheet fml in
                        !(sheet).onlog ("Found affected formulae!", Eincalc.Ndcontroller.Warn);
                    );
                    let _ = Buffer.clear buf in
                    let _ = Eincalc.Ndcontroller.FormGraphSerializer.to_dot ~dir:true "Affected" dotc.global dotc.prnode dotc.predge 
                        !(act.frmgrph) |> Seq.concat |> Seq.iter (fun s -> 
                            Buffer.add_string buf (s ())
                        ) in 
                    let s = Buffer.contents buf in
                    let _ = !(sheet).onlog (Format.sprintf "%s" s, Eincalc.Ndcontroller.Warn) in
                    Con.console##info "Pasted values!";
                    Js._true
                | Error e ->
                    Con.console##error (Format.sprintf "paste error - %s!!!" e);
                    Js._false
            )
        )

        (*You can also write javascript within your OCaml code.
                  Note that the versino of javascript supported is not recent               
                  (no let keyword for example).*)
        (*
                 *method typedArray _ =
                 *(
                 *    let init_typed_array = Js.Unsafe.js_expr
                 *        {js|(function() {
                 *            var buf = new Uint8Array(2);
                 *            buf[0] = 1;
                 *            return buf;
                 *            })
                 *        |js}
                 *    in
                 *    let typed_array = Js.Unsafe.fun_call init_typed_array [||] in
                 *    let typed_array = Js_of_ocaml.Typed_array.String.of_uint8Array typed_array in
                 *    String.iter (fun (x:char) -> Format.printf "%d\n" (int_of_char x)) typed_array
                 *\)
                 *)

        end) 
    in ()
(*Format.printf "Hello console from ocaml!";*)
(*Html.window##.onload := Html.handler onload*)

