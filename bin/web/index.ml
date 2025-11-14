(* TODO: eventually remove svelte canvas with preference to raw Canvas manipulation  *)
(*module Html = Js_of_ocaml.Dom_html*)
(*module Dom  = Js_of_ocaml.Dom*)
(*module G    = Graphics_js*)
module Js   = Js_of_ocaml.Js
module Con  = Js_of_ocaml.Console

let js_str  = Js.string
let js_num  = Js.number_of_float
open Draw

let _ =
    let pltstate   = ref None in
    let ntfclbck   = ref None in
    let refcntr    = ref 0.   in
    let default    = "Default" in
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
    let sheet = Eincalc.Ndcontroller.create_default_controller default plotcb logger in

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

        method get row col = (
            match Eincalc.Ndcontroller.fetch_grid_label sheet default with
            | Some { grid=_g; _ } -> 
                (match Eincalc.Ndmodel.Grid.find_opt _g (row, col) with 
                    | Some Eincalc.Ndmodel.TValue  s -> (js_str s)
                    | Some Eincalc.Ndmodel.TNumber f -> (js_str (Format.sprintf "%.2f" f))
                    | Some Eincalc.Ndmodel.TNat f    -> (js_str (string_of_int f))
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
            (match Eincalc.Ndcontroller.fetch_grid_label sheet default with
                | Some { grid=_g; _ } -> 
                    Eincalc.Ndmodel.Grid.add _g (row, col) (TNumber vstr)
                | None ->
                    Con.console##error "cant add number - Missing grid!!!"
            )
        )

        (* TODO: use OptDef or Opt for null checks *)
        method gridaddstring row col (value: Js.js_string Js.t) = (
            let vstr = Js.to_string value in
            (*let _ = Con.console##log (Format.sprintf "adding %s to %d %d*)
                    (*\n" vstr row col)  in*)
            (match Eincalc.Ndcontroller.fetch_grid_label sheet default with
                | Some { grid=_g; _ } -> 
                    Eincalc.Ndmodel.Grid.add _g (row, col) (TValue vstr)
                | _ -> 
                    Con.console##error "cant add value - Missing grid!!!"
            )
        )

        method griderase row col rend cend = (
            Eincalc.Ndcontroller.erase_grid sheet row rend col cend 
        )

        (* TODO: use OptDef or Opt for null checks *)
        method executecode (value: Js.js_string Js.t) = (
            (* TODO: if it starts with `=` we evaluate it *)
            let vstr = Js.to_string value in
            let _    = Eincalc.Repl.handle_scan_exp { sheet with active=default; } vstr
            in Js._true
        )

        (* TODO: use OptDef or Opt for null checks *)
        (* TODO: figuring out structure here is very rudimentary - make updates *)
        method paste row col (value: Js.js_string Js.t) = (
            let vstr = Js.to_string value in
            let sep = if String.contains vstr '\t' then '\t' else if (not @@ String.contains vstr ',') then ' ' else ',' in
            (match Eincalc.Ndcontroller.paste_values sheet default sep (row, col) vstr with 
                | Ok    _v -> 
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

