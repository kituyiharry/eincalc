(* TODO: eventually remove svelte canvas with preference to raw Canvas manipulation  *)
(*module Html = Js_of_ocaml.Dom_html*)
(*module Dom  = Js_of_ocaml.Dom*)
(*module G    = Graphics_js*)
module Js   = Js_of_ocaml.Js
module Con  = Js_of_ocaml.Console

let js_str  = Js.string
(*let js_num  = Js.number_of_float*)

let _ =
    let _g = Eincalc.Ndmodel.plain_grid 100 in
    (*  TODO: come up with a better controller interface *)
    let _ = Js.export "myLib"
            (object%js (_self)
                
                method get row col = (
                    match Eincalc.Ndmodel.Grid.find_opt _g (row, col) with 
                    | Some Eincalc.Ndmodel.TValue  s -> (js_str s)
                    | Some Eincalc.Ndmodel.TNumber f -> (js_str (Format.sprintf "%.2f" f))
                    | None   -> js_str ""
                )

                (* TODO: use OptDef or Opt for null checks *)
                method gridaddnumber row col (value: Js.number Js.t) = (
                    let vstr = Js.to_float value in
                    (*let _ = Con.console##log (Format.sprintf "adding %f to %d*)
                    (*%d\n" vstr row col) in*)
                    Eincalc.Ndmodel.Grid.add _g (row, col) (TNumber vstr)
                )

                (* TODO: use OptDef or Opt for null checks *)
                method gridaddstring row col (value: Js.js_string Js.t) = (
                    let vstr = Js.to_string value in
                    (*let _ = Con.console##log (Format.sprintf "adding %s to %d %d*)
                    (*\n" vstr row col)  in*)
                    Eincalc.Ndmodel.Grid.add _g (row, col) (TValue vstr)
                )

                (* TODO: use OptDef or Opt for null checks *)
                method executecode (value: Js.js_string Js.t) = (
                    let vstr = Js.to_string value in
                    (*let _ = Con.console##log (Format.sprintf "adding %s to %d %d*)
                    (*\n" vstr row col)  in*)
                    Eincalc.Repl.handle_scan_exp _g vstr
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

