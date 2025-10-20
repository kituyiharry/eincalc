(*module Html = Js_of_ocaml.Dom_html*)
(*module Dom  = Js_of_ocaml.Dom*)
(*module G    = Graphics_js*)
module Js   = Js_of_ocaml.Js

let js_str  = Js.string
let js_num  = Js.number_of_float

let _ =
    let _g = Spinal.Ndmodel.enum_grid (10, 10) in
    let _ = Js.export "myLib"
            (object%js (_self)
                
                method get row col = (
                    match Spinal.Ndmodel.Grid.find_opt _g (row, col) with 
                    | Some v -> (
                        match v with
                        | Spinal.Ndmodel.TFormulae s | Spinal.Ndmodel.TValue s -> (js_str s)
                        | Spinal.Ndmodel.TNumber f-> js_str (string_of_float f)
                    )
                    | None   -> js_str ""
                )

                (*you can export polymorphic methods to javascript.*)
                (*method add x y = x + y*)

                (*you can export values as well. Note that exported values and functions must follow the [snake case]().*)
                (*val repo = _g*)

                (*The Js_of_ocaml.Js module contains javascript types and functions to handle them.*)
                (*
                 *method hello (name: Js.js_string Js.t) : (Js.js_string Js.t) = 
                 *(
                 *    let name = Js.to_string name in 
                 *    let hello_name = "hello " ^ name in
                 *    Js.string hello_name
                 *\)
                 *)

                (*
                 *method load (elt: #Dom.node Js.t) = 
                 *(
                 *    _onload elt
                 *\)
                 *)

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

