module Html = Js_of_ocaml.Dom_html
module Dom  = Js_of_ocaml.Dom
module Js   = Js_of_ocaml.Js
module G    = Graphics_js

let js_str  = Js.string
let js_num  = Js.number_of_float

let doc = Html.document

let canvas_width  = 300.
let canvas_height = 150.

let create_canvas () =
  let r = Html.createCanvas doc in
  r##.width  := int_of_float canvas_width;
  r##.height := int_of_float canvas_height;
  r
;;

let rec draw_things (context: Html.canvasRenderingContext2D Js.t) counter = 
  let open Lwt.Syntax in
  context##.font := (Js.string "50px serif");
  context##fillText (js_str "Hello World") (js_num 20.) (js_num 90.); 
  context##strokeRect (js_num 0.) (js_num 0.) (js_num canvas_width) (js_num canvas_height);

  context##.font := js_str "20px serif";
  context##fillText (js_str ("This page has been open for:")) (js_num 20.) (js_num 110.);
  context##fillText (js_str ((string_of_int counter) ^ " seconds")) (js_num 20.) (js_num 130.);
  
  let* _ = (Js_of_ocaml_lwt.Lwt_js.sleep 1.0) in
    context##clearRect (js_num 0.) (js_num 0.) (js_num canvas_width) (js_num canvas_height);
    draw_things context (counter + 1)
;;

let _onload domnode =
  let canvas = create_canvas () in
  G.open_canvas   canvas;
  Dom.appendChild domnode canvas;
  let c = canvas##getContext Html._2d_ in
  let _ = draw_things c 0 |> ignore in
  Js._false
;;

let _ =
    let _g = Spinal.Ndmodel.enum_grid (10, 10) in
    let _ = Js.export "myLib"
            (object%js (_self)

                (*you can export polymorphic methods to javascript.*)
                method add x y = x + y

                (*you can export values as well. Note that exported values and functions must follow the [snake case]().*)
                val myVal = _g

                (*The Js_of_ocaml.Js module contains javascript types and functions to handle them.*)
                method hello (name: Js.js_string Js.t) : (Js.js_string Js.t) = 
                (
                    let name = Js.to_string name in 
                    let hello_name = "hello " ^ name in
                    Js.string hello_name
                )

                method load (elt: #Dom.node Js.t) = 
                (
                    _onload elt
                )

                (*You can also write javascript within your OCaml code.
                  Note that the versino of javascript supported is not recent               
                  (no let keyword for example).*)
                method typedArray _ =
                (
                    let init_typed_array = Js.Unsafe.js_expr
                        {js|(function() {
                            var buf = new Uint8Array(2);
                            buf[0] = 1;
                            return buf;
                            })
                        |js}
                    in
                    let typed_array = Js.Unsafe.fun_call init_typed_array [||] in
                    let typed_array = Js_of_ocaml.Typed_array.String.of_uint8Array typed_array in
                    String.iter (fun (x:char) -> Format.printf "%d\n" (int_of_char x)) typed_array
                )

            end) 
  in
  Format.printf "Hello console from ocaml!";
  (*Html.window##.onload := Html.handler onload*)

