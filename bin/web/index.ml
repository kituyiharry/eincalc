module Html = Js_of_ocaml.Dom_html
module Dom = Js_of_ocaml.Dom
module Js = Js_of_ocaml.Js
module G =  Graphics_js

let js_str = Js.string
let js_num = Js.number_of_float

let doc = Html.document

let canvas_width = 300.
let canvas_height = 150.

let create_canvas () =
  let r = Html.createCanvas doc in
  r##.width := int_of_float canvas_width;
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

let onload _event =
  let canvas = create_canvas () in
  G.open_canvas canvas;
  Dom.appendChild doc##.body canvas;
  let c = canvas##getContext Html._2d_ in
  let _ = draw_things c 0 |> ignore in
  Js._false
;;

let _ =
  Html.window##.onload := Html.handler onload

