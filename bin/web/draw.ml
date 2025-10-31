(*
    WARNING: this file was partially vibe-coded!!
    NB: common error i got is  when methods are accessed like properties eg 

    canvas##style##width   := (string_of_int display_width ^ "px");
    should be
    canvas##.style##.width := (string_of_int display_width ^ "px");

    the error will show up as confabulation of a method and a property
    but is cryptic to decode so... yeah

*)
open Js_of_ocaml
open Eincalc.Plotter;;

let js_str  = Js.string
let js_num  = Js.number_of_float


(* phantom positions that need to be compensated for when dragging *)
let _layout_header = 104;;
let _layout_drawer = 56;;

(* Get display dimensions *)
let _display_width =  400;;
let _display_height = 240;;

(* ============================================================
   CANVAS MANAGER
   ============================================================ *)

module Canvas = struct

    type t = {
        canvas:                Dom_html.canvasElement Js.t;            [@opaque]
        ctx:                   Dom_html.canvasRenderingContext2D Js.t; [@opaque]
        mutable shapes:        shape list;
        mutable current_color: color;
        mutable initialx:      float;
        mutable initialy:      float;
        mutable currentx:      int;
        mutable currenty:      int;
        mutable is_dragging:   bool;
        pixel_ratio:           float;
        display_width:         int;
        display_height:        int;
    } [@@deriving show]

    (* Get device pixel ratio *)
    let get_pixel_ratio () =
        Js.float_of_number Dom_html.window##.devicePixelRatio
    ;;

    (* Setup canvas for high-DPI displays *)
    let setup_high_dpi canvas display_width display_height =
        let pixel_ratio = get_pixel_ratio () in

        (* Set CSS size (how big it appears) *)
        canvas##.style##.width  := js_str (string_of_int display_width ^ "px");
        canvas##.style##.height := js_str (string_of_int display_height ^ "px");


        (* Set actual canvas size (scaled by pixel ratio) *)
        let scaled_width  = (float_of_int display_width  *. pixel_ratio) in
        let scaled_height = (float_of_int display_height *. pixel_ratio) in
        canvas##.width  := int_of_float scaled_width;
        canvas##.height := int_of_float scaled_height;

        (* Scale context to match *)
        let ctx = canvas##getContext Dom_html._2d_ in
        ctx##scale (js_num pixel_ratio) (js_num pixel_ratio);

        pixel_ratio
    ;;

    let create (canvas_id: string) =
        let doc = Dom_html.document in
        let canvas = Dom_html.createCanvas doc in
        let canvas = 
            Dom_html.CoerceTo.canvas canvas 
            |> (Fun.flip Js.Opt.get (fun () -> failwith "Not a canvas"))
        in

        (* Setup for retina *)
        let pixel_ratio = setup_high_dpi canvas _display_width _display_height in
        let ctx = canvas##getContext Dom_html._2d_ in

        {
            canvas;
            ctx;
            shapes = [];
            is_dragging = false;
            (* offsets *)
            initialx = 0.; 
            initialy = 0.;
            currentx = 0;
            currenty = 0;
            current_color = "#4CAF50";
            pixel_ratio;
            display_width=_display_width;
            display_height=_display_height;
        }
    ;;

    (* Clear the canvas *)
    let clear t =
        (* Use display dimensions, not scaled canvas dimensions *)
        let width =  float_of_int t.display_width in
        let height = float_of_int t.display_height in
        t.ctx##.fillStyle := Js.string "white";
        t.ctx##fillRect (js_num 0.) (js_num 0.) (js_num width) (js_num height)
    ;;

    (* Render a single shape *)
    let render_shape ctx = function
        | Box { x; y; width; height; color; linewidth } ->
            ctx##.fillStyle := Js.string color;
            ctx##fillRect (js_num x) (js_num y) (js_num width) (js_num height);
            ctx##.strokeStyle := js_str "#333";
            ctx##.lineWidth := js_num linewidth;
            ctx##strokeRect (js_num x) (js_num y) (js_num width) (js_num height)

        | Line { x; y; fx; fy; linewidth; color } ->
            ctx##beginPath;
            ctx##moveTo (js_num x) (js_num y);
            ctx##lineTo (js_num fx) (js_num fy);
            ctx##.strokeStyle := js_str color;
            ctx##.lineWidth   := js_num linewidth;
            ctx##stroke

        | Spline { x; y; cp1x; cp1y; cp2x; cp2y; linewidth; color } ->
            ctx##beginPath;
            ctx##bezierCurveTo (js_num cp1x) (js_num cp1y) (js_num cp2x) (js_num cp2y) (js_num x) (js_num y);
            ctx##.strokeStyle := js_str color;
            ctx##.lineWidth   := js_num linewidth;
            ctx##stroke

        | Circle { x; y; radius; color; linewidth } ->
            ctx##.fillStyle := Js.string color;
            ctx##beginPath;
            ctx##arc (js_num x) (js_num y) (js_num radius) (js_num 0.) (js_num (2. *. Float.pi)) Js._false;
            ctx##fill;
            ctx##.strokeStyle := js_str "#333";
            ctx##.lineWidth   := js_num linewidth;
            ctx##stroke

        | Text { x; y; text; color } ->
            ctx##.fillStyle := Js.string color;
            ctx##.font := js_str "14px Arial";
            ctx##fillText (js_str text) (js_num x) (js_num y)
    ;;

    (* Render all shapes *)
    let render_all t =
        clear t;
        List.iter (render_shape t.ctx) t.shapes

    (* Add a shape and re-render *)
    let add_shape t shape =
        t.shapes <- shape :: t.shapes;
        render_all t

    (* Clear all shapes *)
    let clear_shapes t =
        t.shapes <- [];
        render_all t

    (* Get shape count *)
    let shape_count t = List.length t.shapes

    (* Update current color *)
    let set_color t color =
        t.current_color <- color
end

(* Keep shape within canvas bounds *)
let clamp_to_bounds t shape =
    let width = float_of_int t.Canvas.display_width in
    let height = float_of_int t.Canvas.display_height in
    match shape with
    | Box b ->
        let x = max 0. (min b.x (width -. b.width)) in
        let y = max 0. (min b.y (height -. b.height)) in
        Box { b with x; y }
    | Circle c ->
        let x = max c.radius (min c.x (width -. c.radius)) in
        let y = max c.radius (min c.y (height -. c.radius)) in
        Circle { c with x; y }
    | Line l -> 
        let mx  = (max 0. (min l.x  width)) in
        let mfx = (max 0. (min l.fx width)) in
        let my  = (max 0. (min l.y  width)) in
        let mfy = (max 0. (min l.fy width)) in
        Line { l with x=mx; y=my; fx=mfx; fy=mfy; }
    | Spline l -> 
        let mx  = (max 0. (min l.x  width)) in
        let my  = (max 0. (min l.y  width)) in
        let mcp1x = (max 0. (min l.cp1x width)) in
        let mcp1y = (max 0. (min l.cp1y width)) in
        let mcp2x = (max 0. (min l.cp2x width)) in
        let mcp2y = (max 0. (min l.cp2y width)) in
        Spline { l with x=mx; y=my; cp1x=mcp1x; cp1y=mcp1y;  cp2x=mcp2x; cp2y=mcp2y; }
    | Text _ -> shape
;;

(* ============================================================
   EVENT HANDLERS
   ============================================================ *)

(* TODO: does not yet handle touch events! *)
(* TODO: best handled in javascript due to too much overhead!!! ! *)
module EventHandlers = struct

    let setup_drag_handler canvas_manager = 
        let canvas = canvas_manager.Canvas.canvas in
        let _downid = Dom_html.addEventListener canvas Dom_html.Event.mousedown 
            (Dom_html.handler (fun ev -> 
                ev##preventDefault;
                canvas_manager.Canvas.is_dragging <- true;
                canvas##.style##.cursor   := (js_str ("grabbing"));

                (*// Calculate offset from mouse position to div's top-left corner*)
                let bnd = canvas##getBoundingClientRect in
                let cx = (Js.to_float ev##.clientX) in  
                let cy = (Js.to_float ev##.clientY) in
                let bx = (Js.to_float bnd##.left) in 
                let by = (Js.to_float bnd##.top) in

                canvas##.classList##add (js_str "ring-2"); 

                canvas_manager.Canvas.initialx <- (cx);
                canvas_manager.Canvas.initialy <- (cy);
                canvas_manager.Canvas.currentx <- (int_of_float bx - _layout_drawer);
                canvas_manager.Canvas.currenty <- (int_of_float by - _layout_header);

                Js._true
            ))
            Js._true
        in 
        let _downid = Dom_html.addEventListener canvas Dom_html.Event.mousemove
            (Dom_html.handler (fun ev -> 
                ev##preventDefault;
                if canvas_manager.Canvas.is_dragging then 
                    (
                        canvas##.style##.cursor   := (js_str ("grabbing"));
                        let translate_x = int_of_float @@ ((Js.to_float ev##.clientX) -. canvas_manager.Canvas.initialx) in
                        let translate_y = int_of_float @@ ((Js.to_float ev##.clientY) -. canvas_manager.Canvas.initialy) in
                        (
                            (*// Calculate offset from mouse position to div's top-left corner*)
                            let newx = (canvas_manager.Canvas.currentx + translate_x + _layout_drawer) in
                            let newy = (canvas_manager.Canvas.currenty + translate_y - 2) in
                            (*canvas##.style##.transform := (js_str "none");*)
                            canvas##.style##.left := js_str ((string_of_int @@ newx) ^ "px");
                            canvas##.style##.top  := js_str ((string_of_int @@ newy) ^ "px");
                            Js._true
                        )
                    )
                else 
                    Js._true
            ))
            Js._true
        in 
        Dom_html.addEventListener canvas Dom_html.Event.mouseup
            (Dom_html.handler (fun ev -> 
                ev##preventDefault;
                canvas_manager.Canvas.is_dragging <- false;
                canvas##.style##.cursor   := (js_str ("grab"));
                (*// Calculate offset from mouse position to div's top-left corner*)
                let bnd = canvas##getBoundingClientRect in
                let bx = (Js.to_float bnd##.left) in 
                let by = (Js.to_float bnd##.top) in
                ignore(canvas_manager.Canvas.currentx = int_of_float bx);
                ignore(canvas_manager.Canvas.currenty = int_of_float by);
                canvas##.classList##remove (js_str "ring-2"); 
                Js._true
            ))
            Js._true
    ;;

    (* Setup button click handlers *)
    let setup_button_click btn_id handler =
        let doc = Dom_html.document in
        match Js.Opt.to_option (doc##getElementById (Js.string btn_id)) with
        | Some btn ->
            Dom_html.addEventListener btn Dom_html.Event.click
                (Dom_html.handler (fun _ev ->
                    handler ();
                    Js._true
                ))
                Js._true
            |> ignore
        | None -> ()
    ;;

    (* Clear all shapes *)
    let clear_all canvas_manager () =
        Canvas.clear_shapes canvas_manager;
    ;;
end

type plots = { 
    parent:   Dom.element Js.t;
    canvases: Canvas.t list; 
};;

let init (node: Dom.element Js.t) = 
    {
        parent=node
        ;   canvases=[]  
    }
;;

(* TODO: canvas may go off view completely - find a way to bring it back! *)
let add_canvas plts = 
    let canvaselt  = Canvas.create "#fff" in
    let _ = (
        canvaselt.canvas##.style##.position := (js_str "absolute");
        canvaselt.canvas##.style##.left     := (js_str ("121px"));
        canvaselt.canvas##.style##.top      := (js_str ("32px"));
        canvaselt.canvas##.style##.border   := (js_str ("2px solid black"));
        canvaselt.canvas##.style##.cursor   := (js_str ("grab"));
        canvaselt.canvas##.style##.zIndex   := (js_str ("9999"));
        canvaselt.canvas##.classList##add (js_str "shadow-2xl");
        canvaselt.canvas##.classList##add (js_str "rounded");
        canvaselt.canvas##.classList##add (js_str "transition-shadow");
        canvaselt.canvas##.classList##add (js_str "duration-150");
        canvaselt.canvas##.classList##add (js_str "hover:shadow-primary-blue/50");
        Canvas.clear canvaselt;
        Canvas.render_shape canvaselt.ctx (
            Circle { 
                x=((float_of_int (_display_width/2)) +. 5.); 
                y=((float_of_int (_display_height/2)) -. 5.); 
                linewidth=1.;
                radius=(10.); 
                color="green" 
            })
        ;
        Canvas.render_shape canvaselt.ctx (
            Box { 
                x=((float_of_int (_display_width/2)) +. 20.); 
                y=((float_of_int (_display_height/2))); 
                width=10.;
                height=10.;
                linewidth=1.;
                color="red" 
            })
        ;
        Canvas.render_shape canvaselt.ctx (
            Text { 
                x=((float_of_int (_display_width/2))); 
                y=((float_of_int (_display_height/2)) +. 20.); 
                text="Point,Line,Plane";
                color="blue";
            })
        ;
        Canvas.render_shape canvaselt.ctx (
            Line { 
                x=((float_of_int (_display_width/4))); 
                y=((float_of_int (_display_height/4))); 
                fx=((float_of_int (_display_width  - _display_width/4))); 
                fy=((float_of_int (_display_height - _display_height/4))); 
                linewidth=2.;
                color="gray";
            })
        ;
        Canvas.render_shape canvaselt.ctx (
            Spline { 
                x=((float_of_int (_display_width/3))); 
                y=((float_of_int (_display_height* 2/3))); 
                cp1x=((float_of_int (_display_width * 2/3))); 
                cp1y=((float_of_int (_display_height - _display_height/12))); 
                cp2x=((float_of_int (_display_width/6))); 
                cp2y=((float_of_int (_display_height*5/6))); 
                linewidth=2.;
                color="purple";
            })
        ;
        ignore(EventHandlers.setup_drag_handler canvaselt);
    ) in
    let _ = Dom.appendChild plts.parent canvaselt.canvas in
    { plts with canvases = canvaselt :: plts.canvases }
;;
