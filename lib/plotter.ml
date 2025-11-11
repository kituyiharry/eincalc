(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* ============================================================
   SHAPE TYPES (Primitive Shapes)
   ============================================================ *)
open Ndview;;

type color = string [@@deriving show];;

type shape =
    | Box of    { x: float; y: float; width:  float;  height: float; color: color; linewidth: float; border: color }
    | Circle of { x: float; y: float; radius: float;  color:  color; linewidth: float; border: color; }
    | Text of   { x: float; y: float; text:   string; color:  color; size: int }
    | Line of   { x: float; y: float; fx: float; fy: float; linewidth: float; color: color }
    | Spline of { cp1x: float; cp1y: float; cp2x: float; cp2y: float; x: float; y: float; linewidth: float; color: color; }
    | Clear (* clears the canvas *)
    | Reset (* clears the shape buffer - not the canvas - useful to prevent growth *)
[@@deriving show];;

type plotctx = {
        xbound: int 
    ;   ybound: int 
    ;   handle: string
    ;   padding: int
    ;   plotcb:  ((string * int list * shape list) -> unit)
}

type scatterctx = { 
        plot  : plotctx
    ;   radius: float 
    ;   border: string 
    ;   color:  string 
    ;   xlabel: string 
    ;   ylabel: string
}

let make_plot_title vw title = 
    Text { 
        x=(float_of_int ((vw/2) - ((String.length title)*3)));
        y=20.; color="black";size=22;
        text=title
    }
;;

let make_plot_x_label vw vh xlabel = 
    Text { 
        x=(float_of_int (vw/2 - ((String.length xlabel)*3)));
        y=(float_of_int (vh-8));
        color="black";size=12;text=xlabel
    } 
;;

let make_plot_y_label vw vh ylabel = 
    Text { 
        x=8.;y=(float_of_int (vh/2));
        color="black";size=12;text=ylabel
    }
;;

let make_scaler minval maxval minbound maxbound = 
    let mdiff  = maxval   -. minval in 
    (* avoid div by 0 *)
    let mdiff  = if mdiff = 0. then 1. else mdiff in
    let rdiff  = maxbound -. minbound in
    fun v -> 
        (minbound +. (((v -. minval) *. rdiff) /. mdiff))
;;

let inverse_transform_scaler minval maxval minbound maxbound = 
    let rdiff  = maxbound -. minbound in
    let mdiff  = maxval   -. minval in 
    let mdiff  = if mdiff = 0. then 1. else mdiff in
    fun v -> 
        ((v -. minbound) /. (rdiff)) *. (mdiff) +. minval 
;;

let make_axis xscaler xst xbound yscaler yst transformy ybound = 
    Line {
        x= (xscaler xst);     y=(transformy (yscaler yst));
        fx=(xscaler xbound); fy=(transformy (yscaler ybound));
        linewidth=1.;        color="black"
    }
;;

let make_countdown_seq start = 
    Seq.unfold (fun x -> 
        if x <= 0. then 
            None 
        else 
            Some (x, x -. 1.)
    ) start
;;

let grid_hor_lines height num pfloat tx_mn tx_mx = 
    let numf = float_of_int num in
    let span = ((float_of_int height) /. (numf)) in 
    make_countdown_seq (numf) 
    |> Seq.map (fun id -> 
        let cy = (id) *. span in
        Line { 
            x =tx_mn;  y=(cy);
            fx=tx_mx; fy=(cy);
            linewidth=0.5;    color="gray"
        }
    )
    |> List.of_seq
;;

let grid_hor_text height num pfloat tx_mn tx_mx invscaler translate = 
    let numf = float_of_int num in
    let span = ((float_of_int height) /. numf) in 
    make_countdown_seq numf
    |> Seq.map (fun id -> 
        let cy = id *. span in
        let t = (-. translate cy) in
        let v = invscaler t in
        let _ = Format.printf "%f: cy is %f -> %f inv: %f\n" id cy t v in
        Text { 
            x =tx_mn-.24.;  y=(cy);
            color="black"; size=8;
            text=(Format.sprintf "%.2f" v)
        }
    )
    |> List.of_seq
;;

let grid_vert_lines width num pfloat ty_mn ty_mx = 
    let numf = float_of_int num in
    let span = ((float_of_int width) /. (numf)) in 
    make_countdown_seq (numf) 
    |> Seq.map (fun id -> 
        let cy = id *. span in
        Line { 
            x=(cy) ; y =ty_mn;
            fx=(cy); fy=ty_mx;
            linewidth=0.5;    color="gray"
        }
    )
    |> List.of_seq
;;


let grid_vert_text width num pfloat ty_mn ty_mx invscaler = 
    let numf = float_of_int num in
    let span = ((float_of_int width) /. (numf)) in 
    make_countdown_seq (numf) 
    |> Seq.map (fun id -> 
        let cx = id *. span in
        let v = invscaler cx in
        Text { 
            x=(cx) ; y =ty_mn+.8.;
            color="black"; size=8;
            text=(Format.sprintf "%.2f" v)
        }
    )
    |> List.of_seq
;;

(*
INFO: Test on https://www.mathsisfun.com/data/scatter-xy-plots.html
=(@b4..c15) | plot<'Heat', [320,240], scatter<[::, 0:1:], [::, 1:1:], {xl='Temp',yl='Ice Cream',c='red',r=3}>>
*)
let scatter (type data) (ctx: scatterctx) (module SliceView: NDView with type t = data) (_xview: data) (_yview: data) = 

    let (_x_mn, _x_mx) = Masks.minmaxvalue (module SliceView) _xview in
    let (_y_mn, _y_mx) = Masks.minmaxvalue (module SliceView) _yview in

    let _xseq   = SliceView.to_seq _xview in
    let _yseq   = SliceView.to_seq _yview in

    let width  = ctx.plot.xbound+(ctx.plot.padding*2) in
    let height = ctx.plot.ybound+(ctx.plot.padding*2)in

    let hfloat = float_of_int height in
    let pfloat = float_of_int ctx.plot.padding in

    let num = Types.cardinal_of_dim (SliceView.shape _xview) in

    let xscaler  = make_scaler _x_mn _x_mx (pfloat) ((float_of_int ctx.plot.xbound) +. pfloat) in
    let xinverse = inverse_transform_scaler _x_mn _x_mx (pfloat) ((float_of_int ctx.plot.xbound) +. pfloat) in
    let yscaler  = make_scaler _y_mn _y_mx (0.)     (float_of_int (ctx.plot.ybound)) in
    let yinverse = inverse_transform_scaler _y_mn _y_mx (0.) (float_of_int (ctx.plot.ybound)) in

    let transformy yv = hfloat -. (pfloat +. yv) in
    let reversetry yv = (yv -. hfloat) +. pfloat in

    let _vals   = (Seq.zip _xseq _yseq) |> Seq.map (fun (x,y) -> 
        let x', y' = (xscaler x), (yscaler y) in
        Circle {
            x=x'; y=(transformy y'); radius=ctx.radius; 
            color=ctx.color; linewidth=0.; border=ctx.border 
        }
    ) in

    let ttl = make_plot_title   width ctx.plot.handle in
    let xt  = make_plot_x_label width height ctx.xlabel in
    let yt  = make_plot_y_label width height ctx.ylabel in

    let xlegend = make_axis xscaler (_x_mn-.pfloat) (_x_mx+.pfloat) yscaler
        (_y_mn) (transformy) (_y_mn)
    in

    let ylegend = make_axis xscaler (_x_mn) (_x_mn) yscaler
        (_y_mn-.(pfloat*.2.)) (transformy) (_y_mx+.(pfloat*.2.))
    in

    (* FIXME: clear start points in the graph *)

    (* from top to bottom *)
    let vlines = grid_vert_lines width num pfloat 
        (transformy @@ yscaler (_y_mn)) 
        (transformy @@ yscaler (_y_mx)) in 

    let vtext = grid_vert_text width num pfloat 
        (transformy @@ yscaler (_y_mn)) 
        (transformy @@ yscaler (_y_mx)) xinverse in 

    (* from left to right *)
    let hlines = 
        grid_hor_lines height num pfloat 
        ((xscaler _x_mn)) 
        (xscaler (_x_mx+.pfloat)) in 

    let htext = 
        grid_hor_text height num pfloat 
        ((xscaler _x_mn)) 
        (xscaler (_x_mx+.pfloat)) yinverse reversetry in 

    ctx.plot.plotcb (
        ctx.plot.handle, 
        [width; height],  
        Reset :: ylegend :: xlegend :: ttl :: xt :: yt :: (List.of_seq _vals) @
        vlines @ vtext @ hlines @ htext
    ) 
;;


