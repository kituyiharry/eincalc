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
    | Clear     (* clears the canvas *)
    | Reset     (* clears the shape buffer - not the canvas - useful to prevent growth *)
[@@deriving show];;

type plotctx = {
        xbound:   int 
    ;   ybound:   int 
    ;   handle:   string
    ;   paddingx: int
    ;   paddingy: int
    ;   xtextoff: float  (*  x offset for y axis *)
    ;   ytextoff: float  (*  y offset for x axis text *)
    ;   xytextoff: float (*  y offset for x axis *)
    ;   yxtextoff: float (*  x offset for y axis text *)
    ;   gridstep: float
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

type barctx = { 
        plot  : plotctx
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

let make_plot_y_label _vw vh ylabel = 
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
    fun v -> (minbound +. (((v -. minval) *. rdiff) /. mdiff))
;;

let inverse_transform_scaler minval maxval minbound maxbound = 
    let rdiff  = maxbound -. minbound in
    let mdiff  = maxval   -. minval in 
    let mdiff  = if mdiff = 0. then 1. else mdiff in
    fun v -> ((v -. minbound) /. (rdiff)) *. (mdiff) +. minval 
;;

(* lines running from top to bottom along the x axis *)
let grid_vlines width incr y_mn y_mx x_min_scl = 
    let xst = Float.ceil x_min_scl in
    let rec addvlns vlc state = 
        if vlc >= width then 
            state
        else
            (addvlns[@tailcall]) (vlc +. incr) (Line {
                x =vlc; y =y_mn;
                fx=vlc; fy=y_mx;
                linewidth=0.5; color="gray"
            } :: state)
    in addvlns xst []
;;

(* text along x axis *)
let grid_vtext width incr y_mn x_min_scl inv = 
    let xst = Float.ceil x_min_scl in
    let rec addvlns vlc state = 
        if vlc >= width then 
            state
        else
            (addvlns[@tailcall]) (vlc +. incr) (
                Text { 
                    x=(vlc) ; y =y_mn+.10.;
                    color="black"; size=8;
                    text=(Format.sprintf "%.2f" (inv vlc))
                } :: state
            )
    in addvlns xst []
;;

(* labelled along x axis *)
let grid_vtext_labels breadth y_mn x_min lbls = 
    Array.to_seq lbls 
    |> Seq.mapi (fun indx text -> 
        Text { 
            x=(x_min +. ((float_of_int indx) *. breadth) +. 4.); y =y_mn+.10.;
            color="black"; size=8; text
        }
    )
    |> List.of_seq
;;

(* lines running from left to right along the y axis *)
let grid_hlines _height incr x_mn x_mx y_max_scl = 
    let yst = Float.ceil y_max_scl in
    let rec addvlns vlc state = 
        if vlc < incr then 
            state
        else
            (addvlns[@tailcall]) (vlc -. incr) (Line {
                x =x_mn; y =vlc;
                fx=x_mx; fy=vlc;
                linewidth=0.5; color="gray"
            } :: state)
    in addvlns yst []
;;

(* text along y axis *)
let grid_htext height incr x_mn y_max_scl inv = 
    let yst = y_max_scl in
    let rec addvlns vlc state = 
        if vlc < incr then 
            state
        else
            (addvlns[@tailcall]) (vlc -. incr) (
                Text { 
                    x=(x_mn-.16.) ; y =vlc;
                    color="black"; size=8;
                    text=(Format.sprintf "%.2f" (inv (height -. vlc)))
                }
            :: state)
    in addvlns yst []
;;

(*
INFO: Test on https://www.mathsisfun.com/data/scatter-xy-plots.html
=(@b4..c15) | plot<'Heat', [320,240], scatter<[::, 0:1:], [::, 1:1:], {xl='Temp',yl='Ice Cream',c='red',r=3}>>
*)
(*TODO: Cache scaling functions as closures to prevent recalc when called multiple times *)
let scatter (type data) (ctx: scatterctx) (module SliceView: NDView with type t = data) (_xview: data) (_yview: data) = 

    let (_x_mn, _x_mx) = Masks.minmaxvalue (module SliceView) _xview in
    let (_y_mn, _y_mx) = Masks.minmaxvalue (module SliceView) _yview in

    let _xseq   = SliceView.to_seq _xview in
    let _yseq   = SliceView.to_seq _yview in

    let width  = ctx.plot.xbound+(ctx.plot.paddingx*2) in
    let height = ctx.plot.ybound+(ctx.plot.paddingy*2)in

    let hfloat  = float_of_int height in
    let wfloat  = float_of_int width in

    let pfloatx = float_of_int ctx.plot.paddingx in
    let pfloaty = float_of_int ctx.plot.paddingy in

    (*let num = Types.cardinal_of_dim (SliceView.shape _xview) in*)
    (*let num = 10 in*)

    let xscaler  = make_scaler _x_mn _x_mx (pfloatx) ((float_of_int ctx.plot.xbound) +. pfloatx) in
    let xinverse = inverse_transform_scaler _x_mn _x_mx (pfloatx) ((float_of_int ctx.plot.xbound) +. pfloatx) in
    let yscaler  = make_scaler _y_mn _y_mx (pfloaty) ((float_of_int ctx.plot.ybound) +. pfloaty) in
    let yinverse = inverse_transform_scaler _y_mn _y_mx (pfloaty) ((float_of_int ctx.plot.ybound) +. pfloaty) in

    (* because the y axis is inverted and we want the origin to be at the bottom! *)
    let transformy yv = hfloat -. (yv) in

    (* convert data to scaled points *)
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

    (* draw origin lines for x and y axes and displaying tickers *)
    let origin_x_ = Line {
        x= (0.); y=((hfloat -. pfloaty));
        fx=(xscaler (wfloat)); fy=(hfloat -. pfloaty);
        linewidth=2.; color="black"
    } in

    let origin_y_ = Line {
        x= (pfloatx); y=(0.);
        fx=(pfloatx); fy=(hfloat);
        linewidth=2.; color="black"
    } in

    (* from top to bottom *)
    let vlines = 
        grid_vlines (wfloat) ctx.plot.gridstep (pfloaty) (hfloat-.pfloaty) (pfloatx) in

    (* text on the x axis *)
    let vtext = 
        grid_vtext (wfloat) ctx.plot.gridstep (pfloaty+.(float_of_int ctx.plot.ybound)+.ctx.plot.xytextoff) (pfloatx+.ctx.plot.xtextoff) xinverse in

    (* from left to right *)
    let hlines = 
        grid_hlines (hfloat) ctx.plot.gridstep (pfloatx) (wfloat+.pfloatx) (pfloaty+.(float_of_int ctx.plot.ybound)) in

    (* text on the y axis *)
    let htext = 
        grid_htext (hfloat) ctx.plot.gridstep (pfloatx+.ctx.plot.yxtextoff) (pfloaty+.(float_of_int ctx.plot.ybound)+.ctx.plot.ytextoff) yinverse  in

    ctx.plot.plotcb (
        ctx.plot.handle, 
        [width; height],  
        Reset ::
            origin_x_ :: origin_y_ ::
            ttl :: xt :: yt :: 
            vlines @ vtext @ hlines @ htext @ (List.of_seq _vals)
    ) 
;;

(*TODO: Cache scaling functions as closures to prevent recalc when called multiple times *)
(* 
   =(@b2..c23) | plot<'Heat', [620,240], bar<[::, 0:1:],@a2..a23, {xl='Temp',yl='Ice Cream',c='red',r=3,px=100}>> 
*)
let bar (type data) (ctx: barctx) (module SliceView: NDView with type t = data) (_hview: data) lbls = 

    (* check if negative values included *)
    let (_h_mn, _x_mx) = Masks.minmaxvalue (module SliceView) _hview in

    (* start from 0 or lower *)
    let _h_mn   = if _h_mn > 0. then 0. else _h_mn in
    (* start from 0 or higher *)
    let _x_mx   = if _x_mx < 0. then 0. else _x_mx in

    let _cnt    = Array.length lbls in
    let _hseq   = SliceView.to_seq _hview in

    let width  = ctx.plot.xbound+(ctx.plot.paddingx*2) in
    let height = ctx.plot.ybound+(ctx.plot.paddingy*2)in

    let hfloat  = float_of_int height in
    let wfloat  = float_of_int width in

    (* how each bar scales along the width *)
    let breadth =  float_of_int ((ctx.plot.xbound + ctx.plot.paddingx) / _cnt) in

    let pfloatx  = float_of_int ctx.plot.paddingx in
    let pfloaty  = float_of_int ctx.plot.paddingy in

    let heightscaler = make_scaler _h_mn _x_mx (pfloaty) ((float_of_int ctx.plot.ybound) +. pfloaty) in
    let heighinverse = inverse_transform_scaler _h_mn _x_mx (pfloaty) ((float_of_int ctx.plot.ybound) +. pfloaty) in
    (* because the y axis is inverted and we want the origin to be at the bottom! *)
    let transformy yv = hfloat -. (yv) in

    (* because the y axis is inverted and we want the origin to be at the bottom! *)
    (*let transformy yv = hfloat -. (yv) in*)

    (* TODO: convert data to scaled rects *)

    let ttl = make_plot_title   width ctx.plot.handle in
    let xt  = make_plot_x_label width height ctx.xlabel in
    let yt  = make_plot_y_label width height ctx.ylabel in

    (* draw origin lines for x and y axes and displaying tickers *)
    let origin_x_ = Line {
        x= (0.); y=((hfloat -. pfloaty));
        fx=(heightscaler (wfloat)); fy=(hfloat -. pfloaty);
        linewidth=2.; color="black"
    } in

    let origin_y_ = Line {
        x= (pfloatx); y=(0.);
        fx=(pfloatx); fy=(hfloat);
        linewidth=2.; color="black"
    } in

    (* from top to bottom *)
    (*let vlines = *)
        (*grid_vlines (wfloat) ctx.plot.gridstep (pfloaty) (hfloat-.pfloaty) (pfloatx) in*)

    (* text on the x axis *)
    let vtext = 
        grid_vtext_labels breadth (pfloaty+.(float_of_int ctx.plot.ybound)+.ctx.plot.xytextoff) (pfloatx+.ctx.plot.xtextoff) lbls in

    (* from left to right *)
    let hlines = 
        grid_hlines (hfloat) ctx.plot.gridstep (pfloatx) (wfloat+.pfloatx) (pfloaty+.(float_of_int ctx.plot.ybound)) in

    (* text on the y axis *)
    let htext = 
        (*NOTE: xinverse is actually yinverse *)
        grid_htext (hfloat) ctx.plot.gridstep (pfloatx+.ctx.plot.yxtextoff) (pfloaty+.(float_of_int ctx.plot.ybound)+.ctx.plot.ytextoff) heighinverse  in

    let theightmin = transformy (heightscaler 0.) in 
    let _vals   = 
        (*Seq.empty*)
         _hseq |> Seq.mapi (fun indx x -> 
            (* properly handle negative values *)
            let height = (transformy (heightscaler x)) -. theightmin in
             Box {
                x=(pfloatx +. ((float_of_int indx) *. breadth) +. 4.); 
                y=(theightmin); 
                width=(breadth-.8.);height;
                color=ctx.color; linewidth=0.; border=ctx.border 
             }
         ) 
    in

    ctx.plot.plotcb (
        ctx.plot.handle, 
        [width; height],  
        Reset ::
            ttl :: xt :: yt :: 
            origin_x_ :: origin_y_ ::
            (*vlines @ vtext @ hlines @ htext @ (List.of_seq _vals) *)
            vtext @ hlines @ htext @ (List.of_seq _vals) 
            (*vtext @ htext*)
    ) 
;;

