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


