(*
 *
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *   Token Types used to represent Steinmetz formulae
 *)
type ttype =
    | TNumeral  of int    (* 0..9+ *)
    | TFloat    of float  (* x.y *)
    | TAlphaNum of string (* a..zA..z+ *)
    | TArrow              (* -> *)
    | TRange              (* .. *)
    | TComma              (* , *)
    | TQuote              (* '"' *)
    | TLeftParen          (* ( *)
    | TRightParen         (* ) *)
    | TLeftBracket        (* [ *)
    | TRightBracket       (* ] *)
    | TLeftAngle          (* < *) 
    | TRightAngle         (* > *) 
    | TUnderscore         (* _ *)
    | TCaret              (* ^ *)
    | TAt                 (* @ *)
    | TPipe               (* | *)

    | KFor                (* loop *)
    | KDecl               (* declaration *)
    | KIdent of string    (* i,j,k.... *)
    | KNum   of int       (* 1..10 *)
    | KEq                 (* = *)
    | KGreater            (* > *)
    | KLess               (* < *)
    | KPlus               (* add *)
    | KMult               (* multiply *)
    | KBlockStart         (* { *)
    | KBlockEnd           (* } *)
    | KAddAssign          (* += *)

[@@deriving show, eq];;
