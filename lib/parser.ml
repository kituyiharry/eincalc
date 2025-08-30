(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

open Tokens;;
open Lexer;;

let (>>==) = Result.bind;;

type lit     = 
    | Shape of (char * int) list 
and  cell    = string * int      (* Rows are strings, Columns are numbers *)
and  dimnsn  = lit               (* literal and its index *)
and  crange  = cell * cell       (* spreadsheet cell *)
and  params  = crange list       (* function parameters *)
and  einsum  = { 
        inp: dimnsn list         (* input  - at least one *)
    ;   out: dimnsn option       (* output - can be empty *)
}
and  formula = einsum * params   (* formula specification *)
and  program = formula list
[@@deriving show];;

type precedence = 
    | PrecNone 
    | PrecAssgn 
;;

type prattstate = {
        curr: lexeme  option
    ;   prev: lexeme  option
    ;   prog: program
};;

let prattempty = {
    curr = None; prev = None; prog = []
};;

let einempty = { 
        inp = []
    ;   out = None
    ;
};;

type parseres = (program, string) result
type parsefn  = (prattstate -> parseres)

type prattrule = {
        prefix: parsefn option
    ;   infix:  parsefn option
    ;   prec:   precedence
};;

let ruleidx tok = 
    match tok with 
    | TNumeral  _ ->  0 
    | TAlphaNum _ ->  1 
    | TArrow      ->  2 
    | TRange      ->  3 
    | TComma      ->  4 
    | TQuote      ->  5 
    | TLeftParen  ->  6 
    | TRightParen ->  7 
;;

let rules = [|
        (* TNumeral of int    *)
        { prefix=None; infix=None; prec=PrecNone }
        (* TAlphaNum of string *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TArrow              *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TRange              *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TComma              *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TQuote              *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TLeftParen          *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TRightParen         *)
    ;   { prefix=None; infix=None; prec=PrecNone }
|];;

let getrule tok = 
    rules.(ruleidx tok)
;;

(* --- *)

(* check current token tag (not the data) without advancing state *)
let checktag tokn ({curr; _}) = 
    match curr with
    | None -> false 
    | Some { tokn=tok; _ } -> ((Obj.tag @@ Obj.repr tok)  = (Obj.tag @@ Obj.repr tokn))
;;

(* check current token without advancing state *)
let check tokn ({curr; _}) = 
    match curr with
    | None -> false 
    | Some { tokn=tok; _ } -> (equal_ttype tok tokn)
;;

(* advancing to the next token *)
let advance (state, rem) = 
    match rem with 
    | [] -> 
        ({ state with prev=state.curr; curr=(None) }, rem)
    | hd :: rest -> 
        ({ state with prev=state.curr; curr=(Some hd) }, rest)
;;

(* --- *)

let validate word = 
    String.length word > 0 && String.is_valid_utf_8 word
;;

let parse_ein_inp ein word = 
    { ein with 
        inp=(Shape (List.of_seq @@ Seq.mapi (fun i c -> (c, i)) (String.to_seq word)) :: ein.inp) 
    } 
;;

let parse_ein_out ein word = 
    if validate word then  
        { ein with out=Some (Shape (List.of_seq @@ Seq.mapi (fun i c -> (c, i)) (String.to_seq word))) } 
    else 
        { ein with out=None }
;;

let parse_einsum pratt = 
    let rec _parse ein state = 
        (match (fst state).curr with
            |  Some ({ tokn; _ }) -> 
                (match tokn with 
                    | TAlphaNum v -> 
                        (if validate v then (
                            let (prt, rem') as state' = advance state in
                            if check TComma prt then
                                _parse (parse_ein_inp ein v) (advance state') 
                            else 
                                Ok ({ prt with prog=(((parse_ein_inp ein v), []) :: prt.prog) }, rem')
                        ) else (Error "Input indices invalid - please use at least one ascii chars"))
                    | _ -> 
                        Error "Unimplemented"
                )
            |  _          -> 
                Error "Unimplemented"

        )
    in
    (>>==) (_parse einempty pratt) (fun (prt, rem) -> 
        if check TArrow prt then 
            let (prt', rem') = advance (prt, rem) in 
            (match (prt').curr with 
                | Some ({ tokn; _ }) -> 
                    (match tokn with
                        | TAlphaNum v -> 
                            (* Grab the last einsum and update the output *)
                            let ein' = List.hd prt'.prog in
                            let upd  = parse_ein_out (fst ein') v in
                            Ok (advance ({ prt' with prog=(upd, snd ein') :: (List.tl prt'.prog) }, rem'))
                        | _ -> 
                            Ok (prt', rem')
                    )
                | _ -> 
                    Ok (prt', rem')
            )
        else 
            Ok (prt, rem) 
    )
;;

let parse_formulae (prt, rem) = 
    (parse_einsum (prt, rem))
;;

let parse lstream = 
    let rec _parse_lxms (state,  lxms) = 
        (if check TLeftParen state
            then
                ((>>==) (parse_formulae @@ advance (state, lxms)) (fun state' -> 
                    (if check TRightParen (fst state') then
                        _parse_lxms (advance state')
                        else
                        Error "Unclosed einsum formulae"
                    )
                )) 
            else (Ok (state, lxms))
        )
    in
    _parse_lxms @@ advance (prattempty, lstream)
;;
