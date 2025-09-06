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
    (* TODO: make lit *)
and referral = 
    | Self  (* the current cell *)
and  cell    = string * int      (* Rows are strings, Columns are numbers *)
and  dimnsn  = lit               (* literal and its index *)
and  motion  =  
    | North of int (* ^ *)
    | South of int (* _ *)
    | East  of int (* > *)
    | West  of int (* < *)
and 'a ndarray = 
    | Raw     of 'a list
    | Collect of 'a ndarray list
and  crange  = 
    | Range    of cell * cell    (* spreadsheet cell *)
    | Scalar   of cell
    | Static   of float list     (* static array information information *)
    | NdArray  of float ndarray
    | Relative of motion * crange(* Relative cell - Up ^, Down _, Left <, Right, > *)
    | Refer    of referral       (* a way to refer to the current cell *) 
and  params  = crange list       (* function parameters *)
and  einsum  = { 
        inp: dimnsn list         (* input  - at least one - likely in reverse order. should correspond to the number of params *)
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
} [@@deriving show];;

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
    | TNumeral  _   ->  0 
    | TAlphaNum _   ->  1 
    | TArrow        ->  2 
    | TRange        ->  3 
    | TComma        ->  4 
    | TQuote        ->  5 
    | TLeftParen    ->  6 
    | TRightParen   ->  7 
    | TLeftBracket  ->  8 
    | TRightBracket ->  9 
    | TLeftAngle    ->  10 
    | TRightAngle   ->  11 
    | TUnderscore   ->  12
    | TCaret        ->  13
    | TAt           ->  14
    | TFloat   _    ->  15
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
        (* TLeftBracket         *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TRightBracket        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TLeftAngle         *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TRightAngle        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TUnderscore        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TCaret        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TUnderscore        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TAt        *)
    ;   { prefix=None; infix=None; prec=PrecNone }
        (* TFloat        *)
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

(* valid strings are non empty ascii sequences *)
let validate word = 
    String.length word > 0 && String.is_valid_utf_8 word
;;

(* NB: inputs will be in reverse order of declaration! *)
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

(* ensure the order of inputs are symmetric to the declaration *)
let reorder ein = 
    { ein with inp=List.rev ein.inp; } 
;;

type ndshape = 
    | Row of int 
    | Col of (int * ndshape)
;;

let shaper (ndarr) = 
    let rec count c ndarr =
        match ndarr with 
        | Raw _l -> c + 1
        | Collect l -> (List.fold_left (count) c l)
    in 
        match ndarr with 
        | NdArray nd -> count 0 nd
        | _ -> failwith "Expected ndarray"
;;

let arglist pr = 
    snd (List.hd pr.prog)
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
                                Ok (({ prt with prog=(((reorder @@ parse_ein_inp ein v), []) :: prt.prog) }, rem'))
                        ) else (Error (Format.sprintf "Input indices invalid - please use at least one ascii chars at %s" (show_prattstate @@ fst state))))
                    | _ -> 
                        Error (Format.sprintf "Unimplemented at %s" (show_prattstate (fst state)))
                )
            |  _          -> 
                Error (Format.sprintf "Unfinished einsum expression at %s" (show_prattstate (fst state)))
        )
    in
    (>>==) (_parse einempty pratt) (fun current -> 
        (* output of the einsum - NOT the parameters! *)
        (if check TArrow (fst current) then (
            let next = advance current in 
            (match (fst next).curr with 
                | Some ({ tokn; _ }) -> 
                    (match tokn with
                        | TAlphaNum v -> 
                            (* Grab the last einsum and update the output *)
                            let ein' = List.hd (fst next).prog in
                            let upd  = parse_ein_out (fst ein') v in
                            Ok (advance ({ (fst next) with prog=(upd, snd ein') :: (List.tl (fst next).prog) }, (snd next)))
                        | _ -> 
                            (* it was not a token - likely something closing like a Comma or RightParen *)
                            (* not what we expected - we just leave as is -  *)
                            Ok (next)
                    )
                | _ -> 
                    Error "Unexpected einsum result"
            )
        ) else (
            (* arrow can be optional *)
            Ok (current) 
        ))
    )
;;

let as_cell w = 
    match Seq.find_index (fun c ->  Lexer.isDigit c) @@ String.to_seq w  with 
    | Some idx -> 
        let row = String.sub w 0 idx in
        let col = String.sub w idx (String.length w - idx) in
        ( match int_of_string_opt col with 
            | Some col'->  Ok (row, col')
            | None     ->  Error (Format.sprintf "Invalid row value - should be a number: %s" col)
        ) 
    | None -> 
        Error "Row index not found??"
;;

let add_params ((p, r)) start close  = 
    let ein = List.hd p.prog in
    ({ p with prog=(fst ein, (Range (start, close) :: (snd ein))) :: (List.tl p.prog) }, r)
;;

let add_param ((p, r)) start  = 
    let ein = List.hd p.prog in
    ({ p with prog=(fst ein, (Scalar (start) :: (snd ein))) :: (List.tl p.prog) }, r)
;;

let add_static ((p, r)) numerals = 
    let ein = List.hd p.prog in
    ({ p with prog=(fst ein, (Static (List.rev numerals) :: (snd ein))) :: (List.tl p.prog) }, r)
;;

let add_crange (((p, r)), rangeinf) = 
    let ein = List.hd p.prog in
    ({ p with prog=(fst ein, (rangeinf :: (snd ein))) :: (List.tl p.prog) }, r)
;;

(* return consumed state + extracted numerals forming the array *)
let parse_static_array state = 
    let rec stack state' rows = 
        ( 
            (* collect an entire row first *)
            (>>==) (collect state' []) (fun (after, toadd) -> 
                (
                    (* check for more rows *)
                    if check TComma (fst after) then
                        let next = advance after in
                        (
                            (* check if we are starting a new row *)
                            if check TLeftBracket (fst next) then
                                stack (advance next) (toadd :: rows)
                            (* check if it was just a trailing comma *)
                            else if check TRightBracket (fst next) then
                                Ok (advance next, (toadd :: rows))
                            (* weird state  *)
                            else 
                                Error (Format.sprintf "Unexpected token in stack: %s!" (show_prattstate (fst next)))
                        )
                    else if check TRightBracket (fst after) then 
                        Ok (advance after, ((toadd :: rows)))
                    else
                        Ok (after, (toadd :: rows))
                )
            )
        )
    and collect state' numerals =  
        (
            match (fst state').curr with
            | Some { tokn; _ } -> 
                (match tokn with
                    | TFloat value ->  
                        collect (advance state') (value :: numerals)
                    | TNumeral value ->  
                        collect (advance state') ((float_of_int value) :: numerals)
                    | TComma -> 
                        collect (advance state') (numerals)
                    | TLeftBracket -> 
                        (* collect remaining rows *)
                        (>>==) (stack (advance state') []) (fun (fin, blk) -> 
                            Ok (fin, Collect (List.rev blk))
                        )
                    | TRightBracket -> 
                        Ok ((advance state'), (Raw (List.rev numerals)))
                    | _ ->
                        Error "Unexpected token in static array - only floats supported"
                ) 
            | _ -> Error "Unexpected close - need static array"
        )
    in (collect state [])
;;

let parse_param_data _start next = 
    (if check TRange (fst next) then (
        let next' = advance next in 
        match (fst next').curr with 
        | Some { tokn; _ } ->  
            (match tokn with
                | TAlphaNum _end ->  
                    (>>==) (as_cell _start) (fun scell -> 
                        (>>==) (as_cell _end) (fun ecell -> 
                            Ok (advance next', Range (scell, ecell))
                        )
                    )
                | _ -> 
                    Error "Expected range end"
            )
        | _            -> 
            Error "Unclosed range"
    ) else (
            (* no range token  - maybe single cell*)
            (>>==) (as_cell _start) (fun y -> 
                Ok (next,  Scalar y)
            )
        )
    )
;;

let compass tokn motn = 
    match tokn with 
    | TLeftAngle  ->
        (West motn)
    | TRightAngle ->
        (East motn)
    | TCaret      ->
        (North motn)
    | TUnderscore -> 
        (South motn)
    | _ -> 
        failwith "invalid token in compass"
;;

let parse_reference state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (match tokn with 
            | TAlphaNum "self" -> 
                Ok (advance state, Refer Self)
            | _ -> 
                Error "Unhandled reference"
        )
    | _ -> 
        Error "Unfinished reference"
;;

let rec parse_relative dir state' = 
    (match (fst state').curr with
        | Some { tokn; _ } -> 
            (match tokn with 
                | TNumeral  _a -> 
                    let motn = _a in
                    (>>==) (parse_ein_params (advance state')) (fun (final, range) ->
                        Ok (final, Relative ((compass dir motn), range))
                    )
                | _ -> 
                    let motn = 1 in
                    (>>==) (parse_ein_params state') (fun (final, range) ->
                        Ok (final, Relative ((compass dir motn), range))
                    )
            )
        | _ -> 
            Error "Expected optional motion with cell spec but no tokens left"
    )

and parse_ein_params state = 
    match (fst state).curr with
    | Some { tokn; _ } -> 
        (match tokn with
            | TAlphaNum _start ->  
                (if validate _start then
                    let next = advance state in
                    (parse_param_data _start next)
                    else (Error "Invalid range value")
                )
            | TLeftBracket -> 
                (>>==) (parse_static_array (advance state)) (fun (x,y) ->  
                    Ok (x, NdArray y)
                )
            | TLeftAngle | TRightAngle | TUnderscore | TCaret -> 
                (parse_relative tokn (advance state))
            | TAt -> 
                (parse_reference (advance state))
            | _   ->  
                (* ?? -- should be unreachable *)
                Error "Trailing Comma!"
        )
    | _ -> Error "Missing einsum parameters"
;;


(* let the call order reflect how it written for einsum parameters *)
let call_order (prt, _rem) = 
    let (e, p) = List.hd prt.prog in
    ({ prt with prog=((e, (List.rev p)) :: (List.tl prt.prog)) }, _rem)
;;

let parse_formulae state = 
    let rec _extract state =
        (if check TComma (fst state) 
            then ((>>==) (parse_ein_params (advance state)) (Fun.compose _extract add_crange)) 
            else (Ok (call_order state))
        );
    in (>>==) (parse_einsum state) (_extract)
    (*in (parse_einsum state)*)
;;

(* TODO: make errors some easily parseable and serializable type showing expected and current states *)
let parse lstream = 
    let rec _parse_lxms current = 
        (if check TLeftParen (fst current) then
            let next = advance current in 
            (if check TRightParen (fst next) then 
                Error (Format.sprintf "expected einsum expression at %s" (show_prattstate (fst next)))
                else
                    ((>>==) (parse_formulae next) (fun state' -> 
                        (if check TRightParen (fst state') then
                            _parse_lxms (advance state')
                            else
                                Error (Format.sprintf "Unclosed einsum formulae: %s" (show_prattstate (fst state')))
                        )
                    )) 
            ) else (Ok current)
        )
    in
    _parse_lxms @@ advance (prattempty, lstream)
;;
