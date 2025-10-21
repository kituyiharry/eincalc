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
    (* TODO: Does disjoint set make sense ?? *)
    | Shape of (char * int) list 
and referral = 
    | Self  (* the current cell *)
and call = 
    | Diag  of float * int  (* square matrix with diagonal values *) 
    | Zeros of int list     (* zero init with a shape  *)
    | Ones  of int list     (* ones init with a shape  *)
    | Fill  of float * int list (* fill shape with a certain value *)
    | Enum  of float * float * int list (* enumerate from minvalue and increment with a shape *)
    | Rand  of float * int list (* random with bound and shape *)
    | Alt   of float list * int list (* alternate of values *)
and mask = 
    (* TODO: support axis values for direction of application - default means
       flattening and applying over whole  *)
    (* TODO: robust scaling and unit vector norm *)
    | MinMax of float * float     (* min max between a pair of values *)
    | ZScore                      (* z-score normalization *)
    | Mean
    | Mode
    | Stddev
    | Reshape of int list
    | Write   of cell             (* executes an effect to the grid *)           
    (*| Unbox                     (* undo top dimension maybe by running a function over it ?? *) *)
    (*| Partition                 (* break into groups *) *)
    (*| Determ                    (* determinant *) *)
    (*| Rescale                   (* values add up to a certain num *)*)
    (*| Cumulative*)
    (*| Map *)
    (*| Reduce *)
and  cell    = string * int      (* Rows are strings, Columns are numbers *)
and  dimnsn  = lit               (* literal and its index *)
and  motion  =  
    | North of int (* ^ *)
    | South of int (* _ *)
    | East  of int (* > *)
    | West  of int (* < *)
and 'a ndarray = 
    | Itemize of 'a list          (* The columns *)
    | Collect of 'a ndarray list  (* The rows    *)
and  crange  = 
    | Range    of cell * cell    (* spreadsheet cell *)
    | Scalar   of cell
    | NdArray  of float ndarray
    | Relative of motion * crange(* Relative cell - Up ^, Down _, Left <, Right, > *)
    | Refer    of referral       (* a way to refer to the current cell *) 
    | Create   of call 
    | Mask     of crange * mask list
    | Void
and  params  = crange list       (* function parameters *)
and  einsum  = { 
        inp: dimnsn list         (* input  - at least one - likely in reverse order. should correspond to the number of params *)
    ;   out: dimnsn option       (* output - can be empty *)
}
and expr     = 
    | Ein   of (einsum * params) (* formula specification *)
(* TODO: Blocks can only have 1 inner expression and then enclose other blocks or expressions 
   We want to discourage long blocks.
*)
and  formula = 
    | Block of (expr * formula)
    | Stmt  of expr
and  program = formula
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

let einempty = { 
        inp = []
    ;   out = None
    ;
};;


let prattempty = {
    curr = None; prev = None; prog = Stmt (Ein (einempty, []))
};;

type parseres = (program, string) result
type parsefn  = (prattstate -> parseres)

type prattrule = {
        prefix: parsefn option
    ;   infix:  parsefn option
    ;   prec:   precedence
};;

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

(*
 *let getrule tok = 
 *    rules.(ruleidx tok)
 *;;
*)

(* check current token without advancing state *)
let past tokn ({prev; _}) = 
    match prev with
    | None -> false 
    | Some { tokn=tok; _ } -> (equal_ttype tok tokn)
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

let parse_einsum pratt = 
    let rec _parse ein state = 
        (match (fst state).curr with
            | Some ({ tokn; _ }) -> 
                (match tokn with 
                    | TAlphaNum v -> 
                        (if validate v then (
                            let (prt, rem') as state' = advance state in
                            if check TComma prt then
                                _parse (parse_ein_inp ein v) (advance state') 
                            else 
                                (* no params *)
                                Ok (prt, ((reorder @@ parse_ein_inp ein v), []), rem')
                        ) else (Error (Format.sprintf "Input indices invalid - please use at least one ascii chars at %s" (show_prattstate @@ fst state))))
                    | _ -> 
                        let prt', rem' = state in
                        Ok (prt', ((reorder ein), []), rem')
                        (*Error (Format.sprintf "Unimplemented at %s" (show_prattstate (fst state)))*)
                )
            | _ -> 
                Error (Format.sprintf "Unfinished einsum expression at %s" (show_prattstate (fst state)))
        )
    in
    (>>==) (_parse einempty pratt) (fun (current, ein, lxm) -> 
        (* output of the einsum - NOT the parameters! *)
        (if check TArrow (current) then (
            let next = advance (current, lxm) in 
            (match (fst next).curr with 
                | Some ({ tokn; _ }) -> 
                    (match tokn with
                        | TAlphaNum v -> 
                            (* Grab the last einsum and update the output *)
                            (*let ein' = (fst next).prog in*)
                            let upd  = parse_ein_out (fst ein) v in
                            Ok ((advance next), (upd, snd ein))
                        | _ -> 
                            (* it was not a token - likely something closing like a Comma or RightParen *)
                            (* not what we expected - we just leave as is -  *)
                            Ok (next, ein)
                    )
                | _ -> 
                    Error "Unexpected einsum result"
            )
        ) else (
            (* arrow can be optional *)
            Ok ((current, lxm), ein) 
        ))
    )
;;

let parse_einsum_expr pratt = 
    (>>==) (parse_einsum pratt) (fun (state, ein) -> 
        let p = fst state in 
        let t = snd state in 
        Ok ({ p with prog=(Stmt (Ein (fst ein, snd ein))) }, t)
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
                        (if past TComma (fst state') then
                            Error "Missing value between commas!"
                        else
                            collect (advance state') (numerals))
                    | TLeftBracket -> 
                        (* collect remaining rows *)
                        (>>==) (stack (advance state') []) (fun (fin, blk) -> 
                            Ok (fin, Collect (List.rev blk))
                        )
                    | TRightBracket -> 
                        Ok ((advance state'), (Itemize (List.rev numerals)))
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


let parse_extract_slice state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (
            match tokn with 
            | TNumeral v ->  
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) ((float_of_int n) :: p)
                                | TFloat f -> 
                                    collect_rem (next) ((f) :: p)
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (advance next, (List.rev p))
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ (float_of_int v) ]
            | TFloat v   ->  
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) ((float_of_int n) :: p)
                                | TFloat f -> 
                                    collect_rem (next) ((f) :: p)
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (advance next, (List.rev p))
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | _ -> 
                (Error "shapes can only be natural number (>= 0)")
        )
    | _ -> 
        (Error "expected number in extraction")
;;

let parse_extract_shape state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (
            match tokn with 
            | TNumeral v ->  
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) (n :: p)
                                | TFloat _f -> 
                                    Error ("only numerals allowed in shape")
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (advance next, (List.rev p))
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | _ -> 
                (Error "shapes can only be natural number (>= 0)")
        )
    | _ -> 
        (Error "expected number in extraction")
;;

let consume state tt  =
    (match (fst state).curr with
        | Some { tokn; _ } ->
            (if equal_ttype tokn tt then 
                (Ok (advance state)) 
             else
                (Error (Format.sprintf "Expected consume %s found %s" (show_ttype tt) (show_ttype tokn)))
            )
        | _ -> Error "failed consumption check"
    )
;;

let parse_ref_angle_var state = 
    (match (fst state).curr with 
        | Some  {tokn; _} ->
            Ok (advance state, tokn)
        | _ -> 
            (Error "Expected angle variable!")
    )
;;

(* TODO: support range syntax e.g 0..10 *)
let parse_enum_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (parse_ref_angle_var (advance next)) (fun (next', tok)-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (>>==) (parse_ref_angle_var (after)) (fun (next', tok)-> 
                                    (match tok with
                                        |TFloat incv ->  
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum (fval, incv, shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        |TNumeral incn -> 
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum (fval, (float_of_int incn) ,shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        | s ->
                                            Error (Format.sprintf "Expected numeral value, found %s" (show_ttype s))
                                    )
                                )
                            )
                        | TNumeral ival -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (>>==) (parse_ref_angle_var (after)) (fun (next', tok)-> 
                                    (match tok with
                                        |TFloat incv ->  
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum ((float_of_int ival), incv, shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        |TNumeral incn -> 
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum ((float_of_int ival), (float_of_int incn) ,shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        | s ->
                                            Error (Format.sprintf "Expected numeral value, found %s" (show_ttype s))
                                    )
                                )
                            )

                                    | _ -> 
                                        Error "Expected fill value as float"
                                )
                        | _ -> 
                            Error ("Expected shape spec in angle brackets")
                        )
                    | None -> 
                        Error ("Expected shape spec")
                )
;;

let parse_rand_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (parse_ref_angle_var (advance next)) (fun (next', tok)-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Rand (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape filling spec"
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final,
                                                            Create (Rand (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error "Expected shape spec"
                                        )
                                    | _ ->
                                        Error "Expected shape fill spec"
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_diag_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (parse_ref_angle_var (advance next)) (fun (next', tok)-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TNumeral shp -> 
                                                (>>==) (consume (advance after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, shp)))
                                                )
                                            | TFloat shp -> 
                                                (>>==) (consume (advance after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, (int_of_float shp))))
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected diagonal size spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape filling spec"
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TNumeral shp -> 
                                                (>>==) (consume (advance after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, shp)))
                                                )
                                            | TFloat shp -> 
                                                (>>==) (consume (advance after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, (int_of_float shp))))
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected diagonal size spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape fill spec"
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_fill_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (parse_ref_angle_var (advance next)) (fun (next', tok)-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Fill (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape filling spec"
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Fill (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error "Expected shape spec"
                                        )
                                    | _ ->
                                        Error "Expected shape fill spec"
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_ones_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                        (>>==) (consume after' TRightAngle) (fun final -> 
                                            Ok (final, Create (Ones shp))
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_zeros_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                        (>>==) (consume after' TRightAngle) (fun final -> 
                                            Ok (final, Create (Ones shp))
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_alt_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_slice (advance after)) (fun (after', slc) -> 
                                        (>>==) (consume after' TComma) (fun final -> 
                                            (>>==) (parse_extract_shape (advance final)) (fun (after', shp) -> 
                                                (>>==) (consume after' TRightAngle) (fun final -> 
                                                    Ok (final, Create (Alt (slc, shp)))
                                                ) 
                                            )
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_reference state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (match tokn with 
            | TAlphaNum "self" -> 
                Ok (advance state, Refer Self)
            (* TODO: for zeros and ones - reuse definition of fill! *)
            | TAlphaNum "ones" -> 
                parse_ones_reference state
            | TAlphaNum "zeros" -> 
                parse_zeros_reference state
            | TAlphaNum "fill" -> 
                parse_fill_reference state
            | TAlphaNum "rand" -> 
                parse_rand_reference state
            | TAlphaNum "enum" -> 
                parse_enum_reference state
            | TAlphaNum "eye" | TAlphaNum "diag" -> 
                parse_diag_reference state
            | TAlphaNum "alt" -> 
                parse_alt_reference state
            | TAlphaNum _start -> 
                (if validate _start then
                    let next = advance state in
                    (parse_param_data _start next)
                    else (Error "Invalid range value")
                )
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
            (* TODO: single scalar values ?? *)
            | TLeftBracket -> 
                (>>==) (parse_static_array (advance state)) (fun (x,y) ->  
                    Ok (x, NdArray y)
                )
            | TLeftAngle | TRightAngle | TUnderscore | TCaret -> 
                (parse_relative tokn (advance state))
            | TRightParen ->
                Ok (state, Void)
            | TAt -> 
                (parse_reference (advance state))
            | TComma   ->  
                (* NB: Trailing commas will add a Void *)
                Ok (state, Void)
            | TFloat fval -> 
                Ok (advance state, NdArray (Itemize ([ fval ])))
            | TNumeral ival -> 
                Ok (advance state, NdArray (Itemize ([ float_of_int ival ])))
            | _ -> 
                Error "Bad token"
        )
    | _ -> Error "Missing einsum parameters"
;;

let parse_num state = 

    (match (fst state).curr with 
        | Some { tokn; _ } -> 
            (match tokn with 
            | TNumeral _ | TFloat _ -> 
                (Ok ((advance state), tokn))
            | _ ->
                Error ("expected number")
            )
        | None ->
            Error "expected number token"
    )
;;

let parse_ein_mask state = 
    (* TODO: support arbitrary expression operations
     *| Map 
     *| Reduce
     *| Rescale                    (* values add up to a certain num *)*
     *)
    let rec masklist state lst = 
        (match (fst state).curr with 
            | Some { tokn; _ } -> 
                (>>==) (match tokn with 
                    (* maps to same shape *)
                    | TAlphaNum "zscore" -> 
                        Ok (advance state, ZScore)
                    | TAlphaNum "write" -> 
                        let nxt = advance state in
                        if check TLeftAngle (fst nxt) then
                            let nxt' = advance nxt in 
                            (match (fst nxt').curr with 
                            | Some  { tokn; _ } -> 
                                (match tokn with 
                                    | TAlphaNum _end ->  
                                        (>>==) (as_cell _end) (fun ecell -> 
                                            (>>==) (consume (advance nxt') TRightAngle) (fun final -> 
                                                Ok (final, (Write ecell))
                                            )
                                        )
                                    | _ -> 
                                        Error "Expected range end"
                                ) 
                            | None -> 
                                Error "missing cell argument"
                            )
                        else
                            Error "expected write cell argument in angle brackets"
                    | TAlphaNum "minmax" ->
                        let nxt = advance state in
                        if check (TLeftAngle) (fst nxt) then
                            (>>==) (parse_num (advance nxt)) (fun (after, num1) -> 
                                (>>==) (consume  after TComma) (fun nxt -> 
                                    (>>==) (parse_num nxt) (fun (after', num2) -> 
                                        (match (num1, num2) with 
                                        | (TFloat vala, TFloat valb) -> 
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok ((after'), MinMax(vala, valb))
                                            )
                                        | (TNumeral vala, TNumeral valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax((float_of_int vala), (float_of_int valb)))
                                            )
                                        | (TFloat vala, TNumeral valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax(vala, (float_of_int valb)))
                                            )
                                        | (TNumeral vala, TFloat valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax((float_of_int vala), valb))
                                            )
                                        | _ -> 
                                            Error "unreachable condition!!"
                                        )
                                    )
                                )
                            )
                        else
                            Ok (advance state, MinMax(-1., 1.))
                    | TAlphaNum "reshape" ->
                        let nxt = advance state in
                        if check (TLeftAngle) (fst nxt) then
                            let nxt' = advance nxt in
                            if check TLeftBracket (fst nxt') then 
                                (>>==) (parse_extract_shape (advance nxt')) (fun (after, shp) -> 
                                    (>>==) (consume after TRightAngle) (fun after' ->
                                        Ok (after', Reshape shp)
                                    )
                                ) 
                            else
                                Error "reshape value should be in shape format"
                        else
                            Error "missing reshape values!"
                    (* reductions to Scalar *)
                    | TAlphaNum "mean" ->
                        Ok (advance state, Mean)
                    | TAlphaNum "stddev" ->
                        Ok (advance state, Stddev)
                    | TAlphaNum "mode" ->
                        Ok (advance state, Mode)
                    | t -> 
                        Error (Format.sprintf "Unknown mask function: %s" (show_ttype t))
                ) (fun (state', mask) -> 
                    if check TPipe (fst state') then
                        (masklist (advance state') (mask :: lst))
                    else
                        Ok (state', List.rev (mask :: lst))
                )
            | None -> 
                Error "expected mask function"
        ) in 
    masklist state []
;;

let parse_einsum_formulae state = 
    let rec _extract (state, (ein, par)) =
        (if check TComma (fst state) then 
                ((>>==) (parse_ein_params (advance state)) (fun (next, rnge) -> 
                  (* check for masks which are piped to parameters *)
                    if check TPipe (fst next) then
                        (>>==) (parse_ein_mask (advance next)) (fun (next', ml) -> 
                            _extract (next', (ein, (Mask (rnge, ml)) :: par))
                        )
                    else
                        _extract (next, (ein, rnge :: par))
                ))
                (*(Fun.compose _extract add_crange)) *)
            (* a right paren shows the end of parameter sequence - dont advance in this case *)
            else if not @@ check TRightParen (fst state) then 
                ((>>==) (parse_ein_params (state)) (fun (next, rnge) -> 
                    (* let the call order reflect how it written for einsum parameters *)
                    _extract (next, (ein, rnge :: par))
                ))
            else (Ok (state, (ein, (List.rev par))))
        );
    in (>>==) (parse_einsum state) (_extract)
;;

let parse_formulae state = 
    (* will overwrite any previous data *)
    (>>==) (parse_einsum_formulae state) (fun (x, y) -> 
        let p = fst x in 
        let t = snd x in
        Ok ({ p with prog=(Stmt (Ein y)) }, t)
    )
;;

(* TODO: make errors some easily parseable and serializable type showing expected and current states *)
let parse lstream = 
    let rec _parse_lxms current = 
        (if check TLeftParen (fst current) then
            (let next = advance current in 
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
                )
            ) 
            else (Ok current)
        )
    in
    _parse_lxms @@ advance (prattempty, lstream)
;;
