(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized
 *   redistribution of this app without express, written permission from our legal
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Tokens;;

let (>>==) = Result.bind;;

type lexeme = {
        tokn: ttype
    ;   line: int
    ;   colm: int
} [@@deriving show, eq]
;;

(* TODO: track this along with other errors that may happen further down the
   parser and execution stack *)
let mktok line colm tokn = { tokn;  line; colm }
;;

type lexres = (lexeme, int * int * string) result
;;

let isDigit c =
    c >= '0' && c <= '9'
;;

let isAlpha c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z')
;;

let isAlphaNum c =
    isDigit c || isAlpha c
;;

let dbuf = Buffer.create 8;;

let scan_token (line, colm, sstr) index rest =
    match sstr with
    | '('  -> Ok ((mktok line colm TLeftParen),   index, rest)
    | ')'  -> Ok ((mktok line colm TRightParen),  index, rest)
    | ','  -> Ok ((mktok line colm TComma),       index, rest)
    | '"'  -> Ok ((mktok line colm TQuote),       index, rest)
    | '['  -> Ok ((mktok line colm TLeftBracket), index, rest)
    | ']'  -> Ok ((mktok line colm TRightBracket),index, rest)
    | '<'  -> Ok ((mktok line colm TLeftAngle),   index, rest)
    | '>'  -> Ok ((mktok line colm TRightAngle),  index, rest)
    | '^'  -> Ok ((mktok line colm TCaret),       index, rest)
    | '_'  -> Ok ((mktok line colm TUnderscore),  index, rest)
    | '@'  -> Ok ((mktok line colm TAt),          index, rest)
    | '|'  -> Ok ((mktok line colm TPipe),        index, rest)
    | ':'  -> Ok ((mktok line colm TColon),       index, rest)
    | '{'  -> Ok ((mktok line colm TOpenCurly),   index, rest)
    | '}'  -> Ok ((mktok line colm TCloseCurly),  index, rest)
    | '='  -> Ok ((mktok line colm TEq),          index, rest)

    | '*'  -> Ok ((mktok line colm KMult),        index, rest)
    | '/'  -> Ok ((mktok line colm KDiv),         index, rest)
    | '+'  -> Ok ((mktok line colm KPlus),        index, rest)

    | '.'  ->
        (match rest with
            |  ('.') :: rest' ->
                Ok ((mktok line colm TRange), (index+1), rest')
            | _ ->
                Error (line, colm, Format.sprintf "expected range '..'")
        )
    | '-'  -> (match rest with
            | ('>') :: rest' ->
                Ok ((mktok line colm TArrow), index+1, rest')
            | _ ->
                Ok ((mktok line colm KMinus),       index, rest)
        )
    | '\'' -> 
        (*let dbuf = Buffer.create 8 in*)
        let _ = Buffer.clear dbuf in
        let drp  = ref 0 in
        let _ = List.drop_while (fun c ->
            let isd = not @@ Char.equal '\'' c in
            if isd then (
                incr drp;
                Buffer.add_char dbuf c
            );
            isd
        ) rest in
        let alp = Buffer.contents dbuf in
        Ok ((mktok line colm (TAlphaNum alp)), (index + !drp), (List.drop (!drp+1) rest))

    |  chr -> (
        let _ = Buffer.clear dbuf in
        (*let dbuf = Buffer.create 8 in*)
        let () = Buffer.add_char dbuf chr in
        let drp = ref 0 in
        if isDigit chr then
            let rem = List.drop_while (fun c ->
                let isd = isDigit c in
                if isd then (
                    incr drp;
                    Buffer.add_char dbuf c
                );
                isd
            ) rest in
            (match rem with
                | []  ->
                    let alp = Buffer.contents dbuf |> int_of_string in
                    Ok ((mktok line colm (TNumeral alp)), index + !drp, rem)
                | hd :: rest -> (
                    (* floating point *)
                    if hd == '.' then (
                        Buffer.add_char dbuf hd;
                        let rem = List.drop_while (fun c ->
                            let isd = isDigit c in
                            if isd then (
                                incr drp;
                                Buffer.add_char dbuf c
                            );
                            isd
                        ) rest in
                        let alp = Buffer.contents dbuf |> float_of_string in
                        Ok ((mktok line colm (TFloat alp)), index + !drp, rem)
                    ) else (
                        let alp = Buffer.contents dbuf |> int_of_string in
                        Ok ((mktok line colm (TNumeral alp)), index + !drp, rem)
                    )
                )
            )
        else if isAlphaNum chr then
            let _ = List.take_while (fun c ->
                let isd = isAlphaNum c in
                if isd then (
                    incr drp;
                    Buffer.add_char dbuf c;
                );
                isd
            ) rest in
            let alp = Buffer.contents dbuf in
            Ok ((mktok line colm (TAlphaNum alp)), (index + !drp), (List.drop !drp rest))
        else
            Error (line, colm, Format.sprintf "unexpected token %c" chr)
    )
;;

let (let*) = Result.bind
;;

(* run on a single line *)
let run line pstring =
    let rec _tokenize index tseq pstate =
        match pstate with
        | [] ->
            Ok (tseq)
        | ' ' :: rest ->
            _tokenize (index + 1)  (tseq) rest
        | hd :: rest ->
            let* (tok, index', rest') = scan_token (line, index, hd) index rest in
            _tokenize (index' + 1) (tok :: tseq) rest'
    in
    pstring
    |> String.to_seq
    |> List.of_seq
    |> _tokenize 0 []
    |> (function
            | Ok l -> Ok (List.rev l)
            | e    -> e
    )
;;

(* run on multiple lines *)
let runall pstring =
    String.split_on_char '\n' pstring
    |> List.mapi (run)
    |> (fun res ->
        if List.exists (Result.is_error) res then
            List.find (Result.is_error) res
        else
            Ok (List.concat @@ List.map (Result.get_ok) res)
    )
;;
