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

(* TODO: Maybe use array instead of list *)

type lexeme = {
        tokn: ttype 
    ;   line: int 
    ;   colm: int
} [@@deriving show, eq]
;;

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

let scan_token (line, colm, sstr) index rest =
    match sstr with
    | '('  -> Ok ((mktok line colm TLeftParen),  index, rest)
    | ')'  -> Ok ((mktok line colm TRightParen), index, rest)
    | ','  -> Ok ((mktok line colm TComma),      index, rest)
    | '"'  -> Ok ((mktok line colm TQuote),      index, rest)
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
                Error (line, colm, Format.sprintf "expected einsum arrow function '->'")
        )
    |  chr -> (
        let dbuf = Buffer.create 8 in
        let () = Buffer.add_char dbuf chr in
        let drp = ref 0 in
        if isDigit chr then 
            let _ = List.take_while (fun c -> 
                let isd = isDigit c in 
                if isd then (
                    incr drp;
                    Buffer.add_char dbuf c 
                );
                isd
            ) rest in
            let alp = Buffer.contents dbuf |> int_of_string in
            Ok ((mktok line colm (TNumeral alp)), index + !drp, (List.drop !drp rest))
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
