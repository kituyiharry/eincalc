(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
let handle_transform_formulae grid form = 
    match Eval.tosource grid form with 
    | Ok    t -> 
        (*let _ = Format.printf "\n%s\n" (Emitter.show_presource t) in*)
        let _ = 
            Emitter.convert t
            |> Eval.mkvm grid 
            |> Eval.eval
        in 
        ()
    | Error e -> Format.printf "Error: %s\n" e
;;

let handle_parse_exp grid (lex: Lexer.lexeme list) = 
    (
        Parser.parse lex 
        |> (function 
            | Ok ({ Parser.prog; _ }, _lefttoks) -> (
                let lem = List.length _lefttoks in
                if lem > 0 then
                    (*let _ = Format.printf "Tree: %s with rem %d\n" (Parser.show_program prog) lem in *)
                    handle_transform_formulae grid prog
                else
                    (*let _ = Format.printf "Tree: %s\n" (Parser.show_program prog) in *)
                    handle_transform_formulae grid prog
            )
            | Error s   -> Format.printf "Parse Error: %s\n" s
        )
    )
;;

let handle_scan_exp grid (_exp: string) = 
    (
        Lexer.runall _exp
        |> (function 
            | Ok _res ->        handle_parse_exp grid _res
            | Error (l,c,s) ->  Format.printf "Error: l:%d c:%d %s" l c s
        )
    )
;;

let mkbuf s = (let b = Buffer.create 64 in let _ = Buffer.add_string b s in b) ;;

(* handles input -> return bool on whether to continue *)
let handle_input (grid: Ndmodel.spinmodel Ndmodel.Grid.t) (data: Buffer.t) = 
    let l = Buffer.length data in
    (if l > 0 then
        let o = Buffer.to_bytes data |> Bytes.trim |> Bytes.to_string in
        (match String.get o 0 with 
            | '!' -> (
                (match String.get o 1 with 
                    | 'q' ->  let _ = Format.printf "Goodbye :-)\n" in false
                    | _   ->  true
                )
            )
            | '=' -> (
                let _ = handle_scan_exp grid (String.sub o 1 (l-1)) in 
                let _ = Buffer.clear data in
                true
            ) 
            |  _  -> (
                let _ = Format.printf "%s" o   in
                let _ = Buffer.clear data      in
                true
            )
        )
    else
        true)
;;

let repl (grid: Ndmodel.spinmodel Ndmodel.Grid.t) () = 
    let buf = Buffer.create 1024 in 
    let rec input_formula bufc = 
        let l = Buffer.length bufc in
        let _ = if l == 0 then Format.printf ">>> %!" else () in
        let _ =
            Seq.of_dispenser (fun () -> In_channel.input_char In_channel.stdin)
            |> Seq.take_while ((!=)'\n')
            |> Seq.iter (Buffer.add_char buf) 
        in 
        if handle_input grid bufc then input_formula bufc else ()
    in 
    input_formula buf
;;

