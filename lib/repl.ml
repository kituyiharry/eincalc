(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

let handle_eval grid (t) = 
   let fs = Parser.show_program t.Emitter.ast in
   (* build an execution graph *)
   let _ = Format.printf "\n%s\n" (fs) in
   let _ = 
       Emitter.convert t
       |> Eval.mkvm grid 
       |> Eval.eval
   in ()
;;

let handle_transform_formulae grid form = 
    (match Eval.tosource grid form with 
    | Ok    t -> handle_eval grid t
    | Error e -> grid.onlog (Format.sprintf "Error: %s\n" e, Ndcontroller.Error)
    )
;;

let handle_parse_exp grid src (lex: Lexer.lexeme list) = 
    (
        Parser.parse lex src 
        |> (function 
            | Ok ({ Parser.prog; _ }, _lefttoks) -> (
                handle_transform_formulae grid prog
            )
            | Error s   -> 
                grid.onlog ((Format.sprintf "Parse Error: %s\n" s, Ndcontroller.Error))
        )
    )
;;


let simple_parse_exp _grid src (lex: Lexer.lexeme list) = 
    (
        Parser.parse lex src
        |> (function 
            | Ok ({ Parser.prog; _ }, _lefttoks) -> (
                Ok prog
            )
            | Error s   -> 
                Error s
        )
    )
;;


let simple_scan_exp grid (_exp: string) = 
    (
        Lexer.runall _exp
        |> (function 
            | Ok _res -> 
                (simple_parse_exp grid _exp _res) 
            | Error (l,c,s) ->  
                Error (Format.sprintf "%d %d: %s" l c s)
        )
    )
;;

let handle_scan_exp grid (_exp: string) = 
    (
        Lexer.runall _exp
        |> (function 
            | Ok _res -> handle_parse_exp grid _exp _res
            | Error (l,c,s) ->  
                grid.onlog ((Format.sprintf "Scan Error: l:%d c:%d %s" l c s, Ndcontroller.Error))
        )
    )
;;

let mkbuf s = (let b = Buffer.create 64 in let _ = Buffer.add_string b s in b) ;;

(* handles input -> return bool on whether to continue *)
let handle_input grid (data: Buffer.t) = 
    let l = Buffer.length data in
    (if l > 0 then
        let o = Buffer.to_bytes data |> Bytes.trim |> Bytes.to_string in
        (match String.unsafe_get o 0 with 
            | '!' -> (
                (match String.get o 1 with 
                    | 'q' | 'Q' ->  let _ = Format.printf "Goodbye :-)\n" in false
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

let repl (grid: Ndcontroller.gridcontroller) () = 
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

