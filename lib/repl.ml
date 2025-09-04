let handle_parse_exp (lex: Lexer.lexeme list) = 
    (
        Parser.parse lex 
        |> (function 
            | Ok ({ Parser.prog; _ }, _l) -> (
                let lem = List.length _l in
                if lem > 0 then
                    Format.printf "Tree: %s with rem %d\n" (Parser.show_program prog) lem
                else
                    Format.printf "Tree: %s\n" (Parser.show_program prog)
            )
            | Error s   -> Format.printf "Parse Error: %s" s
        )
    )
;;

let handle_scan_exp (_exp: string) = 
    (*let ret = ref true in*)
    let _ = Format.printf "Expr: %s\n" _exp in
    (
        Lexer.runall _exp
        |> (function 
            | Ok _res ->        handle_parse_exp _res
            | Error (l,c,s) ->  Format.printf "Error: l:%d c:%d %s" l c s
        )
    )
;;

(* handles input -> return bool on whether to continue *)
let handle_input (data: Buffer.t) = 
    let l = Buffer.length data in
    (if l > 0 then
        let o = Buffer.contents data |> String.trim  in
        (match String.get o 0 with 
            | '!' -> (
                (match String.get o 1 with 
                    | 'q' ->  let _ = Format.printf "Goodbye :-)\n" in false
                    | _   ->  true
                )
            )
            | '=' -> (
                let _ = handle_scan_exp (String.sub o 1 (l-1)) in 
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

let repl () = 
    let buf = Buffer.create 1024 in 
    let rec input_formula bufc = 
        let l = Buffer.length bufc in
        let _ = if l == 0 then Format.printf "\n>>> %!" else () in
        let _ =
            Seq.of_dispenser (fun () -> In_channel.input_char In_channel.stdin)
            |> Seq.take_while ((!=)'\n')
            |> Seq.iter (Buffer.add_char buf) 
        in 
        if handle_input bufc then input_formula bufc else ()
    in 
    input_formula buf
;;

