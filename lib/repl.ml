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

let scan_and_notify sheet vstr = 
   (*let _ = Format.printf "scan_and_not: %s\n" vstr in*)
   (*let buf = Buffer.create 256 in*)
   match simple_scan_exp sheet vstr with  
   | Ok cell ->
       (* transform the ast to capture any reads and writes so we can
          form dependencies *)
       (match Eval.tosource sheet cell with
           | Ok ({ ast=(Parser.Stmt s); _ } as cell) ->
               let _ = Ndcontroller.dependants sheet cell.ast in
               let _ = handle_eval sheet cell in
               let dotc = Ndcontroller.plaindctx () in 
               (* see if we wrote over other formulaes *)
               let _ = (
                   List.iter (fun (msk, shp) ->
                       Ndcontroller.notify sheet msk shp
                       |> List.map (Ndcontroller.affected sheet dotc)
                       |> List.concat
                       |> List.sort_uniq (fun (Parser.Stmt x) (Parser.Stmt y) ->
                           Float.compare x.stamp y.stamp
                       )
                       |> List.iter (fun fml -> 
                           handle_transform_formulae sheet fml
                       )
                   ) s.writes
               ) in
               (*let _ = Buffer.clear buf in*)
               (*let _ = Ndcontroller.FormGraphSerializer.to_dot ~dir:true "Affected" dotc.global dotc.prnode dotc.predge *)
                   (*!(sheet.frmgrph) |> Seq.concat |> Seq.iter (fun s -> *)
                       (*Buffer.add_string buf (s ())*)
                   (*) in *)
               (*let s = Buffer.contents buf in*)
               (*let _ = sheet.onlog (Format.sprintf "%s" s, Ndcontroller.Warn) in*)
                ()
           | Error s -> 
               sheet.onlog (s, Ndcontroller.Error);
                ()
       )
    | Error e -> 
        (failwith ("ParseError: %s" ^ e))
;;

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
                let _ = scan_and_notify grid (String.sub o 1 (l-1)) in 
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

