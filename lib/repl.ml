(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

let _DEBUG = ref false;;
let _OPTS  = ref true ;;

let handle_eval grid (t) = 
   let fs = Parser.show_program t.Emitter.ast in
   (* build an execution graph *)
   let _ = Format.printf "\n%s\n" (fs) in
   let _ = 
       Emitter.convert t
       |> Eval.mkvm grid !(_DEBUG) 
       |> Eval.eval
   in ()
;;

let handle_transform_formulae grid (form) = 
    let rec rununiquestamp grid (Parser.Stmt ctx as form) stamps  =

        (* Skip re-execution - simple case *)
        if List.exists (
            (fun (act', stamp') -> 
                Float.equal ctx.stamp stamp' 
                &&
                String.equal act' !(grid.Ndcontroller.active)
            )
        ) stamps then stamps else

        (match Eval.tosource grid form !_OPTS with 
        | Ok    t ->
                let _ = handle_eval grid t in 
                let oldactive = !(grid.active) in
                let stamps =  (!(grid.active),ctx.stamp) :: stamps in
                let stamps = List.fold_left (fun stamps -> function 
                    (* WARN: handle cross sheet writes! -> can cause an infinite loop if not carefull - added stamp list but not properly tested !! *)
                    (* TODO: use stamps to ensure idempotent runts here! *)
                    | ((Parser.WriteTo (_indx, _) as msk), shp)-> 
                        (
                            Ndcontroller.notify grid msk shp
                            |> List.map (
                                fun (index,y) ->
                                    let act = Ndcontroller.IndexToSheet.find grid.indexed index in
                                    let _ = (grid.active := act) in
                                    List.map (fun x -> (act, x)) (Ndcontroller.affected grid (Ndcontroller.plaindctx ()) y)
                            )
                            |> List.concat
                            |> List.sort_uniq (fun (_, (Parser.Stmt x)) (_, (Parser.Stmt y)) -> Float.compare x.stamp y.stamp)
                            |> List.fold_left (fun stamps (act, ((Parser.Stmt c) as y)) ->
                                (* Skip re-execution - simple case *)
                                let _ = List.iter (fun (x,y) -> 
                                    Format.printf "%s-%f" x y
                                ) stamps in
                                let _ = Format.print_newline ( ) in
                                if List.exists (
                                    (fun (act', stamp') -> 
                                        Float.equal c.stamp stamp' 
                                        &&
                                        String.equal act' act
                                    )
                                ) stamps then (stamps) else
                                let _ = (grid.active := act) in
                                let stamps = rununiquestamp grid y ((act, ctx.stamp) :: stamps)
                                in stamps
                            ) stamps
                        ) 
                    | _ -> 
                        (stamps)
                ) stamps (ctx.writes) in 
                let _ = grid.active := oldactive in 
                stamps
        | Error e -> 
                let _ = grid.onlog (Format.sprintf "Error: %s\n" e, Ndcontroller.Err)
                in stamps
        )
    in ignore @@ rununiquestamp grid form []
;;

let handle_parse_exp grid src (lex: Lexer.lexeme list) = 
    (
        Parser.parse lex src 
        |> (function 
            | Ok ({ Parser.prog; _ }, _lefttoks) -> (
                handle_transform_formulae grid prog
            )
            | Error s   -> 
                grid.onlog ((Format.sprintf "Parse Error (l: %d, c: %d): %s\n" s.line s.colm s.errt, Ndcontroller.Err))
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
            | Error (l) ->  
                Error (l)
        )
    )
;;

let handle_scan_exp grid (_exp: string) = 
    (
        Lexer.runall _exp
        |> (function 
            | Ok _res -> handle_parse_exp grid _exp _res
            | Error (l) ->  
                grid.onlog ((Format.sprintf "Scan Error: l:%d c:%d %s" l.line l.colm l.errt, Ndcontroller.Err))
        )
    )
;;

let mkbuf s = (let b = Buffer.create 64 in let _ = Buffer.add_string b s in b) ;;

let scan_and_notify sheet vstr = 
   match simple_scan_exp sheet vstr with  
   | Ok cell ->
       (* transform the ast to capture any reads and writes so we can
          form dependencies *)
        (match Eval.tosource sheet cell !_OPTS with
            | Ok ({ ast=(Parser.Stmt s); _ } as cell) ->
                let _ = Ndcontroller.dependants sheet cell.ast in
                let _ = handle_eval sheet cell in
                let oldactive = !(sheet.active) in
                (* see if we wrote over other formulaes *)
                let _ = (
                    List.iter (fun (msk, shp) ->
                        Ndcontroller.notify sheet msk shp
                        |> List.map (
                            fun (index,y) ->
                                let act = Ndcontroller.IndexToSheet.find sheet.indexed index in
                                let _ = (sheet.active := act) in
                                List.map (fun x -> (index, x)) (Ndcontroller.affected sheet (Ndcontroller.plaindctx ()) y)
                        )
                        |> List.concat
                        |> List.sort_uniq (fun (_, (Parser.Stmt x)) (_, (Parser.Stmt y)) -> Float.compare x.stamp y.stamp)
                        |> List.iter (fun (index, y) ->
                            let act = Ndcontroller.IndexToSheet.find sheet.indexed index in
                            let _ = (sheet.active := act) in
                            handle_transform_formulae sheet y
                        )
                    ) s.writes
                ) in (sheet.active := oldactive)
            | Error s -> 
                sheet.onlog (s, Ndcontroller.Err)
        )
    | Error e -> 
        (
            let s = ("EvalErr: (l: " ^ (string_of_int e.line) ^", c: " ^ (string_of_int e.colm) ^ "): %s" ^ e.errt) in
            sheet.onlog (s, Ndcontroller.Err);
            failwith s
        )
;;

(* handles input -> return bool on whether to continue *)
let handle_input grid (data: Buffer.t) = 
    let l = Buffer.length data in
    (if l > 0 then
        let o = Buffer.to_bytes data |> Bytes.trim |> Bytes.to_string in
        if String.starts_with ~prefix:"=" o then 
                let _ = scan_and_notify grid (String.sub o 1 (l-1)) in 
                let _ = Buffer.clear data in
                true
        else
        (match o with 
            | "q" | "quit" | "!q" -> (
                let _ = Format.printf "Goodbye :-)\n" in false
            )
            | "f" | "formulaes" | "formulas" -> (
                (match Ndcontroller.formulaes grid !(grid.active) with 
                |  Ok fl ->
                    let _ = 
                        List.iter (fun f -> Format.printf "%d: %s\n" f.Ndcontroller.indx f.Ndcontroller.text) fl
                        in true
                | Error e -> 
                    let _ = Format.printf "Error: %s\n" e 
                    in true
                )
            )
            |  _  -> (
                let _ = Buffer.clear data      in
                true
            )
        )
    else
        true)
;;

(* FIXME: input bufferring seems wonky on some terminals - idk why!! *)
let repl (grid: Ndcontroller.gridcontroller) () = 
    let buf = Buffer.create 1024 in 
    let rec input_formula bufc = 
        let l = Buffer.length bufc in
        let _ = if l == 0 then Format.printf ">>>> %!" else () in
        let _ =
            In_channel.input_line In_channel.stdin
            |> (function 
                | Some s -> 
                    Buffer.add_string bufc s 
                | None -> 
                    ()
            )
        in 
        if handle_input grid bufc then input_formula bufc else ()
    in 
    input_formula buf
;;

