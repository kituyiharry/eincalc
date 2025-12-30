type startctx = { 
        load: string option
    ;   run : string option 
    ;   format: string option
    ;   interactive: bool
    ;   debug: bool
    ;   opts : bool
} [@@deriving show];;

let parse_args len = 
    let rec args len idx ctx = 
        if len <= 1 then 
            ctx 
        else
            let rem = len - 1 in
            (match Sys.argv.(idx) with
                | "-l" | "-load" | "--load" -> 
                    if rem <= 1 then
                        failwith "args error: missing file to load on start: --load <file>"
                    else
                        let fname = Sys.argv.(idx + 1) in 
                        args (rem-1) (idx+2) { ctx with load=Some fname; }  
                | "-r" | "-run" | "--run" -> 
                    if rem <= 1 then
                        failwith "args error: missing file to run on start: --run <file>"
                    else
                        let fname = Sys.argv.(idx + 1) in 
                        args (rem-1) (idx+2) { ctx with run=Some fname; }  
                | "-f" | "-format" | "--format" -> 
                    if rem <= 1 then
                        failwith "args error: missing file format (supports only csv or tsv. leave out to infer from filename): --format <file>"
                    else
                        let fmt = Sys.argv.(idx + 1) in 
                        args (rem-1) (idx+2) { ctx with format=Some fmt; }  
                | "-i" | "-int" | "--interactive" | "-interactive"  -> 
                    args (rem) (idx+1) { ctx with interactive=true; }  
                | "-ni" | "-noint" | "--no-interactive" | "-no-interactive"  -> 
                    args (rem) (idx+1) { ctx with interactive=false; }  
                | "-nodebug" | "--nodebug" -> 
                    args (rem) (idx+1) { ctx with debug=false; }  
                | "-noopt" | "--noopt" -> 
                    args (rem) (idx+1) { ctx with opts=false; }  
                | n -> 
                    failwith ("unrecognized cli option: " ^ n)
            )
    in
    args len 1 { load=None; run=None; interactive=false;format=None; debug=true; opts=true }
;;

let load_file controller is_csv _is_tsv file = 
    let ic = open_in file in
    let buf = Seq.of_dispenser (fun () -> In_channel.input_line ic) in
    let r = ref 0 in
    let _   = Seq.iter (fun line -> 
        (match Eincalc.Ndcontroller.paste_values controller "Default" (if is_csv then ','  else '\t') (!r, 0) line 
            with 
            | Ok (_r', _c') -> 
                incr r; 
            | Error e -> (
                Format.printf "Error loading line: %s" e
            )
        )
    ) buf in
    let _ = close_in ic in
    Format.printf "Added %d rows\n" !r
;;

let load_ein_file controller file = 
    let ic = open_in_bin file in
    let line = really_input_string ic (Int64.to_int @@ In_channel.length ic) in
    let s   = String.split_on_char ';' line |> List.map (String.trim) |> List.filter (fun s -> String.length s > 0) in
    (*let buf = char_seq_of_file ic  in*)
    (* is_empty doesn't work on ephemeral *)
    (*let _ = Seq.iter (Format.print_char) buf in*)
    let rec reduce = function 
        | [] -> () 
        | hd :: rest -> 
            if String.length hd > 0 then
                let _ = Eincalc.Repl.scan_and_notify controller (hd) in
                reduce rest
            else
                reduce rest
    in reduce s
;;

let interp_args controller ctx = 
    let _ = (match ctx.load with
        | Some file -> 
            let is_csv = (String.ends_with ~suffix:"csv" file) || String.equal (Option.value ~default:"" ctx.format) "csv" in
            let is_tsv = (String.ends_with ~suffix:"tsv" file) || String.equal (Option.value ~default:"" ctx.format) "tsv" in
            if not @@ (is_csv || is_tsv) then 
                (failwith "only csv and tsv files supported") 
            else
                load_file controller is_csv is_tsv file
        | _ -> 
            ()
    ) in 
    let _ = (match ctx.run with 
        | Some rfile ->  
            load_ein_file controller rfile
        | _ -> 
            ()
    ) in
    ()
;;

let () = 
    let ln = Array.length Sys.argv in
    let contr = Eincalc.Ndcontroller.create_controller () in
    let grid = Eincalc.Ndcontroller.new_sheet contr "Default" in
    let _ = Format.printf "%d args\n" ln in
    if ln > 1 then 
        let ctx = parse_args ln in
        let _ = Eincalc.Repl._DEBUG := ctx.debug in
        let _ = Eincalc.Repl._OPTS := ctx.opts in
        let _ = Format.printf "%s\n" (show_startctx ctx) in
        let _ = interp_args grid ctx in
        if ctx.interactive then 
            Eincalc.Repl.repl grid ()
        else ()
    else
        Eincalc.Repl.repl grid ()
