type startctx = { 
        load: string option
    ;   run : string option 
    ;   format: string option
    ;   interactive: bool
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
                | n -> 
                    failwith ("unrecognized cli option: " ^ n)
            )
    in
    args len 1 { load=None; run=None; interactive=false;format=None;  }
;;

let interp_args controller ctx = 
    let _ = (match ctx.load with
        | Some file -> 
            let is_csv = (String.ends_with ~suffix:"csv" file) || String.equal (Option.value ~default:"" ctx.format) "csv" in
            let is_tsv = (String.ends_with ~suffix:"tsv" file) || String.equal (Option.value ~default:"" ctx.format) "tsv" in
            if not @@ (is_csv || is_tsv) then 
                (failwith "only csv and tsv files supported") 
            else
                let ic = open_in file in
                let buf = Seq.of_dispenser (fun () -> In_channel.input_line ic) in
                let r = ref 0 in
                let _   = Seq.iter (fun line -> 
                    (match Eincalc.Ndcontroller.paste_values controller "Default" (if is_csv then ','  else '\t') (!r, 0) line 
                        with 
                        | Ok (r', c') -> 
                            incr r; 
                            (*c := !c + c'*)
                        | Error e -> (
                            Format.printf "Error loading line: %s" e
                        )
                    )
                ) buf in
                let _ = Format.printf "Added %d rows\n" !r in
                () 
        | _ -> 
            ()
    ) in ()
;;

let () = 
    let ln = Array.length Sys.argv in
    let contr = Eincalc.Ndcontroller.create_controller () in
    let grid = Eincalc.Ndcontroller.new_sheet contr "Default" in
    let _ = Format.printf "%d args\n" ln in
    if ln > 1 then 
        let _ = interp_args grid (parse_args ln) in
        Eincalc.Repl.repl grid ()
    else
        Eincalc.Repl.repl grid ()
