let () = 
    let contr = Eincalc.Ndcontroller.create_controller () in
    let grid = Eincalc.Ndcontroller.new_sheet contr "Default" in
    Eincalc.Repl.repl grid ()
