let () = 
    let grid = Eincalc.Ndmodel.enum_grid (26, 26) in
    Eincalc.Repl.repl grid ()
