open OUnit;;
open Eincalc;;

let _tests = "Ndcsr unit tests" >::: [
    "CSR case 1" >:: (fun _ -> 
        (* ensure the list is sorted!! *)
        let data = [
            ((0, 0), Ndmodel.TNumber 1.);
            ((0, 2), Ndmodel.TNumber 2.);
            ((1, 1), Ndmodel.TNumber 3.);
            ((2, 0), Ndmodel.TNumber 4.);
            ((2, 1), Ndmodel.TNumber 5.);
            ((2, 2), Ndmodel.TNumber 6.3333356);
        ] in
        let tbl = Ndmodel.Grid.create 6 in
        let s = Ndcsr.encode 6 (List.to_seq data) in 
        let _ = ignore @@ Ndcsr.decode tbl s in
        let l = 
            Ndmodel.Grid.to_seq tbl |> List.of_seq |> List.sort (
            fun (x, _) (y, _) -> 
                Stdlib.compare x y
            ) 
        in
        (*let _ = Format.printf "ln: %d\n" (List.length l) in*)
        (*let _ = List.iter (fun ((x, y), sp) -> Format.printf "%d,%d -> %s \n" x y (Ndmodel.show_spinmodel sp)) l in*)
        (*let _ = Format.printf "%s\n" (Ndcsr.show_csrstore s) in*)
        assert_equal s
            ({ Ndcsr.values = [|"f1."; "f2."; "f3."; "f4."; "f5."; "f6.3333356"|]; 
                columns = [|0; 2; 1; 0; 1; 2|]; rows = [
                    { Ndcsr.index=0; row=0 }; 
                    { Ndcsr.index=2; row=1 }; 
                    { Ndcsr.index=3; row=2 }
                ]; 
            });
        assert_equal data l 
    );
    "CSR case 2" >:: (fun _ -> 
        (* ensure the list is sorted!! *)
        let data = [
            ((0, 0), Ndmodel.TNat 5);
            ((1, 1), Ndmodel.TNat 8);
            ((2, 2), Ndmodel.TNat 3);
            ((3, 1), Ndmodel.TNat 6);
        ] in
        let s = Ndcsr.encode 4 (List.to_seq data) in 
        let tbl = Ndmodel.Grid.create 6 in
        let _ = Ndcsr.decode tbl s in
        let l = 
            Ndmodel.Grid.to_seq tbl |> List.of_seq |> List.sort (
            fun (x, _) (y, _) -> 
                Stdlib.compare x y
            ) 
        in
        (*let _ = Format.printf "%s\n" (Ndcsr.show_csrstore s) in*)
        assert_equal s
            ({ Ndcsr.values = [|"n5"; "n8"; "n3"; "n6";|]; 
                columns = [|0; 1; 2; 1|]; rows = [
                    { Ndcsr.index=0; row=0};
                    { Ndcsr.index=1; row=1}; 
                    { Ndcsr.index=2; row=2}; 
                    { Ndcsr.index=3; row=3}
                ]; });
        assert_equal data l; 
    );
    "CSR case 3" >:: (fun _ -> 
        (* ensure the list is sorted!! *)
        let data = [
            ((0, 0), Ndmodel.TNat 10);
            ((0, 1), Ndmodel.TNat 20);
            ((1, 1), Ndmodel.TCover (30., ""));
            ((1, 3), Ndmodel.TNumber 40.);
            ((2, 2), Ndmodel.TValue "fifty");
            ((2, 3), Ndmodel.TValue "sixty");
            ((2, 4), Ndmodel.TNat 70);
            ((3, 5), Ndmodel.TNat 80);
        ] in
        let s = Ndcsr.encode 8 (List.to_seq data) in 
        let tbl = Ndmodel.Grid.create 6 in
        let _ = Ndcsr.decode tbl s in
        let l = 
            Ndmodel.Grid.to_seq tbl |> List.of_seq |> List.sort (
            fun (x, _) (y, _) -> 
                Stdlib.compare x y
            ) 
        in
        (*let _ = Format.printf "%s\n" (Ndcsr.show_csrstore s) in*)
        assert_equal s
            ({ Ndcsr.values = [|"n10"; "n20"; "c30.$"; "f40."; "vfifty"; "vsixty"; "n70"; "n80"|]; 
                columns = [|0; 1; 1; 3; 2; 3; 4; 5;|]; 
                rows = [ 
                    { Ndcsr.index=0; row=0}; 
                    { Ndcsr.index=2; row=1}; 
                    { Ndcsr.index=4; row=2}; 
                    { Ndcsr.index=7; row=3}; 
                ];
            });
        assert_equal data l; 
    );
]

let _ = run_test_tt_main _tests
;;
