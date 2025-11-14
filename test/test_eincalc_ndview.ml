open OUnit;;
open Eincalc;;


let _tests = "Ndview unit tests" >::: [
    "Scalar slice" >:: (fun _ -> 
        let module S = Ndarray.Scalar in 
        let s = S.make  [||] 10. in
        let _ = S.set s [||] 999. in
        let module SV = Ndview.MakeSliceView(S) in
        let slice = SV.make s ([ Parser.Along 1 ])  in
        let eq = ref true in
        let _ = (SV.iteri(fun _d v -> eq := (!eq && Float.equal v 999.))) slice in
        assert_bool "Slice error"  !eq
    );
    "Matrix slice" >:: (fun _ -> 
        let module S = Ndarray.Matrix in 
        let s = S.make  [|3;3|] 10. in
        let _ = S.set s [|0;0|] 999. in
        let _ = S.set s [|0;1|] 999. in
        let _ = S.set s [|0;2|] 999. in
        let module SV = Ndview.MakeSliceView(S) in
        let slice = SV.make s ([ 
            Parser.Select { start=(Some 0); len=(Some 1); skip=None };
            Parser.Select { start=None; len=None; skip=None } 
        ])  in
        let eq = ref true in
        let _ = (SV.iteri(fun _d v -> eq := (!eq && Float.equal v 999.))) slice in
        assert_bool "Slice error"  !eq
    );
]

(* TODO: add more tests *)

let _ = run_test_tt_main _tests
;;

