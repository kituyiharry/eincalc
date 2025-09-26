open OUnit;;
open Spinal;;


let _tests = "Ndarray unit tests" >::: [
    "Scalar initialization" >:: (fun _ -> 
        let module S = Ndarray.Scalar in 
        let s = S.make  [||] 10. in
        let _ = S.set s [||] 999. in
        assert_equal 999. (S.get s [||])
    );
    "Vector initialization" >:: (fun _ -> 
        let module S = Ndarray.Vector in 
        let s = S.make  [|10|] 10. in
        let _ = S.set s [|00|] 999. in
        let _ = S.set s [|09|] 888. in 
        let () = assert_equal 999. (S.get s [|0|]) in
        assert_equal 888. (S.get s [|9|])
    );
    "Matrix initialization" >:: (fun _ -> 
        let module S = Ndarray.Matrix in 
        let s = S.make  [|3;3|] 10. in
        let _ = S.set s [|00;00|] 999. in
        let _ = S.set s [|01;01|] 999. in
        let _ = S.set s [|02;02|] 999. in
        let () = assert_equal 999. (S.get s [|0;0|]) in
        let () = assert_equal 999. (S.get s [|1;1|]) in
        let () = assert_equal 999. (S.get s [|2;2|]) in
        assert_equal 999. (S.get s [|0;0|])
    );
    "Batch Matrix initialization" >:: (fun _ -> 
        let module S = Ndarray.BatchMatrix in 
        let s = S.make  [|3;3;3|] 10. in
        let _ = S.set s [|00;00;00|] 999. in
        let _ = S.set s [|01;01;01|] 999. in
        let _ = S.set s [|02;02;02|] 999. in
        let () = assert_equal 999. (S.get s [|0;0;0|]) in
        let () = assert_equal 999. (S.get s [|1;1;1|]) in
        let () = assert_equal 999. (S.get s [|2;2;2|]) in
        assert_equal 999. (S.get s [|0;0;0|])
    );
(*
 *    "Shape values" >:: (fun _ ->
 *
 *        let module M = Ndarray.Matrix(Float) in 
 *        let module B = Ndarray.BatchMatrix(Float) in 
 *        let module V = Ndarray.Vector(Float) in 
 *        let module S = Ndarray.Scalar(Float) in 
 *
 *        let s = S.of_list [0.] in
 *        let v = V.of_list [1.;2.;3.] in
 *        let m = M.of_list [[1.; 2.];[1.; 2.]; [1.; 2.]] in
 *        let b = B.of_list [[[1.];[2.]];[[1.];[2.]];[[1.];[2.]]] in
 *
 *        let _ = assert_equal [||]      (S.shape s) in
 *        let _ = assert_equal [|3|]     (V.shape v) in
 *        let _ = assert_equal [|3;2|]   (M.shape m) in
 *                assert_equal [|3;2;1|] (B.shape b)
 *    )
 *)
]

let _ = run_test_tt_main _tests
;;

