open OUnit;;
open Spinal;;
open Spinal.Eval;;
open Spinal.Types;;

let execute grid src = 
    match (Lexer.runall src) with
    |  Ok tokens -> 
        (>>==) (Parser.parse tokens) (fun tree -> 
            (>>==) (Eval.tosource grid (fst tree).prog) (fun comp -> 
                let vm = Eval.mkvm grid (Emitter.convert comp) in 
                let () = Eval.eval vm in
                Ok (vm)
            )
        )
    |  Error (l, c, err) -> 
        failwith (Format.sprintf "Error lexing program!! L: %d C: %d -> %s" l c err)
;;

let compare_kernels x y = 
    strue @@ (sallclose (x) (y)) 
;;

let make_scalar v  = 
    ndarray_of_dim_init [] (fun _ -> v)
;;

let vector_of_list l = 
    let v = Ndarray.Vector.init ([|List.length l|]) (fun _dim -> 
        List.nth l _dim.(0)
    ) in 
    (SNdim((module Ndarray.Vector), v))
;;

let matrix_of_list l  = 
    let r = List.length l in 
    let c = List.length (List.nth l 0) in
    let v = Ndarray.Matrix.init [|r;c|] (fun _dim -> 
        let l' = List.nth l (_dim.(0)) in 
        List.nth l' (_dim.(1))
    ) in
    (SNdim((module Ndarray.Matrix), v))
;;

(* Assumption that the final result kernel is at position 0 *)

let _test_grid = Ndmodel.rand_grid 100. (8, 8) ;;

let tests = "Eval unit tests" >::: [
    "simple vector identity"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(i -> i, @fill<1,[3]>)") in
        assert_equal (true) (compare_kernels (vm.source.kernels.(0)) (vm.source.kernels.(1)))
    );
    "simple vector summation"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(i -> , @fill<1,[3]>)") in
        assert_equal (true) (compare_kernels (make_scalar 3.) (vm.source.kernels.(0)))
    );
    "simple vector by vector mult"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(i,j -> ij, @enum<1,1,[3]>, @enum<1,1,[3]>)") in
        assert_equal (true) (compare_kernels (matrix_of_list 
            [[1.; 2.; 3.];
             [2.; 4.; 6.];
             [3.; 6.; 9.];]
        ) (vm.source.kernels.(0)))
    );
    "simple vector by vector "   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(i,j -> i, @enum<1,1,[3]>, @enum<1,1,[3]>)") in
        assert_equal (true) (compare_kernels (vector_of_list [ 6.; 12.; 18.]) (vm.source.kernels.(0)))
    );
    "simple matrix identity"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(ij -> ij, @fill<1,[3,3]>)") in
        assert_equal (true) (compare_kernels (vm.source.kernels.(1)) (vm.source.kernels.(0)))
    );
    "simple matrix transpose"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(ij -> ji, @enum<0,1,[3,3]>)") in
        let fi = matrix_of_list  
            [   [0.;3.;6.];
                [1.;4.;7.];
                [2.;5.;8.];
            ] 
        in
        assert_equal (true) (compare_kernels (fi) (vm.source.kernels.(0)))
    );
    "simple matrix summation"   >:: (fun _ -> 
        let vm = Result.get_ok @@ (execute _test_grid "(ij -> , @enum<0,1,[3,3]>)") in
        assert_equal (true) (compare_kernels (make_scalar 36.) (vm.source.kernels.(0)))
    );
];;

let _ = 
    run_test_tt_main tests
;;
