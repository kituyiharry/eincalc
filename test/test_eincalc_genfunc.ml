open OUnit;;
open Eincalc;;

let fetch_first (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
    match (fst @@ Result.get_ok res).prog with 
    | Parser.Stmt (Ein e) -> 
            List.hd @@ snd @@ e 
    | _ -> 
            failwith "Unhandled fetch"
;; 

let fetch_form (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
  match (fst @@ Result.get_ok res).prog with 
    | Parser.Stmt (Ein e) -> 
        e 
    | _ -> 
        failwith "Unhandled form"
;; 

let lexparse x = 
    Parser.parse @@ Result.get_ok @@ Lexer.runall x 
;;

let as_shape x = 
    match x with
    | Parser.NdArray x -> Genfunc.homogenous @@ Genfunc.metashape x
    | _  -> Error "??"
;;

let tests = "Genfunc unit tests" >::: [
    "empty params are 0 shaped"   >:: (fun _ -> 
        assert_equal ([0])    (Result.get_ok @@ as_shape @@ fetch_first @@ lexparse "(i -> , [])")
    );
    "empty by 1"   >:: (fun _ -> 
        assert_equal ([1; 0]) (Result.get_ok @@ as_shape @@ fetch_first @@ lexparse "(i -> , [[]])")
    );
    "x shaped"   >:: (fun _ -> 
        assert_equal ([3])    (Result.get_ok @@ as_shape @@ fetch_first @@ lexparse "(i -> ,[1,2,3])")
    );
    "x by x shaped"   >:: (fun _ -> 
        assert_equal ([2; 3]) (Result.get_ok @@ as_shape @@ fetch_first @@ lexparse "(i -> , [[1,2,3],[4,5,6]])")
    );
    "x by x shaped multi" >:: (fun _ -> 
        assert_equal ([3; 3; 3]) (Result.get_ok @@ as_shape @@ fetch_first @@ lexparse "(i -> , 
            [[[ 0,  1,  2],
              [ 3,  4,  5],
              [ 6,  7,  8]],
             [[ 9, 10, 11],
              [12, 13, 14],
              [15, 16, 17]],
             [[18, 19, 20],
              [21, 22, 23],
              [24, 25, 26]]]
        )")
    );
    "Non homogenous should fail"   >:: (fun _ -> 
        assert_equal true (Result.is_error @@ as_shape @@ fetch_first @@ lexparse "(i -> , [[1,2],[]])")
    );
    "Non homogenous mult dim"   >:: (fun _ -> 
        assert_equal true (Result.is_error @@ as_shape @@ fetch_first @@
            lexparse "(i -> , [[[1,2,3]],[[]]])")
    );
    "Input to parameter parammatch"  >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Genfunc.parammatch @@ fetch_form @@ lexparse "(i -> , [[1,2,3]])")
    );
    "Input to parameter parammatch without shape check"  >:: (fun _ -> 
        assert_equal true (Result.is_ok @@ Genfunc.parammatch @@ fetch_form @@ lexparse "(ij,jk -> , [[1,2,3]], [[]])")
    );
    "Output not reflected in input check" >:: (fun _ ->
        assert_equal true (Result.is_error @@ Genfunc.correspondence @@ fetch_form @@ lexparse "(ij,jk -> qtv, [[1,2,3]], [[]])")
    );
    "Output has no duplication" >:: (fun _ ->
        assert_equal true (Result.is_error @@ Genfunc.correspondence @@ fetch_form @@ lexparse "(ij,jk -> ii, [[1,2,3]], [[]])")
    );
    "Repeated inputs MUST have the same dimension" >:: (fun _ ->
        assert_equal true (Result.is_ok @@ Genfunc.correspondence @@ fetch_form @@ lexparse "(ij,ij -> ij, [[1,2,3]], [[1,2,3]])")
    );
    "Failure if Repeated inputs DONT have the same dimension" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Genfunc.correspondence @@ fetch_form @@ lexparse "(ij,ij -> ij, [[1,2,3]], [[1,3]])")
    );
    "Failure if Repeated inputs DONT have the same dimension even in summation expr" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Genfunc.correspondence @@ fetch_form @@ lexparse "(ij,jk -> [[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]])")
    );
]

let _ = 
    run_test_tt_main tests
;;
