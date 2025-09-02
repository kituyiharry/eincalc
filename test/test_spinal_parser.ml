open OUnit;;
open Spinal;;

let fetch_program (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
   fst @@ List.hd @@ (fst @@ Result.get_ok res).prog
;;

let fetch_first (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
   List.hd @@ (fst @@ Result.get_ok res).prog
;;

let tests = "Parser unit tests" >::: [
    "empty expression" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "()") 
    );
    "simple summation" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ Spinal.Parser.Shape [('i', 0);]; ]; 
            out=None 
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> )")
    );
    "simple identity einsum" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ Parser.Shape [('i', 0);('j', 1)]; ];
            out=Some(Parser.Shape [('i', 0);('j', 1)]);
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(ij -> ij)")
    );
    "simple matmul" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ 
                Parser.Shape [('i', 0);('k', 1)];
                Parser.Shape [('k', 0);('j', 1)];
            ];
            out=Some(Parser.Shape [('i', 0);('j', 1)]);
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(ik,kj -> ij)")
    );
    "simple identity einsum with params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape [('i', 0);]; ];
            out=Some(Parser.Shape [('i', 0);]);
        }, [ (Parser.Scalar ("D", 10)) ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> i, D10)")
    );
    "simple identity einsum with params trailing comma" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape [('i', 0);]; ];
            out=Some(Parser.Shape [('i', 0);]);
        }, [ (Parser.Scalar ("D", 10)) ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> i, D10, )")
    );
    "simple summation einsum with ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape [('i', 0);]; ];
            out=None;
        }, [ Parser.Range (("D", 10),("E",11)) ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i ->  , D10..E11)")
    );
    "simple summation einsum with multiple ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ 
                Parser.Shape [('i', 0);];
                Parser.Shape [('i', 0);];
            ];
            out=None;
        },
        [
            Parser.Range (("D", 10),("E",11)); 
            Parser.Range (("E",10), ("F",11)); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i,i -> , D10..E11, E10..F11)")
    );
    "simple summation einsum with multiple mixed ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ 
                Parser.Shape [('i', 0);];
                Parser.Shape [('i', 0);];
            ];
            out=None;
        },
        [
            Parser.Scalar ("D",10); 
            Parser.Scalar ("F",11); 
            Parser.Range  (("A",0), ("A", 100)); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i,i -> , D10, F11, A0..A100)")
    );
    "simple summation with static arrays" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape [('i', 0);]; ];
            out=None;
        },
        [ Parser.Static [1;2;3;4;5]; ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , [1,2,3,4,5])")
    );
    "simple summation with relative indexing" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape [('i', 0);]; ];
            out=None;
        },
        [
            Parser.Relative (Parser.West 1, Parser.Scalar ("D", 3));
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , <D3)")
    );
]

let _ = 
    run_test_tt_main tests
;;
