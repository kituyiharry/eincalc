open OUnit;;
open Eincalc;;

let fetch_program (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
   match (fst @@ Result.get_ok res).prog with 
    | Parser.Stmt (Literal (EinSpec (e, _, _))) -> e 
    | Parser.Stmt (Grouping(Literal (EinSpec (e,_,_)))) -> e
    | _ -> failwith "Invalid program"
;;

let fetch_first (res: (Parser.prattstate * Lexer.lexeme list, string) result) = 
    match (fst @@ Result.get_ok res).prog with 
    | Parser.Stmt (Literal (EinSpec (e,f,_))) -> (e, f)
    | Parser.Stmt (Grouping(Literal (EinSpec (e,f,_)))) -> (e, f)
    | _ -> failwith "Invalid program"
;;

let tests = "Parser unit tests" >::: [
    "empty expression" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "()") 
    );
    "simple summation" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ Eincalc.Parser.Shape ([('i', 0);], []); ]; 
            out=None 
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> )")
    );
    "simple identity einsum" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ Parser.Shape ([('i', 0);('j', 1)], []); ];
            out=Some(Parser.Shape ([('i', 0);('j', 1)], []));
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(ij -> ij)")
    );
    "simple matmul" >:: (fun _ -> 
        assert_equal { 
            Parser.inp=[ 
                Parser.Shape ([('i', 0);('k', 1)], []);
                Parser.Shape ([('k', 0);('j', 1)], []);
            ];
            out=Some(Parser.Shape ([('i', 0);('j', 1)], []));
        } (fetch_program @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(ik,kj -> ij)")
    );
    "simple identity einsum with params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=Some(Parser.Shape ([('i', 0);], []));
        }, [ (Parser.Scalar ("D", 10)) ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> i, @D10)")
    );
    "simple identity einsum with params trailing comma" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=Some(Parser.Shape ([('i', 0);], []));
        }, [ (Parser.Scalar ("D", 10)); Parser.Void ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> i, @D10, )")
    );
    "simple summation einsum with ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        }, [ Parser.Range (("D", 10),("E",11)) ]) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i ->  , @D10..E11)")
    );
    "simple summation einsum with multiple ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ 
                Parser.Shape ([('i', 0);], []);
                Parser.Shape ([('i', 0);], []);
            ];
            out=None;
        },
        [
            Parser.Range (("D", 10),("E",11)); 
            Parser.Range (("E",10), ("F",11)); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i,i -> , @D10..E11, @E10..F11)")
    );
    "simple summation einsum with multiple mixed ranged params" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ 
                Parser.Shape ([('i', 0);], []);
                Parser.Shape ([('i', 0);], []);
            ];
            out=None;
        },
        [
            Parser.Scalar ("D",10); 
            Parser.Scalar ("F",11); 
            Parser.Range  (("A",0), ("A", 100)); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i,i -> , @D10, @F11, @A0..A100)")
    );
    "simple summation with static arrays" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [ Parser.NdArray (Parser.Itemize [1.;2.;3.;4.;5.]); ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , [1,2,3,4,5])")
    );
    "simple summation with ndim arrays" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [ Parser.NdArray (
                Parser.Collect [
                    Parser.Itemize [1.;2.;3.;4.;5.];
                    Parser.Itemize [1.;2.;3.;4.;5.];
                ]
            ); ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , [[1,2,3,4,5],[1,2,3,4,5]])")
    );
    "simple summation with bigger ndim arrays" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [ 
            Parser.NdArray (
                Parser.Collect [
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [5.;4.;3.;2.;1.];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [5.;4.;3.;2.;1.];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                ]
            ); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.runall "
                (i -> , 
                    [
                        [
                            [1,2,3,4,5],
                            [5,4,3,2,1],
                            [0,0,0,0,0],
                        ],
                        [
                            [1,2,3,4,5],
                            [5,4,3,2,1],
                            [0,0,0,0,0],
                        ],
                    ]
                )
            ")  
    );
    "simple summation with non-homogenous ndim arrays" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [ 
            Parser.NdArray (
                Parser.Collect [
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [5.;4.;3.;];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [1.];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                ]
            ); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.runall "
                (i -> , 
                    [
                        [
                            [1,2,3,4,5],
                            [5,4,3],
                            [0,0,0,0,0],
                        ],
                        [
                            [1,2,3,4,5],
                            [1],
                            [0,0,0,0,0],
                        ],
                    ]
                )
            ")  
    );
    "simple summation with ndim arrays and trailing commas" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [ 
            Parser.NdArray (
                Parser.Collect [
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                    Parser.Collect [
                        Parser.Itemize [1.;2.;3.;4.;5.];
                        Parser.Itemize [0.;0.;0.;0.;0.];
                    ];
                ]
            ); 
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.runall "
                (i -> , 
                    [
                        [
                            [1,2,3,4,5,],
                            [0,0,0,0,0,],
                        ],
                        [
                            [1,2,3,4,5,],
                            [0,0,0,0,0,],
                        ],
                    ]
                )
            ")  
    );
    "simple summation with malformed ndim arrays and commas" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Parser.parse @@ Result.get_ok @@ Lexer.runall "
            (i -> , [ ,2,3,4,5,, [0,0,0,0,0,], ], [ [1,2,3,4,5,], [0,0,0,0,0,], ], ])
        ") 
    );
    "simple summation with relative indexing" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
        [
            Parser.Relative (Parser.West 1, Parser.Scalar ("D", 3));
        ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , <D3)")
    );
    "simple summation with multiple relative indexing" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
            [
                (* TODO: Shrink this to West 3 if necessary *)
                Parser.Relative (
                    (Parser.West 1), (Parser.Relative (
                        (Parser.East 1), (Parser.Scalar ("D", 3))
                    ))
                );
            ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , <>@D3)")
    );
    "simple summation with reference" >:: (fun _ -> 
        assert_equal ({ 
            Parser.inp=[ Parser.Shape ([('i', 0);], []); ];
            out=None;
        },
            [
                Parser.Refer (Self);
            ]
        ) (fetch_first @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , @self)")
    );
    "simple summation with unknown reference" >:: (fun _ -> 
        assert_equal true (Result.is_error @@ Parser.parse @@ Result.get_ok @@ Lexer.run 0 "(i -> , @unknown)")
    );
]

let _ = 
    run_test_tt_main tests
;;
