open OUnit;;
open Spinal;;

let list_prnt l = 
    let buf = Buffer.create 64 in
    let ()  = List.iter (
        Fun.compose (Buffer.add_string buf) (
            Fun.compose ((^) "\n")
                (Lexer.show_lexeme)
        )
    ) l 
    in Buffer.contents buf
;;

let res_prnt p = function 
    | Ok (o)    -> p o 
    | Error (_l, _c, s) ->  s

let tests = "Lexer unit tests" >::: [
    "empty buffer"   >:: (fun _ -> 
        assert_equal (Ok []) (Lexer.run 0 "") ~printer:(res_prnt (list_prnt))
    );
    "empty formulae" >:: (fun _ ->
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 Tokens.TLeftParen );
            (Lexer.mktok 0 1 Tokens.TRightParen);
        ]) (Lexer.run 0 "()") ~printer:(res_prnt (list_prnt))
    );
    "composite tokens" >:: (fun _ -> 
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 Tokens.TArrow);
            (Lexer.mktok 0 3 Tokens.TRange);
        ]) (Lexer.run 0 "-> ..") ~printer:(res_prnt (list_prnt))
    );
    "Tokens with Data" >:: (fun _ -> 
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 (Tokens.TAlphaNum "abc"));
        ]) (Lexer.run 0 "abc") ~printer:(res_prnt (list_prnt))
    );
    "Variable" >:: (fun _ ->
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 (Tokens.TAlphaNum "D4"));
            (Lexer.mktok 0 2 (Tokens.TRange));
            (Lexer.mktok 0 4 (Tokens.TAlphaNum "E5"));
        ]) (Lexer.run 0 "D4..E5") ~printer:(res_prnt (list_prnt))
    );
    "Static arrays" >:: (fun _ ->
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 (Tokens.TLeftBracket));
            (Lexer.mktok 0 1 (Tokens.TNumeral 1));
            (Lexer.mktok 0 2 (Tokens.TComma));
            (Lexer.mktok 0 3 (Tokens.TNumeral 2));
            (Lexer.mktok 0 4 (Tokens.TComma));
            (Lexer.mktok 0 5 (Tokens.TNumeral 3));
            (Lexer.mktok 0 6 (Tokens.TRightBracket));
        ]) (Lexer.run 0 "[1,2,3]") ~printer:(res_prnt (list_prnt))
    );
    "Static float arrays" >:: (fun _ ->
        assert_equal (Ok [ 
            (Lexer.mktok 0 0 (Tokens.TLeftBracket));
            (Lexer.mktok 0 1 (Tokens.TFloat 1.));
            (Lexer.mktok 0 2 (Tokens.TComma));
            (Lexer.mktok 0 3 (Tokens.TFloat 2.));
            (Lexer.mktok 0 4 (Tokens.TComma));
            (Lexer.mktok 0 5 (Tokens.TFloat 3.5));
            (Lexer.mktok 0 7 (Tokens.TRightBracket));
        ]) (Lexer.run 0 "[1.,2.,3.5]") ~printer:(res_prnt (list_prnt))
    );
    "Sample einsum function expression" >:: (fun _ -> 
        assert_equal (Ok [ 
            (Lexer.mktok 0 0  (Tokens.TAlphaNum "abc"));
            (Lexer.mktok 0 3  (Tokens.TComma));
            (Lexer.mktok 0 4  (Tokens.TAlphaNum "cde"));
            (Lexer.mktok 0 8  (Tokens.TArrow));
            (Lexer.mktok 0 11 (Tokens.TAlphaNum "abde"));
        ]) (Lexer.run 0 "abc,cde -> abde") ~printer:(res_prnt (list_prnt))
    );
    "Full unquoted sample einsum summation call with parameters" >:: (fun _ -> 
        assert_equal (Ok [ 
            (Lexer.mktok 0 0  (Tokens.TLeftParen));
            (Lexer.mktok 0 1  (Tokens.TAlphaNum "i"));
            (Lexer.mktok 0 3  (Tokens.TArrow));
            (Lexer.mktok 0 6  (Tokens.TComma));
            (Lexer.mktok 0 8  (Tokens.TAlphaNum "D2"));
            (Lexer.mktok 0 10 (Tokens.TRange));
            (Lexer.mktok 0 12 (Tokens.TAlphaNum "D8"));
            (Lexer.mktok 0 14 (Tokens.TRightParen));
        ]) (Lexer.run 0 "(i -> , D2..D8)") ~printer:(res_prnt (list_prnt))
    );
    "Full unquoted sample einsum summation call with relative parameters" >:: (fun _ -> 
        assert_equal (Ok [ 
            (Lexer.mktok 0 0  (Tokens.TLeftParen));
            (Lexer.mktok 0 1  (Tokens.TAlphaNum "i"));
            (Lexer.mktok 0 3  (Tokens.TArrow));
            (Lexer.mktok 0 6  (Tokens.TComma));
            (Lexer.mktok 0 8  (Tokens.TLeftAngle));
            (Lexer.mktok 0 9  (Tokens.TNumeral 4));
            (Lexer.mktok 0 10 (Tokens.TAlphaNum "D2"));
            (Lexer.mktok 0 12 (Tokens.TRange));
            (Lexer.mktok 0 14 (Tokens.TAlphaNum "D8"));
            (Lexer.mktok 0 16 (Tokens.TRightParen));
        ]) (Lexer.run 0 "(i -> , <4D2..D8)") ~printer:(res_prnt (list_prnt))
    );
]

let _ = 
    run_test_tt_main tests
;;

