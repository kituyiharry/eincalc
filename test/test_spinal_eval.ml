open OUnit;;
open Spinal.Eval;;


let _testvm = {
        spine=  [||]
    ;   stkidx= 0
    ;   frmptr= 0
    ;   source={
            oprtns= [| 
                Nop;
                Nop;
            |]
            ; cursor=  0
            ; consts = [||]
        };
    } 
;;

let tests = "Eval unit tests" >::: [
];;

let _ = 
    run_test_tt_main tests
;;
