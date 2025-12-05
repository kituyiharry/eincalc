type lexerr = {
        line: int 
    ;   colm: int 
    ;   errt: [
            |   `Unrecognized of string 
            |   `Expected of string
        ]
}
[@@deriving show];;

type parserr =  {
        line: int 
    ;   colm: int 
    ;   errt: [
        | `Expected of string
    ]
}
[@@deriving show];;

type generr = 
    | NonHomogenous of string
[@@deriving show];;

type emiterr = 
    | UnhandledMask of string
[@@deriving show];;

type evalerr = 
    | UnhandledOperation of string
[@@deriving show];;

type einerr = 
    | LexrErr of lexerr
    | ParsErr of parserr
    | GenfErr of generr
    | EmitErr of emiterr
    | EvalErr of evalerr
[@@deriving show];;

type errctx = {
        errtyp: einerr      (* type of error *)
    ;   errsum: string      (* summary *)
    ;   errmsg: string      (* long message *)
    ;   suggst: string list (* possible suggestions *)
} [@@deriving show];;
