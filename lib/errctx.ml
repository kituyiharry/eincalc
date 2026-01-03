type parserr = {
        line: int 
    ;   colm: int 
    ;   errt: string
    ;   sugg: string (* key that can be used to show actions e.g. parser.mask will suggest a list of masks *)
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
