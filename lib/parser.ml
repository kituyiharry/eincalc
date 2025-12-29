(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
(* TODO: Replace Format.sprintf|printf calls for smaller bundle size! *)
open Tokens;;
open Lexer;;

let (>>==) = Result.bind;;
let (let*) = Result.bind;;

module CharSet = Set.Make (Char);;

type einmatch = {
        label: char (* the label itself eg i k ... *)
    ;   param: int  (* its parameter number *)
    ;   index: int  (* its index in the expression, i in ik is 0 *)
    ;   dimen: int  (* number of dimensions to corresponding shape *)
    ;   outlc: int  (* if it appears in the output expression *)
} 
and eincomp = {
       shape: int list
    ;  elems: einmatch list
    ;  chset: CharSet.t      [@opaque]
    ;  param: crange
    ;  masks: mask list
} 
and eintree = {
        inps: eincomp list 
    ;   outs: (einmatch list option * int list * mask list) (* output matches, final shape and post execution masks *)
}
and lit     = 
    (* TODO: Does disjoint set make sense ?? *)
    (* evaluation of this mask is AFTER the parameter masks *)
    (* variable and its index along a mask list *)
    | Number  of (float) 
    | Tensor  of (crange)
    | EinSpec of (einsum * params * eintree option)
(* normal arithmetic operations *)
and shape = 
    | Shape  of ((char * int) list * mask list)
and unary = 
    | Negate 
    (*| Invert *)
and factor = 
    | Div 
    | Mul 
and term = 
    | Sub 
    | Add 
(*and comparison = *)
    (*| Greater *)
    (*| GreaterEq *)
    (*| Lesser *)
    (*| LesserEq *)
(*and equality = *)
    (*| Eq *)
    (*| NotEq *)
(*and logical = *)
    (*| And*)
    (*| Or*)
and referral = 
    | Self  (* the current cell *)
and call = 
    (* @diag<2.,4> creates a 4x4 matrix with 2 along the diagonal *)
    | Diag  of float * int  (* square matrix with diagonal values *) 
    (* @zeros<[x,y,...]>  shaped ndarray filled with zeros*)
    | Zeros of int list     (* zero init with a shape  *)
    (* @ones<[x,y,...]>  shaped ndarray filled with ones *)
    | Ones  of int list     (* ones init with a shape  *)
    (* @fill<x, [x,y,...]>  shaped ndarray filled with x *)
    | Fill  of float * int list (* fill shape with a certain value *)
    (* @enum<0,1,[4,3]> *)
    | Enum  of float * float * int list (* enumerate from minvalue and increment with a shape *)
    (* @random<bound, [x,y,...]>  shaped ndarray filled with random values between 0 and bound *)
    | Rand  of float * int list (* random with bound and shape *)
    (* @alt<[x,y,z,....], [x,y,...]>  shaped ndarray filled with alternating
       values from the first argument *)
    | Alt   of float list * int list (* alternate of values *)
and slice = 
    (* TODO: technically we only need Select, Along can be written in terms of
       select but its here for communication purposes - remove when necessary *)
    | Along  of int       (* index, - adds direction *)
    | Select of { 
            start: int option 
        ;   len:   int option 
        ;   skip:  int option
    }   (* start and length and skip *)
and props = (string, ttype) Hashtbl.t [@opaque]
and draw = 
    | Box    of props 
    | Circle of props
    | Line   of props
    | Text   of props
    | Clear
    | Reset
and plot = 
    | Line    of props
    | Bar     of { 
        data:  slice list; (* data from zero, usually a pair of a slice along the row and column *)
        ylabl: crange;     (* labels for each entry of data *)
        props: props
    }
    | Hist    of props
    | Pie     of props
    (* TODO: don't restrict to slices, maybe up a level to support axis or cell ranges
       directly - think this out *)
    | Scatter of { 
            slices: slice list list 
        ;   props: props
    }
and mask = 
    (* TODO: robust scaling and unit vector norm *)
    (* minmax<x,y> *)
    | MinMax of float * float     (* min max between a pair of values *)
    (* zscore *)
    | ZScore                      (* z-score normalization *)
    (* mean *)
    | Mean
    (* mode *)
    | Mode
    (* Sum *)
    | Sum
    (* Min value *)
    | Min
    (* Max value *)
    | Max
    (* cumulative sum *)
    | Cumsum
    (* logsumexp with beta *)
    | LogSumExp of float
    (* softmax with beta *)
    | Softmax of float
    (* apply a function *)
    | Map of (float -> float)
    (* stddev *)
    | Stddev
    (* reshape<[x,y,...]> *)
    | Reshape of int list
    (* write<A100> *)
    | Write   of cell             (* executes an effect to the grid *)           
    (* axis<'j', mean | ...> *)
    | Axis    of int * mask list  (* apply mask along an axis *)
    (* TODO: Logits?? *)
    (* slice<[1, -1:10:3]> *)        
    | Slice   of slice list       (* slice an array - np slice syntax *)
    (*draw<handle, [height, width], [{...props},..]>*)
    | Draw    of { 
            handle: string 
        ;   bounds: int list (* only 1st 2 are accounted for *)
        ;   elmnts: draw list
    }
    (* plot<title, line<...>> *)
    | Plot    of { 
            handle: string
        ;   oftype: plot 
        ;   bounds: int list
    }
    | Sort of (int * expr * int list)
    (*| Binning *)
    (*| Unbox                     (* undo top dimension maybe by running a function over it ?? *) *)
    (*| Partition                 (* break into groups *) *)
    (*| Determ                    (* determinant *) *)
    (*| Rescale                   (* values add up to a certain num *)*)
    (*| Map *)
    (*| Reduce *)
and  cell    = string * int      (* Columns are strings, Rows are numbers *)
and  dimnsn  = shape             (* literal and its index *)
and  motion  =  
    | North of int (* ^ *)
    | South of int (* _ *)
    | East  of int (* > *)
    | West  of int (* < *)
and 'a ndarray = 
    | Itemize of 'a list          (* The columns *)
    | Collect of 'a ndarray list  (* The rows    *)
and query = 
    | Filter   of int * int list * expr
and  crange  = 
    | Range    of cell * cell     (* spreadsheet cells *)
    | Span     of cell * int list (* spreadsheet cell and a shape or length *)
    | Scalar   of cell
    (* categorical data e.g [ "One", "Two", "Three", ... ] 
       can be paired with numbers for use with cover *)
    (*| Category of string * float list*)
    | NdArray  of float ndarray
    | Relative of motion * crange (* Relative cell - Up ^, Down _, Left <, Right, > *)
    | Refer    of referral        (* a way to refer to the current cell *) 
    | Create   of call 
    | Mask     of crange * mask list (* functional masks - copy over data *)
    | Query    of query * crange    (* modify from a start cell with a shape*)
    | Void
and  params  = crange list       (* function parameters *)
and  einsum  = { 
        inp: dimnsn list         (* input.  should correspond to the number of params *)
    ;   out: dimnsn option       (* output - can be empty *)
}
and expr     = 
    | Literal  of lit
    | Factor   of factor
    | Term     of term

    (* TODO: arbitrary expressions for filters and sorts ... etc *)
    (*| Compr     of comparison*)
    (*| Operator  of equality*)
    (*| Logic     of logical*)

    | Unary    of unary * expr
    | Binary   of expr  * expr * expr
    | Reduce   of expr  * mask list
    | Grouping of expr 

(* TODO: Blocks can only have 1 inner expression and then enclose other blocks or expressions 
   We want to discourage long blocks. *)
and state = 
    {
        prog:   expr
    ;   inputs: crange list 
    ;   source: string
    ;   writes: (mask * int list) list (* mask and shape list, only Write<..> masks should be here *)
    ;   stamp:  float                  (* stamp *)
    }
and  formula = 
    (* 
       the program, 
       input dependencies (only Ranges and Spans from the grid)
       and 
       output dependencies (only Writes for now) from the grid 
    *)
    (*expr * crange list * mask list*)
    | Stmt of state
and  program = formula
[@@deriving show];;

type precedence = 
    | PrecNone 
    | PrecAssgn 
;;

type prattstate = {
        curr: lexeme  option
    ;   prev: lexeme  option
    ;   prog: program
} [@@deriving show];;

let einempty = { 
        inp = []
    ;   out = None
    ;
};;

let prattempty = {
    curr = None; prev = None; prog = Stmt { 
        source="";
        prog=(Literal (EinSpec (einempty, [], None)))
        ; inputs=[]; writes=[]; stamp=(Unix.gettimeofday ())
    }
};;

(* check current token without advancing state *)
let past tokn ({prev; _}) = 
    match prev with
    | None -> false 
    | Some { tokn=tok; _ } -> (equal_ttype tok tokn)
;;

(* check current token without advancing state *)
let check tokn ({curr; _}) = 
    match curr with
    | None -> false 
    | Some { tokn=tok; _ } -> (equal_ttype tok tokn)
;;

(* advancing to the next token *)
let advance (state, rem) = 
    match rem with 
    | [] -> 
        ({ state with prev=state.curr; curr=(None) }, rem)
    | hd :: rest -> 
        (*let _ = Format.printf "handled tok: %s\n" (show_lexeme (hd)) in*)
        (*let _ = Format.print_flush () in*)
        ({ state with prev=state.curr; curr=(Some hd) }, rest)
;;

let consume state tt  =
    (match (fst state).curr with
        | Some { tokn; _ } ->
            (if equal_ttype tokn tt then 
                (Ok (advance state)) 
             else
                (Error (Format.sprintf "Expected consume %s found %s" (show_ttype tt) (show_ttype tokn)))
            )
        | _ -> Error ("failed consumption check with bad termination expecting " ^ (show_ttype tt))
    )
;;

(* consume if it exists *)
let consume_optional tt state = 
    match (consume state tt) with 
    | Ok s -> Ok s
    | Error _ -> Ok state
;;

(* parse within an enclosed block like brackets, curly braces .... *)
let enclosed opentok closetok apply state = 
    (if check opentok (fst state) then 
        let nxt' = advance state in
        (>>==) (apply nxt') (fun (after, outcome) -> 
            (>>==) (consume after closetok) (fun final -> 
                Ok (final, outcome)
            )
        )
        else Error (Format.sprintf "missing opening enclose token (%s) found %s" (show_ttype opentok) 
        (show_prattstate (fst state)))
    )
;;

let current state = 
    (fst state).curr 
;;

let rec takenum state = 
    (match current state with 
    | Some ({ tokn=(TFloat v); _ }) -> 
        Ok ((TFloat v), (advance state)) 
    | Some ({ tokn=(TNumeral _v) as x; _ }) -> 
        Ok (x, (advance state))
    | Some ({ tokn=KMinus; _ }) -> 
        (let* (n, state') = takenum (advance state) in
            (match n with
            | TFloat v -> 
                Ok ((TFloat   (-1. *. v)), state')
            | TNumeral n -> 
                Ok ((TNumeral (-1 * n)), state')
            | _ -> 
                Error "invalid value in takenum"
            )
        )
    | _ -> 
        Error "invalid token when extracting number"
    )
;;

(* --- *)

(* valid strings are non empty ascii sequences *)
let validate word = 
    String.length word > 0 && String.is_valid_utf_8 word
;;

let parse_key_value state = 
    let prptbl = Hashtbl.create 8 in
    enclosed TOpenCurly TCloseCurly (fun state' -> 
        let rec collect cstate =
            (match current cstate with 
            | Some ({ tokn=(TAlphaNum key); _ }) -> 
                (>>==) (consume (advance cstate) TEq) (fun cstate' -> 
                    (match (current cstate') with 
                    | Some ({ tokn=(TCloseCurly); _ }) -> 
                        Error "missing key value"
                    | Some ({ tokn=KMinus; _}) ->
                        let* (num, after) = takenum cstate' in 
                        let _ = Hashtbl.add prptbl key num in 
                        collect after 
                    | Some ({ tokn; _ }) -> 
                        let _ = Hashtbl.add prptbl key tokn in 
                        collect (advance cstate')
                    | _ -> 
                        Error "terminated when parsing value for key"
                    )
                )
            | Some ({ tokn=(TComma);_ }) -> 
                collect (advance cstate)
            | Some ({ tokn=(TCloseCurly);_ }) -> 
                Ok ((cstate), prptbl)
            | _ -> 
                Error "bad termination when parsing key value, expected string key"
            )
        in 
        collect state'
    ) state 
;;

(* NB: inputs will be in reverse order of declaration! *)
let parse_ein_inp ein word ml = 
    { ein with 
        inp=(Shape (
            (List.of_seq @@ Seq.mapi (fun i c -> (c, i)) (String.to_seq word))
            , ml
        ) :: ein.inp) 
    } 
;;

let parse_ein_out ein word ml = 
    if validate word then  
        { ein with out=Some (Shape (
            (List.of_seq @@ Seq.mapi (fun i c -> (c, i)) (String.to_seq word))
            , ml
        )) } 
    else 
        { ein with out=None }
;;

(* ensure the order of inputs are symmetric to the declaration *)
let reorder ein = 
    { ein with inp=List.rev ein.inp; } 
;;
let as_cell w = 
    match Seq.find_index (fun c ->  Lexer.isDigit c) @@ String.to_seq w  with 
    | Some idx -> 
        let row = String.sub w 0 idx in
        let col = String.sub w idx (String.length w - idx) in
        ( match int_of_string_opt col with 
            | Some col'->  Ok (row, col')
            | None     ->  Error (Format.sprintf "Invalid row value - should be a number: %s" col)
        ) 
    | None -> 
        Error "Row index not found??"
;;

(* return consumed state + extracted numerals forming the array *)
let parse_static_array state = 
    let rec stack state' rows = 
        (* collect an entire row first *)
        (>>==) (collect state' []) (fun (after, toadd) -> 
            (
                (* check for more rows *)
                if check TComma (fst after) then
                    let next = advance after in
                    (
                        (* check if we are starting a new row *)
                        if check TLeftBracket (fst next) then
                            stack (advance next) (toadd :: rows)
                        (* check if it was just a trailing comma *)
                        else if check TRightBracket (fst next) then
                            Ok (advance next, (toadd :: rows))
                        (* weird state  *)
                        else 
                            Error (Format.sprintf "Unexpected token in stack: %s!" (show_prattstate (fst next)))
                    )
                else if check TRightBracket (fst after) then 
                    Ok (advance after, ((toadd :: rows)))
                else
                    Ok (after, (toadd :: rows))
            )
        )
    and collect state' numerals =  
        match (fst state').curr with
        | Some { tokn; _ } -> 
            (match tokn with
                | KMinus -> 
                    let* (num, after) = takenum state' in 
                    let _ = Format.printf "found num %s!\n" (show_ttype num) in
                    (match num with
                    | TFloat v   ->  
                        collect (after) (v :: numerals)
                    | TNumeral n ->
                        collect (after) ((float_of_int n) :: numerals)
                    | _ ->
                        Error "expected number in static array decl"
                    )
                | TFloat value ->  
                    collect (advance state') (value :: numerals)
                | TNumeral value ->  
                    collect (advance state') ((float_of_int value) :: numerals)
                | TComma -> 
                    (if past TComma (fst state') then
                        Error "Missing value between commas!"
                    else
                        collect (advance state') (numerals))
                | TLeftBracket -> 
                    (* collect remaining rows *)
                    (>>==) (stack (advance state') []) (fun (fin, blk) -> 
                        Ok (fin, Collect (List.rev blk))
                    )
                | TRightBracket -> 
                    Ok ((advance state'), (Itemize (List.rev numerals)))
                | n ->
                    Error (Format.sprintf "Unexpected token %s in static array - only floats supported" (show_ttype n))
            ) 
        | _ -> Error "Unexpected close - need static array"
    in (collect state [])
;;

let compass tokn motn = 
    match tokn with 
    | TLeftAngle  ->
        (West motn)
    | TRightAngle ->
        (East motn)
    | TCaret      ->
        (North motn)
    | TUnderscore -> 
        (South motn)
    | _ -> 
        failwith "invalid token in compass"
;;

let parse_extract_slice_indices state = 
    let rec collect_rem next p = 
        (match (fst next).curr with 
            | Some { tokn; _ } -> 
                (match tokn with
                    | TComma -> 
                        collect_rem (advance next) p
                    | TNumeral start -> 
                        let next' = advance next in 
                        (if check TColon (fst next') then 
                            let next = advance next' in 
                            (match (fst next).curr with
                                | Some ({ tokn =TNumeral len; _ }) -> 
                                    let next = advance next in 
                                    (if check TColon (fst next) then 
                                        let next = advance next in 
                                        (match (fst next).curr with
                                            | Some ({ tokn =TNumeral skip; _ }) -> 
                                                collect_rem (advance next) ((
                                                    Select { 
                                                        start=Some start;
                                                        len  =Some len;
                                                        skip =Some skip;
                                                    }
                                                ) :: p)
                                            | _ -> 
                                                collect_rem (next) ((
                                                    Select { 
                                                        start=Some start;
                                                        len  =Some len;
                                                        skip =None;
                                                    }
                                                ) :: p)
                                        ) 
                                        else  
                                            collect_rem (next) ((
                                                Select { 
                                                    start=Some start;
                                                    len  =Some len;
                                                    skip =None;
                                                }
                                            ) :: p)
                                    )
                                | Some ({ tokn=TComma; _ }) -> 
                                    collect_rem (advance next) ((
                                        Select { 
                                            start=(Some start);  
                                            len=None;
                                            skip=None;
                                        }
                                    ) :: p)
                                | Some ({ tokn=TColon; _ }) -> 
                                    let next = advance next in 
                                    (match (fst next).curr with 
                                        | Some ({ tokn=(TNumeral skip); _ }) -> 
                                            collect_rem (advance next) ((
                                                Select { 
                                                    start=(Some start);  
                                                    len  = None;
                                                    skip =(Some skip);
                                                }
                                            ) :: p)
                                        | Some ({ tokn=(TComma); _ }) -> 
                                            collect_rem (advance next) ((
                                                Select { 
                                                    start=(Some start);  
                                                    len  = None;
                                                    skip =(None);
                                                }
                                            ) :: p)
                                        | Some _ -> 
                                            collect_rem (next) ((
                                                Select { 
                                                    start=(Some start);  
                                                    len  = None;
                                                    skip =(None);
                                                }
                                            ) :: p)
                                        | None -> 
                                            Error "bad slice structure termination"
                                    )
                                | _ -> 
                                    collect_rem next ((
                                        Select { 
                                            start=(Some start);  
                                            len  = None;
                                            skip =(None);
                                        }
                                    ) :: p)
                            )
                            else 
                                collect_rem next' ((Along start) :: p)
                        )
                    | TColon -> 
                        let next = advance next in 
                        (match (fst next).curr with
                            | Some ({ tokn =TNumeral len; _ }) -> 
                                let next = advance next in 
                                (if check TColon (fst next) then 
                                    let next = advance next in 
                                    (match (fst next).curr with
                                        | Some ({ tokn =TNumeral skip; _ }) -> 
                                            collect_rem (advance next) ((
                                                Select { 
                                                    start=None;
                                                    len  =Some len;
                                                    skip =Some skip;
                                                }
                                            ) :: p)
                                        | _ -> 
                                            collect_rem (next) ((
                                                Select { 
                                                    start=None;
                                                    len  =Some len;
                                                    skip =None;
                                                }
                                            ) :: p)
                                    ) 
                                    else  
                                        collect_rem (next) ((
                                            Select { 
                                                start=None;
                                                len  =Some len;
                                                skip =None;
                                            }
                                        ) :: p)
                                )
                            | Some ({ tokn=TComma; _ }) -> 
                                collect_rem (advance next) ((
                                    Select { 
                                        start=None;  
                                        len=None;
                                        skip=None;
                                    }
                                ) :: p)
                            | Some ({ tokn=TColon; _ }) -> 
                                let next = advance next in 
                                (match (fst next).curr with 
                                    | Some ({ tokn=(TNumeral skip); _ }) -> 
                                        collect_rem (advance next) ((
                                            Select { 
                                                start=(None);  
                                                len  = None;
                                                skip =(Some skip);
                                            }
                                        ) :: p)
                                    | Some ({ tokn=(TComma); _ }) -> 
                                        collect_rem (advance next) ((
                                            Select { 
                                                start=(None);  
                                                len  = None;
                                                skip =(None);
                                            }
                                        ) :: p)
                                    | _ -> 
                                        collect_rem (next) ((
                                            Select { 
                                                start=(None);  
                                                len  = None;
                                                skip =(None);
                                            }
                                        ) :: p)
                                )
                            | _ -> 
                                Error "bad slice arg"
                        )
                    | TRightBracket -> 
                        (* exit condition - leave as is to match it in enclose calls *)
                        Ok (next, (List.rev p))
                    |  _ -> Error ("bad slice value: negative slices not currently supported ")
                )
            | None -> 
                (Error "malformed slice specification?")
        )
    in collect_rem state [ ]
;;

let parse_extract_range state = 
    let* (num, after) = takenum state in
    match num with 
    | TNumeral v ->  
        let rec collect_rem state' p  = 
            (*let next = advance state' in*)
            (match (fst state').curr with 
                | Some { tokn; _ } -> 
                    (match tokn with
                        | TComma -> 
                            collect_rem (advance state') p
                        | TNumeral n -> 
                            collect_rem (advance state') ((float_of_int n) :: p)
                        | TFloat f -> 
                            collect_rem (advance state') ((f) :: p)
                        | TRightBracket -> 
                            (* exit condition *)
                            Ok (advance state', (List.rev p))
                        |  KMinus -> 
                            let* (num, after) = takenum state' in 
                            (match num with 
                            | TFloat v -> 
                                collect_rem (after) ((v) :: p)
                            | TNumeral v -> 
                                collect_rem (after) ((float_of_int v) :: p)
                            | _ -> 
                                Error ("bad value!")
                            )
                        |  _ -> Error ("bad termination")
                    )
                | None -> 
                    Error ("Unterminated")
            )
        in collect_rem after [ (float_of_int v) ]
    | TFloat v   ->  
        let rec collect_rem state' p  = 
            (*let next = advance state' in*)
            (match (fst state').curr with 
                | Some { tokn; _ } -> 
                    (match tokn with
                        | TComma -> 
                            collect_rem (advance state') p
                        | TNumeral n -> 
                            collect_rem (advance state') ((float_of_int n) :: p)
                        | TFloat f -> 
                            collect_rem (advance state') (f :: p)
                        | TRightBracket -> 
                            (* exit condition *)
                            Ok (advance state', (List.rev p))
                        |  KMinus -> 
                            let* (num, after) = takenum state' in 
                            (match num with 
                            | TFloat v -> 
                                collect_rem (after) ((v) :: p)
                            | TNumeral v -> 
                                collect_rem (after) ((float_of_int v) :: p)
                            | _ -> 
                                Error ("bad value!")
                            )
                            (*Error ("negative shapes not feasible!")*)
                        |  _ -> Error ("bad termination")
                    )
                | None -> 
                    Error ("Unterminated")
            )
        in collect_rem after [ v ]
    | _ -> 
        (Error "expected number in extraction")
;;

(* FIXME: replace parse_extract_shape with this one - requires handling the exit
   condition differently - likely with an enclose! *)
let parse_extract_shape_override state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (
            match tokn with 
            | TNumeral v ->  
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) (n :: p)
                                | TFloat _f -> 
                                    Error ("only numerals allowed in shape")
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (next, (List.rev p))
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | TFloat f ->  
                let v = int_of_float f in
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) (n :: p)
                                | TFloat _f -> 
                                    collect_rem (next) ((int_of_float _f) :: p)
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (next, (List.rev p))
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | t -> 
                Error (Format.sprintf "the shapes|bounds can only be natural number (>= 0): found %s" (show_ttype t))
        )
    | _ -> 
        (Error "expected number in extraction")
;;

(** DEPRECATED **)
let parse_extract_shape state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (
            match tokn with 
            | TNumeral v ->  
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) (n :: p)
                                | TFloat _f -> 
                                    Error ("only numerals allowed in shape")
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (advance next, (List.rev p))
                                |  KMinus -> Error ("negative size shapes not feasible!")
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | TFloat f ->  
                let v = int_of_float f in
                let rec collect_rem state' p  = 
                    let next = advance state' in
                    (match (fst next).curr with 
                        | Some { tokn; _ } -> 
                            (match tokn with
                                | TComma -> 
                                    collect_rem next p
                                | TNumeral n -> 
                                    collect_rem (next) (n :: p)
                                | TFloat _f -> 
                                    collect_rem (next) ((int_of_float _f) :: p)
                                | TRightBracket -> 
                                    (* exit condition *)
                                    Ok (advance next, (List.rev p))
                                |  KMinus -> Error ("negative size shapes not feasible!")
                                |  _ -> Error ("bad termination")
                            )
                        | None -> 
                            Error ("Unterminated")
                    )
                in collect_rem state [ v ]
            | _ -> 
                (Error "the shapes can only be natural number (>= 0)")
        )
    | _ -> 
        (Error "expected number in extraction")
;;

let parse_param_data _start next = 
    (if check TRange (fst next) then (
        let next' = advance next in 
        match current next' with 
        | Some { tokn; _ } ->  
            (match tokn with
                (* A1..B2 *)
                | TAlphaNum _end ->  
                    (>>==) (as_cell _start) (fun scell -> 
                        (>>==) (as_cell _end) (fun ecell -> 
                            Ok (advance next', Range (scell, ecell))
                        )
                    )
                (* A1..[100,200,...] [row, column] *)
                | TLeftBracket -> 
                    (>>==) (enclosed TLeftBracket TRightBracket (parse_extract_shape_override) next') 
                        (fun (final, shp) -> 
                            (>>==) (as_cell _start) (fun scell -> 
                                match shp with 
                                | [] -> 
                                    Ok (final, Span (scell, shp))
                                (* TODO: single shape makes it row based though this
                                   may cause some ambiguity with shapes *)
                                | x :: [] -> 
                                    Ok (final, Span (scell, [x;1]))
                                | _ -> 
                                    Ok (final, Span (scell, shp))
                            )
                        )
                (* A1..100 along a row *) 
                | TNumeral row -> 
                    if row < 0 then Error "Must be positive sized number in range"
                    else
                    (>>==) (as_cell _start) (fun scell -> 
                        Ok ((advance next'), Span (scell, [1; row]))
                    )
                | _ -> 
                    Error "Expected range end"
            )
        | _            -> 
            Error "Unclosed range"
    ) else (
            (* no range token  - maybe single cell*)
            (>>==) (as_cell _start) (fun y -> 
                Ok (next,  Scalar y)
            )
        )
    )
;;

let parse_ones_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                        (>>==) (consume after' TRightAngle) (fun final -> 
                                            Ok (final, Create (Ones shp))
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_zeros_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                        (>>==) (consume after' TRightAngle) (fun final -> 
                                            Ok (final, Create (Ones shp))
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_alt_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    let after = advance next in 
                    (match (fst after).curr with
                        | Some { tokn; _ } -> 
                            (match tokn with 
                                | TLeftBracket -> 
                                    (>>==) (parse_extract_range (advance after)) (fun (after', slc) -> 
                                        (>>==) (consume after' TComma) (fun final -> 
                                            (>>==) (parse_extract_shape (advance final)) (fun (after', shp) -> 
                                                (>>==) (consume after' TRightAngle) (fun final -> 
                                                    Ok (final, Create (Alt (slc, shp)))
                                                ) 
                                            )
                                        )
                                    )
                                | _ -> 
                                    Error "Expected shape spec"
                            )
                        | _ ->
                            Error "Missing shape spec"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_fill_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (takenum (advance next)) (fun (tok, next')-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Fill (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape filling spec"
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Fill (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error "Expected shape spec"
                                        )
                                    | _ ->
                                        Error "Expected shape fill spec"
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

(* TODO: support range syntax e.g 0..10 *)
let parse_enum_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (takenum (advance next)) (fun (tok, next')-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (>>==) (takenum (after)) (fun (tok, next')-> 
                                    (match tok with
                                        |TFloat incv ->  
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum (fval, incv, shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        |TNumeral incn -> 
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum (fval, (float_of_int incn) ,shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        | s ->
                                            Error (Format.sprintf "Expected numeral value, found %s" (show_ttype s))
                                    )
                                )
                            )
                        | TNumeral ival -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (>>==) (takenum (after)) (fun (tok, next')-> 
                                    (match tok with
                                        |TFloat incv ->  
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum ((float_of_int ival), incv, shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        |TNumeral incn -> 
                                            (>>==) (consume next' TComma) (fun after -> 
                                                (match (fst after).curr with
                                                    | Some { tokn; _ } -> 
                                                        (match tokn with 
                                                            | TLeftBracket -> 
                                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                                        Ok (final, Create (Enum ((float_of_int ival), (float_of_int incn) ,shp)))
                                                                    )
                                                                )
                                                            | _ -> 
                                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                                    | _ ->
                                                        Error "Expected shape filling spec"
                                                )
                                            ) 
                                        | s ->
                                            Error (Format.sprintf "Expected numeral value, found %s" (show_ttype s))
                                    )
                                )
                            )

                                    | _ -> 
                                        Error "Expected fill value as float"
                                )
                        | _ -> 
                            Error ("Expected shape spec in angle brackets")
                        )
                    | None -> 
                        Error ("Expected shape spec")
                )
;;

let parse_rand_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (takenum (advance next)) (fun (tok, next')-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final, Create (Rand (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected shape spec, found: %s" (show_ttype tokn)))
                                    | _ ->
                                        Error "Expected shape filling spec"
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                (match (fst after).curr with
                                    | Some { tokn; _ } -> 
                                        (match tokn with 
                                            | TLeftBracket -> 
                                                (>>==) (parse_extract_shape (advance after)) (fun (after', shp) -> 
                                                    (>>==) (consume after' TRightAngle) (fun final -> 
                                                        Ok (final,
                                                            Create (Rand (fval, shp)))
                                                    )
                                                )
                                            | _ -> 
                                                Error "Expected shape spec"
                                        )
                                    | _ ->
                                        Error "Expected shape fill spec"
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_diag_reference state = 
    let next = advance state in 
    (match (fst next).curr with 
        | Some { tokn; _ } -> 
            (match tokn with
                | TLeftAngle -> 
                    (>>==) (takenum (advance next)) (fun (tok, next')-> 
                        match tok with 
                        | TFloat fval -> 
                            (>>==) (consume next' TComma) (fun after -> 
                                let* num = takenum after in
                                (match (num) with
                                    (*| Some { tokn; _ } -> *)
                                        (*(match tokn with *)
                                            | TNumeral shp, after -> 
                                                (>>==) (consume (after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, shp)))
                                                )
                                            | TFloat shp, after -> 
                                                (>>==) (consume (after) TRightAngle) (fun final -> 
                                                    Ok (final, Create (Diag (fval, (int_of_float shp))))
                                                )
                                            | _ -> 
                                                Error (Format.sprintf "Expected diagonal size spec, found: %s" (show_ttype tokn))
                                    (*| _ ->*)
                                        (*Error "Expected shape filling spec"*)
                                )
                            )
                        | TNumeral ival -> 
                            let fval = float_of_int ival in
                            (>>==) (consume next' TComma) (fun after -> 
                                let* num = takenum after in
                                (match (num) with
                                    | (TNumeral shp, after) -> 
                                        (>>==) (consume (after) TRightAngle) (fun final -> 
                                            Ok (final, Create (Diag (fval, shp)))
                                        )
                                    | (TFloat shp, after) -> 
                                        (>>==) (consume (after) TRightAngle) (fun final -> 
                                            Ok (final, Create (Diag (fval, (int_of_float shp))))
                                        )
                                    | _ -> 
                                        Error (Format.sprintf "Expected diagonal size spec, found: %s" (show_ttype tokn))
                                )
                            )
                        | _ -> 
                            Error "Expected fill value as float"
                    )
                | _ -> 
                    Error ("Expected shape spec in angle brackets")
            )
        | None -> 
            Error ("Expected shape spec")
    )
;;

let parse_reference state = 
    match (fst state).curr with
    | Some { tokn;_ } -> 
        (match tokn with 
            | TAlphaNum "self" -> 
                Ok (advance state, Refer Self)
            (* TODO: for zeros and ones - reuse definition of fill! *)
            | TAlphaNum "ones" -> 
                parse_ones_reference state
            | TAlphaNum "zeros" -> 
                parse_zeros_reference state
            | TAlphaNum "fill" -> 
                parse_fill_reference state
            | TAlphaNum "rand" -> 
                parse_rand_reference state
            | TAlphaNum "enum" -> 
                parse_enum_reference state
            | TAlphaNum "eye" | TAlphaNum "diag" -> 
                parse_diag_reference state
            | TAlphaNum "alt" -> 
                parse_alt_reference state
            | TAlphaNum _start -> 
                (if validate _start then
                    let next = advance state in
                    (parse_param_data _start next)
                    else (Error "Invalid range value")
                )
            | tkn -> 
                Error (Format.sprintf "Unhandled reference: %s" (show_ttype tkn))
        )
    | _ -> 
        Error "Unfinished reference"
;;

let rec parse_relative dir state' = 
    (match (fst state').curr with
        | Some { tokn; _ } -> 
            (match tokn with 
                | TNumeral  _a -> 
                    let motn = _a in
                    (>>==) (parse_ein_params (advance state')) (fun (final, range) ->
                        Ok (final, Relative ((compass dir motn), range))
                    )
                | _ -> 
                    let motn = 1 in
                    (>>==) (parse_ein_params state') (fun (final, range) ->
                        Ok (final, Relative ((compass dir motn), range))
                    )
            )
        | _ -> 
            Error "Expected optional motion with cell spec but no tokens left"
    )

and parse_ein_params state = 
    match (fst state).curr with
    | Some { tokn; _ } -> 
        (match tokn with
            | TAlphaNum _start ->  
                (if validate _start then
                    let next = advance state in
                    (parse_param_data _start next)
                    else (Error "Invalid range value")
                )
            (* TODO: single scalar values ?? *)
            | TLeftBracket -> 
                (>>==) (parse_static_array (advance state)) (fun (x,y) ->  
                    Ok (x, NdArray y)
                )
            | TLeftAngle | TRightAngle | TUnderscore | TCaret -> 
                (parse_relative tokn (advance state))
            | TRightParen ->
                Ok (state, Void)
            | TAt -> 
                (parse_reference (advance state))
            | TComma   ->  
                (* NB: Trailing commas will add a Void *)
                Ok (state, Void)
            | TFloat fval -> 
                Ok (advance state, NdArray (Itemize ([ fval ])))
            | TNumeral ival -> 
                Ok (advance state, NdArray (Itemize ([ float_of_int ival ])))
            | KMinus -> 
                let* (num, state) = takenum state in 
                (match num with 
                | TFloat ival  -> 
                    Ok (advance state, NdArray (Itemize ([ ival ])))
                | TNumeral ival -> 
                    Ok (advance state, NdArray (Itemize ([ float_of_int ival ])))
                | _  -> 
                    Error "minus confusion!!"
                )
            | _ -> 
                Error "Bad token"
        )
    | None -> Error "badly terminated einsum expression"
;;

let parse_scatter_params state = 
    let* after, xslices = enclosed TLeftBracket TRightBracket (fun state' -> 
        (* get x,y as slices into the data *)
        (parse_extract_slice_indices state')
    ) (state) in 
    let* next = consume after TComma in
    let* proc, yslices = enclosed TLeftBracket TRightBracket (fun state' -> 
        (* get x,y as slices into the data *)
        (parse_extract_slice_indices state')
    ) (next) in 
    let* comm  = consume proc TComma in
    let* (final, props) = parse_key_value comm in
    Ok (final, Scatter { slices=[xslices;yslices]; props; })
;;


let parse_bar_params state = 
    let* after, slices = enclosed TLeftBracket TRightBracket (fun state' -> 
        (* get row,col as slices into the data *)
        (parse_extract_slice_indices state')
    ) (state) in 
    let* next = consume after TComma in
    let* proc, rng = (parse_ein_params next) in
    let* comm  = consume proc TComma in
    let* (final, props) = parse_key_value comm in
    Ok (final, Bar { data=slices; ylabl=rng; props; })
;;

let parse_plot_params state = 
    (match (fst state).curr with 
        | Some ({ tokn=(TAlphaNum plottype); _ }) -> 
            (match plottype with 
                | "scatter" -> 
                    enclosed TLeftAngle TRightAngle (parse_scatter_params) (advance state)
                | "line" -> 
                    Ok ((advance state), Line (Hashtbl.create 2))
                | "hist" -> 
                    Ok ((advance state), Hist (Hashtbl.create 2))
                | "pie" -> 
                    Ok ((advance state), Pie  (Hashtbl.create 2))
                | "bar" -> 
                    enclosed TLeftAngle TRightAngle (parse_bar_params) (advance state)
                | _ -> 
                    Error "unrecognized plot"
            )
        | _ -> 
            Error "expected a plot type"
    )
;;

let parse_ref_angle_var state = 
    (match (fst state).curr with 
        | Some  {tokn; _} ->
            Ok (advance state, tokn)
        | _ -> 
            (Error "Expected angle variable!")
    )
;;

let parse_num state = 

    let* num = takenum state in
    (match num with 
        | ((TNumeral _) as tokn, after) ->
            (Ok (after, tokn))
        | ((TFloat _) as tokn, after) -> 
            (Ok (after, tokn))
        | _ ->
                Error ("expected number")
    )
;;

let parse_draw_params state = 
    (match current state with
    | Some ({ tokn=(TAlphaNum handle); _ }) -> 
            (>>==) (consume (advance state) TComma) (fun state' -> 
                (>>==) (enclosed TLeftBracket TRightBracket (parse_extract_shape_override) state') (fun (state', bounds) -> 
                    (>>==) (consume state' TComma) (fun state' ->
                        (match (current state') with 
                            | Some { tokn=(TAlphaNum drw);_ } ->
                                if drw = "clear" then 
                                    Ok (advance state', Draw { handle; bounds; elmnts=[ Clear ] })
                                else if drw = "reset" then 
                                    Ok (advance state', Draw { handle; bounds; elmnts=[ Reset ] })
                                else 
                                    (>>==) (parse_key_value (advance state')) (fun (after, props) -> 
                                        (match drw with 
                                            | "box" | "rect" -> Ok (after, Draw({
                                                handle; bounds; elmnts=[ (Box props) ] })) 
                                            | "circle" -> Ok (after, Draw({ handle; bounds; elmnts=[ (Circle props)] }))
                                            | "line" ->   Ok (after, Draw({ handle; bounds; elmnts=[ (Line props) ] }))
                                            | "text" ->   Ok (after, Draw({ handle; bounds; elmnts=[ Text props ] }))
                                            | _ -> Error ("unknown draw call " ^ drw)
                                        )
                                    )
                            | _ -> 
                                Error ("expected draw type (box, line, circle ...)")
                        )
                    )
                ) 
            )
    | _ -> 
        Error "expected draw handle title"
    )
;;

let parse_collect_draw_params state = 
    (match current state with
        | Some ({ tokn=(TAlphaNum handle); _ }) -> 
            (>>==) (consume (advance state) TComma) (fun state' -> 
                (>>==) (enclosed TLeftBracket TRightBracket (parse_extract_shape_override) state') (fun (state', bounds) -> 
                    (>>==) (consume state' TComma) (fun state' ->
                        enclosed TLeftBracket TRightBracket (fun state' -> 
                            let rec collect nxt buf = 
                                (match (current nxt) with 
                                    | Some { tokn=(TAlphaNum drw);_ } ->
                                        if drw = "clear" then 
                                            collect (advance nxt) (Clear :: buf) 
                                        else if drw = "reset" then 
                                            collect (advance nxt) (Reset :: buf) 
                                        else 
                                            let* after, props = (parse_key_value (advance nxt))  in
                                            (match drw with 
                                                | "box" | "rect"    -> 
                                                    collect after ((Box props) :: buf) 
                                                | "circle" -> 
                                                    collect after ((Circle props) :: buf)
                                                | "line" ->   
                                                    collect after ((Line props) :: buf)
                                                | "text" -> 
                                                    collect after ((Text props) :: buf)
                                                | _ -> 
                                                    Error ("unknown draw call " ^ drw)
                                            )
                                    | Some { tokn=(TRightBracket);_ } ->
                                        Ok (nxt, buf)
                                    | Some { tokn=(TComma);_ } ->
                                        collect (advance nxt) buf
                                    | _ -> 
                                        Error ("expected draw type (box, line, circle ...)")
                                ) 
                            in 
                            (>>==) (collect state' []) (fun (after, elmnts) -> 
                                Ok (after, Draw { handle; bounds; elmnts=(List.rev elmnts) })
                            )
                        ) state'
                    )
                )
            )
        | _ -> 
            Error "expected draw handle title"
    )
;;

let parse_ein_mask state = 
    (* TODO: support arbitrary expression operations *)
    let rec masklist state lst = 
        (match (fst state).curr with 
            | Some { tokn; _ } -> 
                (>>==) (match tokn with 
                    (* maps to same shape *)
                    | TAlphaNum "zscore" -> 
                        Ok (advance state, ZScore)
                    | TAlphaNum "write" -> 
                        let nxt = advance state in
                        if check TLeftAngle (fst nxt) then
                            let nxt' = advance nxt in 
                            (match (fst nxt').curr with 
                            | Some  { tokn; _ } -> 
                                (match tokn with 
                                    | TAlphaNum _end ->  
                                        (>>==) (as_cell _end) (fun ecell -> 
                                            (>>==) (consume (advance nxt') TRightAngle) (fun final -> 
                                                Ok (final, (Write ecell))
                                            )
                                        )
                                    | _ -> 
                                        Error "Expected range end"
                                ) 
                            | None -> 
                                Error "missing cell argument"
                            )
                        else
                            Error "expected write cell argument in angle brackets"
                    | TAlphaNum "minmax" ->
                        let nxt = advance state in
                        if check (TLeftAngle) (fst nxt) then
                            (>>==) (parse_num (advance nxt)) (fun (after, num1) -> 
                                (>>==) (consume  after TComma) (fun nxt -> 
                                    (>>==) (parse_num nxt) (fun (after', num2) -> 
                                        (match (num1, num2) with 
                                        | (TFloat vala, TFloat valb) -> 
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok ((after'), MinMax(vala, valb))
                                            )
                                        | (TNumeral vala, TNumeral valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax((float_of_int vala), (float_of_int valb)))
                                            )
                                        | (TFloat vala, TNumeral valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax(vala, (float_of_int valb)))
                                            )
                                        | (TNumeral vala, TFloat valb) ->
                                            (>>==) (consume after' TRightAngle) (fun after' ->
                                                Ok (after', MinMax((float_of_int vala), valb))
                                            )
                                        | _ -> 
                                            Error "unreachable condition!!"
                                        )
                                    )
                                )
                            )
                        else
                            Ok (advance state, MinMax(-1., 1.))
                    | TAlphaNum "reshape" ->
                        let nxt = advance state in
                        if check (TLeftAngle) (fst nxt) then
                            let nxt' = advance nxt in
                            if check TLeftBracket (fst nxt') then 
                                (>>==) (parse_extract_shape (advance nxt')) (fun (after, shp) -> 
                                    (>>==) (consume after TRightAngle) (fun after' ->
                                        Ok (after', Reshape shp)
                                    )
                                ) 
                            else
                                Error "reshape value should be in shape format"
                        else
                            Error "missing reshape values!"
                    | TAlphaNum "slice" ->
                        let nxt = advance state in
                        (if check TLeftAngle (fst nxt) then 
                            let nxt' = advance nxt in
                            (match (fst nxt').curr with 
                            | Some ({ tokn=TLeftBracket; _ }) ->  
                                (>>==) (parse_extract_slice_indices (advance nxt')) 
                                (fun (after, slices) -> 
                                    (>>==) (consume (advance after) TRightAngle) (fun after -> 
                                        Ok (after, Slice slices)
                                    )
                                )
                            | _ -> 
                                Error "expected slice parameters in brackets"
                            )
                        else Error "missing slice parameters!")
                    | TAlphaNum "draw" -> 
                        (enclosed TLeftAngle TRightAngle (parse_draw_params) (advance state)) 
                    | TAlphaNum "drawall" -> 
                        (enclosed TLeftAngle TRightAngle (parse_collect_draw_params) (advance state)) 
                    | TAlphaNum "plot" ->
                        (enclosed TLeftAngle TRightAngle (fun nxt' -> 
                            (match (fst nxt').curr with 
                                | Some ({ tokn=(TAlphaNum handle); _ }) ->  
                                    let*  nxt' = consume (advance nxt') TComma in
                                    let* (proc, bounds) = (enclosed TLeftBracket TRightBracket (parse_extract_shape_override) nxt') in
                                    let* comm = consume proc TComma in
                                    ((>>==) ((parse_plot_params comm))
                                        (fun (after, oftype) -> 
                                            Ok (after, Plot { handle; bounds; oftype; })
                                        )
                                    )
                                | _ -> 
                                    Error "expected plot title and parameters"
                            )
                        ) (advance state))
                    | TAlphaNum "axis" ->
                        let nxt = advance state in
                        if check (TLeftAngle) (fst nxt) then
                            let nxt' = advance nxt in
                            (match (fst nxt').curr with
                                | Some { tokn; _ } -> 
                                    (match tokn with
                                        | TAlphaNum "row" -> 
                                            (>>==) (consume (advance nxt') TComma) (fun nxt'' -> 
                                                (>>==) (masklist nxt'' []) 
                                                    (fun (after, masks) ->
                                                        (>>==) (consume after TRightAngle) (fun after' ->
                                                            Ok (after', Axis (0, masks))
                                                        )
                                                    )
                                            )
                                        | TAlphaNum "column" | TAlphaNum "col" -> 
                                            (>>==) (consume (advance nxt') TComma) (fun nxt'' -> 
                                                (>>==) (masklist nxt'' []) 
                                                    (fun (after, masks) ->
                                                        (>>==) (consume after TRightAngle) (fun after' ->
                                                            Ok (after', Axis (1, masks))
                                                        )
                                                    )
                                            )
                                        | TNumeral axischar -> 
                                            (>>==) (consume (advance nxt') TComma) (fun nxt'' -> 
                                                (>>==) (masklist nxt'' []) 
                                                    (fun (after, masks) ->
                                                        (>>==) (consume after TRightAngle) (fun after' ->
                                                            Ok (after', Axis (axischar, masks))
                                                        )
                                                    )
                                            )
                                        | _ -> 
                                            Error "expected axis number first: must be a natural number >= 0"
                                    )
                                | _ -> 
                                    Error "axis description missing"
                            )
                        else
                            Error "missing axis argument!"
                    (* reductions to Scalar *)
                    | TAlphaNum "mean" ->
                        Ok (advance state, Mean)
                    | TAlphaNum "min" ->
                        Ok (advance state, Min)
                    | TAlphaNum "max" ->
                        Ok (advance state, Max)
                    | TAlphaNum "sum" ->
                        Ok (advance state, Sum)
                    | TAlphaNum "cumsum" ->
                        Ok (advance state, Cumsum)
                    (* will be in radians *)
                    | TAlphaNum "sin" ->
                        Ok (advance state, Map(Float.sin))
                    | TAlphaNum "sinh" ->
                        Ok (advance state, Map(Float.sinh))
                    | TAlphaNum "cos" ->
                        Ok (advance state, Map(Float.cos))
                    | TAlphaNum "cosh" ->
                        Ok (advance state, Map(Float.cosh))
                    | TAlphaNum "tan" ->
                        Ok (advance state, Map(Float.tan))
                    | TAlphaNum "tanh" ->
                        Ok (advance state, Map(Float.tanh))
                    | TAlphaNum "asin" ->
                        Ok (advance state, Map(Float.asin))
                    | TAlphaNum "asinh" ->
                        Ok (advance state, Map(Float.asinh))
                    | TAlphaNum "atan" ->
                        Ok (advance state, Map(Float.atan))
                    | TAlphaNum "atanh" ->
                        Ok (advance state, Map(Float.atanh))
                    | TAlphaNum "exp" ->
                        Ok (advance state, Map(Float.exp))
                    | TAlphaNum "log" ->
                        Ok (advance state, Map(Float.log))
                    | TAlphaNum "sqrt" ->
                        Ok (advance state, Map(Float.sqrt))
                    | TAlphaNum "clip" | TAlphaNum "clamp" ->
                        (>>==) (enclosed TLeftAngle TRightAngle (fun s -> 
                            (match current s with 
                            | Some { tokn=(TNumeral p); _ } ->
                                    (>>==) (consume (advance s) TComma) (fun s' -> 
                                        (match current s' with 
                                            | Some { tokn=(TNumeral p'); _ } ->
                                                Ok (advance s', (float_of_int p, float_of_int p'))
                                            | Some { tokn=(TFloat p'); _ } ->
                                                Ok (advance s', (float_of_int p, p'))
                                            | _ -> 
                                                Error "min and max number"
                                        )
                                    )
                            | Some { tokn=(TFloat p); _ } ->
                                    (>>==) (consume (advance s) TComma) (fun s' -> 
                                        (match current s' with 
                                            | Some { tokn=(TNumeral p'); _ } ->
                                                Ok (advance s', (p, float_of_int p'))
                                            | Some { tokn=(TFloat p'); _ } ->
                                                Ok (advance s', (p, p'))
                                            | _ -> 
                                                Error "min and max number"
                                        )
                                    )
                            | _ -> 
                                Error "clamp | clip expects min and max number"
                            )
                        ) (advance state))
                        (fun (state', (clampmin, clampmax)) -> 
                            Ok (state', Map (fun v -> 
                                if v < (clampmin) then  
                                    clampmin
                                else if v > (clampmax) then 
                                    clampmax 
                                else
                                    v
                            ))
                        )
                    | TAlphaNum "logsumexp" ->
                        (>>==) (enclosed TLeftAngle TRightAngle (fun s -> 
                            (match current s with 
                            | Some { tokn=(TNumeral p); _ } ->
                                Ok (advance s, float_of_int p)
                            | Some { tokn=(TFloat p); _ } ->
                                Ok (advance s, p)
                            | _ -> 
                                Error "logsumexp expects number"
                            )
                        ) (advance state))
                        (fun (state', beta) -> 
                            Ok (state', LogSumExp beta)
                        )
                    | TAlphaNum "softmax" ->
                        (>>==) (enclosed TLeftAngle TRightAngle (fun s -> 
                            (match current s with 
                            | Some { tokn=(TNumeral p); _ } ->
                                Ok (advance s, float_of_int p)
                            | Some { tokn=(TFloat p); _ } ->
                                Ok (advance s, p)
                            | _ -> 
                                Error "softmax expects number"
                            )
                        ) (advance state))
                        (fun (state', beta) -> 
                            Ok (state', Softmax beta)
                        )
                    | TAlphaNum "pow" ->
                        (>>==) (enclosed TLeftAngle TRightAngle (fun s -> 
                            (match current s with 
                            | Some { tokn=(TNumeral p); _ } ->
                                Ok (advance s, float_of_int p)
                            | Some { tokn=(TFloat p); _ } ->
                                Ok (advance s, p)
                            | _ -> 
                                Error "power expects number"
                            )
                        ) (advance state))
                        (fun (state', num) -> 
                            Ok (state', Map (fun v -> Float.pow v num))
                        )
                    | TAlphaNum "inv" ->
                        Ok (advance state, Map((/.) 1.))
                    | TAlphaNum "ceil" ->
                        Ok (advance state, Map(Float.ceil))
                    | TAlphaNum "floor" ->
                        Ok (advance state, Map(Float.floor))
                    | TAlphaNum "stddev" ->
                        Ok (advance state, Stddev)
                    | TAlphaNum "mode" ->
                        Ok (advance state, Mode)
                    | t -> 
                        Error (Format.sprintf "Unknown mask function: %s" (show_ttype t))
                ) (fun (state', mask) -> 
                        if check TPipe (fst state') then
                            (masklist (advance state') (mask :: lst))
                        else
                            Ok (state', List.rev (mask :: lst))
                    )
            | None -> 
                Error "expected mask function"
        ) in 
    masklist state []
;;

let parse_einsum pratt = 
    let rec _parse ein state = 
        (match (fst state).curr with
            | Some ({ tokn; _ }) -> 
                (match tokn with 
                    | TAlphaNum v -> 
                        (if validate v then (
                            let (prt, rem') as state' = advance state in
                            if check TComma prt then
                                _parse (parse_ein_inp ein v []) (advance state') 
                            else if check TPipe prt then
                                (>>==) (parse_ein_mask (advance state')) (
                                    fun ((prt', rem'') as state'', ml) -> 
                                    if check TComma (fst state'') then 
                                        _parse (parse_ein_inp ein v ml) (advance state'') 
                                    else
                                        Ok (prt', ((reorder @@ parse_ein_inp ein v ml), []), rem'')
                                )
                            else 
                                (* no params *)
                                Ok (prt, ((reorder @@ parse_ein_inp ein v []), []), rem')
                        ) else (
                                Error (Format.sprintf "Input indices invalid - please use at least one ascii chars at %s" 
                                    (show_prattstate @@ fst state))
                            )
                        )
                    | _ -> 
                        let prt', rem' = state in
                        Ok (prt', ((reorder ein), []), rem')
                    (*Error (Format.sprintf "Unimplemented at %s" (show_prattstate (fst state)))*)
                )
            | _ -> 
                Error (Format.sprintf "Unfinished einsum expression at %s" (show_prattstate (fst state)))
        )
    in
    (>>==) (_parse einempty pratt) (fun (current, ein, lxm) -> 
        (* output of the einsum - NOT the parameters! *)
        (if check TArrow (current) then (
            let next = advance (current, lxm) in 
            (match (fst next).curr with 
                | Some ({ tokn; _ }) -> 
                    (match tokn with
                        | TAlphaNum v -> 
                            (* Grab the last einsum and update the output *)
                            (*let ein' = (fst next).prog in*)
                            let next' = advance next in
                            if check TPipe (fst next') then
                                (>>==) (parse_ein_mask (advance next')) (
                                    fun (state'', ml) -> 
                                    let upd  = parse_ein_out (fst ein) v ml in
                                    Ok (state'', (upd, snd ein))
                                )
                            else
                                let upd  = parse_ein_out (fst ein) v [] in
                                Ok (next', (upd, snd ein))
                        | _ -> 
                            (* it was not a token - likely something closing like a Comma or RightParen *)
                            (* not what we expected - we just leave as is -  *)
                            Ok (next, ein)
                    )
                | _ -> 
                    Error "Unexpected einsum result"
            )
        ) else (
                (* arrow can be optional *)
                Ok ((current, lxm), ein) 
            ))
    )
;;

let parse_einsum_expr pratt = 
    (>>==) (parse_einsum pratt) (fun (state, ein) -> 
        let p = fst state in 
        let t = snd state in 
        Ok ({ p with prog=(Stmt { 
            source="";
                prog=Literal (EinSpec (fst ein, snd ein, None)) 
            ;   inputs=[]; writes=[]; stamp=(Unix.gettimeofday ())
        }) }, t)
    )
;;

let parse_einsum_formulae state = 
    let rec _extract (state, (ein, par)) =
        (if check TComma (fst state) then 
            ((>>==) (parse_ein_params (advance state)) (fun (next, rnge) -> 
                (* check for masks which are piped to parameters *)
                if check TPipe (fst next) then
                    (>>==) (parse_ein_mask (advance next)) (fun (next', ml) -> 
                        _extract (next', (ein, (Mask (rnge, ml)) :: par))
                    )
                else
                    _extract (next, (ein, rnge :: par))
            ))
            (*(Fun.compose _extract add_crange)) *)
            (* a right paren shows the end of parameter sequence - dont advance in this case *)
            else if not @@ check TRightParen (fst state) then 
                ((>>==) (parse_ein_params (state)) (fun (next, rnge) -> 
                    (* let the call order reflect how it written for einsum parameters *)
                    _extract (next, (ein, rnge :: par))
                ))
            else (Ok (state, (ein, (List.rev par))))
        );
    in (>>==) (parse_einsum state) (_extract)
;;

let parse_formulae state = 
    (* will overwrite any previous data *)
    (>>==) (parse_einsum_formulae state) (fun (x, y) -> 
        let p = fst x in 
        let t = snd x in
        Ok ({ p with prog=(Stmt {
            source="";
            prog=(Literal (EinSpec (fst y, snd y, None))) 
            ;   inputs=[]; writes=[]; stamp=(Unix.gettimeofday ())
        }) }, t)
    )
;;

let parse_expression state = 
    let rec _term state' =
        let* (lexpr, termseq') = _factor state'  in
        let rec check  l_expr ts = 
            (match current ts with
                | Some ({ tokn=KMinus; _ }) ->
                    let* r_expr, _ts' = _term (advance ts) in
                    (match (l_expr, r_expr) with 
                        | (Literal _l, Binary (_, (Factor _), _)) ->   
                            let lexpr' = (Binary (l_expr, (Term Sub), (Grouping r_expr))) in
                            check  lexpr' _ts'
                        | (Literal l, Binary (l', (Term _t), r')) ->   
                            let lexpr' = (Binary ((Grouping (Binary (Literal l, (Term Sub), l'))), (Term _t), r')) in
                            check  lexpr' _ts'
                        | (Literal _l, Reduce (l', r')) ->   
                            let lexpr' = (Reduce ((Binary (l_expr, (Term Sub), l')), r')) in
                            check  lexpr' _ts'
                        | _ -> 
                            let lexpr' = (Binary (l_expr, (Term Sub), r_expr)) in
                            check  lexpr' _ts'
                    )
                | Some ({ tokn=KPlus; _ }) ->
                    let* r_expr, _ts' = _term (advance ts) in
                    (*let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in*)
                    (match (l_expr, r_expr) with 
                        | (Literal _l, Reduce (l', r')) ->   
                            let lexpr' = (Reduce ((Binary (l_expr, (Term Add), l')), r')) in
                            check  lexpr' _ts'
                        | _ -> 
                            let lexpr' = (Binary (l_expr, (Term Add), r_expr)) in
                            check  lexpr' _ts'
                    )
                    (*check  lexpr' _ts'*)
                | _ -> 
                    Ok (l_expr, ts)
            )
        in check  lexpr termseq' 

    and _factor state' = 
        let* (lexpr, facseq') = _unary state'  in
        let rec check  l_expr ts = 
            (*FIXME: precedence not working properly! *)
            match  current ts with
            | Some ({ tokn=(KDiv); _ }) ->
                let* r_expr, _ts' = _term  (advance ts) in
                (match (l_expr, r_expr) with
                    | (Literal _l, Binary (l', (Factor Mul), r')) -> 
                        let lexpr' = (Binary ((Grouping (Binary (l_expr, (Factor Div), l'))), (Factor Mul), r')) in
                        check  lexpr' _ts'
                    | (Literal _l, Binary (l', (Term op'), r')) -> 
                        let lexpr' = (Binary ((Grouping (Binary (l_expr, (Factor Div), l'))), (Term op'), r')) in
                        check  lexpr' _ts'
                    | (Literal _l, Reduce (Binary (l', (Factor Mul), r'), m)) -> 
                        let lexpr' = (Reduce ((Binary ((Grouping (Binary (l_expr, (Factor Div), l'))), (Factor Mul), r')), m)) in 
                        check  lexpr' _ts'
                    | (Literal _l, Reduce (Binary (l', (Term op'), r'), m)) -> 
                        let lexpr' = (Reduce ((Binary ((Grouping (Binary (l_expr, (Factor Div), l'))), (Term op'), r')), m)) in 
                        check  lexpr' _ts'
                    | (Literal _l, Reduce (l', r')) -> 
                        let lexpr' = (Reduce ((Binary (l_expr, (Factor Div), l')), r')) in 
                        check  lexpr' _ts'
                    | _ -> 
                        let lexpr' = (Binary (l_expr, (Factor Div), r_expr)) in
                        check  lexpr' _ts'
                )
            | Some  ({ tokn=(KMult);_ })-> 
                let* r_expr, _ts' = _term  (advance ts) in
                (match (l_expr, r_expr) with
                    | (Literal _l, Binary (l', (Term op'), r')) -> 
                        let lexpr' = (Binary (Grouping (Binary (l_expr, (Factor Mul), l')), (Term op'), r')) in 
                        check  lexpr' _ts'
                    | (Literal _l, Reduce (Binary (l', (Term op'), r'), m)) -> 
                        let lexpr' = (Reduce ((Binary (Grouping (Binary (l_expr, (Factor Mul), l')), (Term op'), r')), m)) in 
                        check  lexpr' _ts'
                    | (Literal _l, Reduce (l', r')) -> 
                        let lexpr' = (Reduce ((Binary (l_expr, (Factor Mul), l')), r')) in 
                        check  lexpr' _ts'
                    | _ ->   
                        let lexpr' = (Binary (l_expr, (Factor Mul), r_expr)) in 
                        check  lexpr' _ts'
                )
            | Some  ({ tokn=(TPipe);_ })-> 
                let* _ts', r_expr = parse_ein_mask (advance ts) in
                let lexpr' = (Reduce (l_expr, r_expr)) in 
                check  lexpr' _ts'
            | Some  (_t) ->
                Ok (l_expr, ts)
            | _ ->
                Ok (l_expr, ts)
        in check  lexpr facseq' 

    and _unary state' = 
        (match current state' with
            | Some ({ tokn=(KMinus); _  }) ->
                let* r_expr, _ts' = _unary (advance state')  in
                Ok (Unary (Negate, r_expr), _ts')
            | _ ->
                _primary state' 
        ) 

    and _primary state' = 
        (match current state' with 
            | Some { tokn=(TNumeral n); _  } ->
                Ok ((Literal (Number (float_of_int n))), (advance state'))
            | Some { tokn=(TFloat n); _ } ->
                Ok ((Literal (Number n)), (advance state'))
            | Some { tokn=(TLeftParen); _ } ->
                let* (lit, next) = _extract_group (advance state') in
                let* next = (consume next TRightParen) in
                Ok (Grouping lit, next) 
            | Some { tokn=(TAt); _ } -> 
                let* (next, tns) = (parse_reference (advance state')) in 
                Ok (Literal (Tensor tns), next)
            | Some { tokn=(TLeftBracket); _ } -> 
                (* auto-advances the state *)
                let* (next, tns) = (parse_static_array (advance state')) in 
                Ok (Literal (Tensor (NdArray tns)), next)
            | Some { tokn=(TAlphaNum _n); _ } ->
                let* (next, (e, p)) = (parse_einsum_formulae (state')) in
                Ok (Literal (EinSpec (e, p, None)), next)
            | Some { tokn=t; _ } ->
                Error (Format.sprintf "unexpected token %s !" (show_ttype t)) 
            | _n -> 
                Error (Format.sprintf "unhandled null expression") 
        )

    and _extract_group state' = 
        _term state'
    in 
    let* (form, (rem, left)) =  (>>==) (_extract_group state) (fun (x, y) ->
        (>>==) (consume_optional TSemiColon y) (fun y -> 
            Ok (x, y)
        )
    )
    in 
    Ok ({ rem with prog=(Stmt { source=""; prog=(form); inputs=[]; writes=[]; stamp=(Unix.gettimeofday ()) }) }, left)
;;

(* TODO: make errors some easily parseable and serializable type showing expected and current states *)
let parse lstream src = 
    let _parse_lxms current = 
        parse_expression current
    in
    (>>==) (_parse_lxms @@ advance (prattempty, lstream)) (fun ({ prog=(Stmt s);_ } as  p, l) -> 
        Ok ({ p  with prog=(Stmt { s with source=src; }) }, l)
    )
;;
