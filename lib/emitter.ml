(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
(*
    (I[3] -> K[3])
    (i in 0..3) { K[i] = I[i] }

    (I[3] -> K[1])
    (i in 0..3) { K[i] += I[i] }

    (I[3],I[3] -> II[3]) 
    (i in 0..3) { II[i] += I[I] * I[i] }

    - block scope
    - declaration
    - initialization
    - condition
    - increment

*)

open Genfunc;;
open Ndarray;;

(* stack values *)
type spinval = 
    | SNil
    | SNumber of float
    | SIndex  of int
    | SBool   of bool
    | SStr    of string
    | SNdim:  ((module NDarray with type t = 'cont and type e = 'elt) * 'cont) -> spinval 
;; 

let shape_of_module (type n) (type b) (module N: NDarray with type t = n and type e = b) v = 
    N.shape v
;;

let show_spinval s = 
    match s with
    | SNil      -> "nil"
    | SNumber f -> Format.sprintf "%f" f
    | SIndex  i -> Format.sprintf "%d" i 
    | SBool   b -> Bool.to_string b 
    | SStr    s -> s 
    | SNdim  (d, n) -> Genfunc.string_of_dim @@ (shape_of_module d n)
;;

let pp_spinval _f _s = 
    ()
;;

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | SIndex x',  SIndex  y' -> SIndex  (x' + y')
    | _ -> failwith "Invalid add operands"
;;

let seql x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.equal x' y')
    | SIndex  x', SIndex y' ->  SBool (Int.equal x' y')
    | SBool   x', SBool y' ->   SBool (Bool.equal  x' y')
    | _ -> failwith "Invalid operands for equal"
;;

let strue x = 
    match x with 
    | SBool y' -> (Bool.equal y' true)
    | _ -> failwith "Expected bool operand!"
;;

let smul x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' *. y')
    | SIndex x', SIndex y'   -> SIndex (x' * y')
    | _ -> failwith "Invalid mul operands"
;;

let snot x = 
    match x with 
    | SBool b -> SBool (not b)
    | _ -> failwith "invalid boolean not operand"
;;

let sless x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (-1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (-1))
    | _ -> failwith "Invalid add operands"
;;

let sgreater x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (1))
    | _ -> failwith "Invalid add operands"
;;

type instr = 
    (* arith *)
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    | IPush      of spinval
    (* logic *)
    (*| ITrue*)
    (*| IFalse*)
    | INot               (* boolean inversion *)
    | ILess              (* Lesser than *)
    | IGreater           (* Greater than *)
    (* jumping *)
    | IJump      of int  (* Jump  *)
    | IJumpFalse of int ref  (* Jump if false *)
    | ILoop      of int  (* Jump to a specific location directly ? *)
    (* var ops *)
    | IConst     of int  (* load constant from position int onto the stack *)
    | IGetVar    of int  (* get the variable at a certain displacement from the stack index *)
    | ISetVar    of int  (* set value at certain displacement from stack index *)
    (* effects *)
    | IEcho              (* print value at the top of the stack *)
    | IEchoNl
[@@deriving show];;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
};;

(* TODO: unsafe assert *)
let get_const idx { consts; _ } = 
    Array.get consts idx
;;

type presource = {
        consts: spinval list
    ;   oprtns: instr   list
    ;   cnsidx: int
    ;   nmdvar: (char, int) Hashtbl.t  [@opaque]
    ;   varcnt: int
    ;   name:   string
    ;   args:   Parser.crange list
    ;
} [@@deriving show];;


let presempty name = {
        consts=[] 
    (*;   ptrarr=[]*)
    ;   oprtns=[]
    ;   cnsidx=0
    ;   nmdvar=(Hashtbl.create 8) 
    ;   varcnt=0
    ;   args=[]
    ;   name
} [@@deriving show];;

(* todo: interning! *)
let add_const vlue ps = 
    (ps.cnsidx, { ps with consts = vlue :: ps.consts; cnsidx=ps.cnsidx+1 })
;;

let add_named_var vlue ps = 
    match Hashtbl.find_opt ps.nmdvar vlue with 
    | Some idx ->  (idx, ps) 
    | None ->
        let _ = Hashtbl.add ps.nmdvar vlue ps.varcnt in
        (ps.varcnt, { ps with varcnt=ps.varcnt+1 })
;;

let get_named_var vlue ps = 
    Hashtbl.find_opt ps.nmdvar vlue 
;;

let echoall vlist =
    vlist
    |> List.rev
    |> List.map (fun (var, idx) -> 
        [
            IPush (SStr (Format.sprintf "%c" var));
            IEcho;
            IGetVar idx;
            IEcho;
            IPush (SStr " ");
            IEchoNl;
            IPop;
            IPop;
            IPop;
        ]
    ) 
    |> List.concat
;;

(* 
   create a loop 

   vrn:    loop variable name (i j k ...)
   count:  loop upper bound
   ps:     presource data
   lidx:   the index in the instruction source where this loop starts
   body:   body of the loop
*)

let oplen ps =
    List.length ps.oprtns
;;

let rec iterndarray nest f nda = 
    match nda with 
    | Parser.Itemize a -> List.iteri (fun l c -> (f (l :: nest) c)) a
    | Parser.Collect c -> List.iteri (fun i c' -> (iterndarray (i :: nest) f c') ) c
;;


let range_to_ndarray: (Parser.crange -> int list -> spinval) = fun n _shp -> 
    (* spreadsheet cell *)
    match n with 
    | Parser.NdArray (_ndfl) -> (
        let _scal = (module Scalar(Float): NDarray with type t = float ref and type e = float) in 
        let (module Scalar: NDarray with type t = float ref and type e = float) = _scal in
        let _sdat = Scalar.make [||] 0. in
        (SNdim (_scal, _sdat))
    )
    | _ -> failwith "not implemented"
    (*| Range    (_frcell, _tocell) -> () *)
    (*| Relative (_motion, _crange) -> () *)
    (*| Refer    (_referral) -> () *)
    (*| Void ->  ()*)
;;



(* generates nested loops from a list of variable matches *)
let genloop ps (parms: Parser.crange list) =

    (* load params into the  stack frame first as if they were function call arguments *)
    (* TODO: tranform into NDARRAYs *)
    let ps = { ps with args=parms } in


    fun (pre) (post) (body) vlist -> 
        let rec genl ({ label=vrn; dimen=bound; _ } as ein) ps lidx decl rem =
            let (sidx, ps) = add_const (SIndex 0)     ps in (* count from 0 *)
            let (sinc, ps) = add_const (SIndex 1)     ps in (* increment by 1 *)
            let (eidx, ps) = add_const (SIndex bound) ps in (* end at  eidx *)
            let (vidx, ps) = add_named_var vrn ps in
            (* calculated as 1 jump instr + 6 for increment + 1 loop *)
            let jmp        = ref 8 in
            let ps'        = (
                (* do what you want pre body invocation 
                   pass the state with current variable, remaining vars and
                   startindex for instructions in this loop *)
                let prebody = pre ps ein rem lidx in
                let body = (
                    match rem with 
                    | [] -> 
                        let islast = true in 
                        (*{ prebody with oprtns=(echoall ((vrn, vidx) :: decl)) }*)
                        body islast ein (prebody)
                    | hd' :: rst -> 
                        let islast = false in 
                        (* build the inner loop *)
                        body islast ein (genl hd' prebody (lidx + 11) ((vrn, vidx) :: decl) rst) 
                ) in
                (* do what you want post body invocation *)
                post body ein 
            ) in
            let _ = jmp := (!jmp + oplen ps') in
            { 
                ps' with 
                oprtns=
                    [ 

                        (* load the indexes *)
                        IConst  sidx; 
                        IGetVar vidx; 

                        IConst  eidx; 
                        ILess       ; 

                        (* jump out of the loop *)
                        IJumpFalse jmp;

                        (* jump over the increment*)
                        IJump       6; 

                        (* increment -> loop back here after executing loop body *)
                        IGetVar    vidx;
                        IConst     sinc; 
                        IAdd;         
                        ISetVar    vidx;   (* also pops the stack *)
                        ILoop      (lidx + 1); 
                        (* end increment *)
                    ]
                    (* do loop operations here *)
                    @ ps'.oprtns @
                    (* end loop operations *)
                    [

                        (* Go to the increment *)
                        ILoop  (lidx + 6);

                        (* pop the named variable *)
                        IPop; 
                    ]
            }
        in 
        match vlist with 
        | [] -> ps
        | hd :: rest -> genl hd ps 0 [] rest
;;

let convert (s: presource) = 
    {
        oprtns=(Array.of_list s.oprtns) 
        ;   consts=(Array.of_list @@ List.rev s.consts)
        ;   cursor=0
    }
;;
