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
    | SKern   of int   (* point to kernel area *)
    | SAddr   of int array
    | SNdim:  ((module NDarray with type t = 'cont) * 'cont) -> spinval 
;; 

let shape_of_module (type n) (module N: NDarray with type t = n) v = 
    N.shape v
;;

let show_spinval s = 
    match s with
    | SNil      -> "nil"
    | SNumber f -> Format.sprintf "num: %.2f" f
    | SIndex  i -> Format.sprintf "idx: %d" i 
    | SBool   b -> Bool.to_string b 
    | SStr    s -> s 
    | SKern   k -> Format.sprintf "kern: %d" k
    | SAddr   a -> Format.sprintf "addr: [ %s ]" (string_of_dim a)
    | SNdim  (d, n) -> Genfunc.string_of_dim @@  (shape_of_module d n)
;;

let pp_spinval _f _s = 
    (Format.fprintf _f "%s" (show_spinval _s))
;;

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | SIndex x',  SIndex  y' -> SIndex  (x' +  y')
    | _ -> failwith (Format.sprintf "Invalid add operands: %s + %s" (show_spinval x) (show_spinval y))
;;

let seql x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.equal x' y')
    | SIndex  x', SIndex  y' -> SBool (Int.equal   x' y')
    | SBool   x', SBool   y' -> SBool (Bool.equal  x' y')
    | _ -> failwith (Format.sprintf "Invalid equality operands: %s = %s" (show_spinval x) (show_spinval y))
;;

let strue x = 
    match x with 
    | SBool y' -> (Bool.equal y' true)
    | _ -> failwith "Expected bool operand!"
;;

let smul x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' *. y')
    | SIndex  x', SIndex  y' -> SIndex  (x' *  y')
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
    | _ -> failwith (Format.sprintf "Invalid less operands: %s < %s" (show_spinval x) (show_spinval y))
;;

let sgreater x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (1))
    | _ -> failwith (Format.sprintf "Invalid greater operands: %s > %s" (show_spinval x) (show_spinval y))
;;

type instr = 
    (* arith *)
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    | IPush      of spinval (* TODO: not really needed -> load via IConst directly *)
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
    | IGetKern           (* load value from a kernel *)
    | ISetKern           (* write value to a kernel *)
    | IEchoKern          (* dereference and print out kernel values *)
    | ILoadAddr  of int  (* create an address from the next n values on the stack *)
[@@deriving show];;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
    ;   kernels: spinval array 
};;

(* TODO: unsafe assert *)
let get_const idx { consts; _ } = 
    Array.get consts idx
;;

type presource = {
        consts: spinval list
    ;   oprtns: instr   list
    ;   kernels: spinval list  (* a global area for array parameters *)
    ;   kcount: int
    ;   cnsidx: int
    ;   nmdvar: (char, int) Hashtbl.t  [@opaque]
    ;   varcnt: int
    ;   name:   string
    ;   args:   Parser.crange list
    ;
} [@@deriving show];;


let presempty name = {
        consts=[] 
    ;   kernels=[]
    ;   kcount=0
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

(* creates a slot for a named variable - should already be placed on the stack
   by the time you are using the variable -> this provides a way of referencing
   it allowing it to be set *)
let add_named_var vlue ps = 
    match Hashtbl.find_opt ps.nmdvar vlue with 
    | Some idx ->  (idx, ps) 
    | None ->
        let _ = Hashtbl.add ps.nmdvar vlue ps.varcnt in
        (ps.varcnt, { ps with varcnt=ps.varcnt+1 })
;;

let add_kernel ndim ps = 
    (ps.kcount, { ps with kernels=ndim::ps.kernels;kcount=ps.kcount+1 })
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

let iterndarray f nda = 
    let rec iternd nest f nda =
        match nda with 
        (* remember -> indices are in reverse so we need to reverse again *)
        | Parser.Itemize a -> List.iteri (fun l c ->  (f (List.rev (l :: nest)) c)) a
        | Parser.Collect c -> List.iteri (fun i c' -> (iternd (i :: nest) f c') ) c
    in 
    iternd [] f nda
;;

let ndarray_of_dim shp =  
    match shp with 
    | [] -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.make [||] 0. in
        (SNdim (_scal, _sdat))
    | hd :: [] -> 
        let _scal = (module Vector: NDarray with type t = float vector wrap) in 
        let (module Vector) = _scal in
        let _sdat = Vector.make [|hd|] 0. in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: [] -> 
        let _scal = (module Matrix: NDarray with type t = float matrix wrap) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.make [|hd;hd1|] 0. in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: hd2 :: [] -> 
        let _scal = (module BatchMatrix: NDarray with type t = batches) in 
        let (module BatchMatrix) = _scal in
        let _sdat = BatchMatrix.make [|hd;hd1;hd2|] 0. in
        (SNdim (_scal, _sdat))
    | rem -> 
        let _scal = (module MulDim: NDarray with type t = bigfloatarray) in 
        let (module MulDim) = _scal in
        let _sdat = MulDim.make (Array.of_list rem) 0. in
        (SNdim (_scal, _sdat))
;;


let ndarray_of_dim_init shp f =  
    match shp with 
    | [] -> 
        let _scal = (module Scalar: NDarray with type t = float ref) in 
        let (module Scalar) = _scal in
        let _sdat = Scalar.init [||] f in
        (SNdim (_scal, _sdat))
    | hd :: [] -> 
        let _scal = (module Vector: NDarray with type t = float vector wrap) in 
        let (module Vector) = _scal in
        let _sdat = Vector.init [|hd|] f in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: [] -> 
        let _scal = (module Matrix: NDarray with type t = float matrix wrap) in 
        let (module Matrix) = _scal in
        let _sdat = Matrix.init [|hd;hd1|] f in
        (SNdim (_scal, _sdat))
    | hd :: hd1 :: hd2 :: [] -> 
        let _scal = (module BatchMatrix: NDarray with type t = batches) in 
        let (module BatchMatrix) = _scal in
        let _sdat = BatchMatrix.init [|hd;hd1;hd2|] f in
        (SNdim (_scal, _sdat))
    | rem -> 
        let _scal = (module MulDim: NDarray with type t = bigfloatarray) in 
        let (module MulDim) = _scal in
        let _sdat = MulDim.init (Array.of_list rem) f in
        (SNdim (_scal, _sdat))
;;

let range_to_ndarray n shp =
    (* spreadsheet cell *)
    match n with 
    | Parser.NdArray (_ndfl) -> (
        match ndarray_of_dim shp with 
        | SNdim ((module M), _sdat) -> 
            let _ = iterndarray (
                fun x y -> (
                    M.set _sdat (Array.of_list x) y
                )
            ) _ndfl in  
            SNdim ((module M), _sdat)
        |  _ -> failwith "Unreachable in range conversion!"
    )
    | Parser.Create (_c) -> (
        match _c with
        | Parser.Diag (v, s) -> 
            ndarray_of_dim_init [s;s] (fun dimidx -> 
                let isdiag = Array.fold_left (fun (state, prev) x -> 
                    (state && prev = x, x)
                ) (true, dimidx.(0)) dimidx in
                if fst (isdiag) then 
                    v
                else
                    0.
            )
        | Parser.Zeros s -> 
            ndarray_of_dim s 
        | Parser.Ones s ->
            ndarray_of_dim_init s (fun _dimidx -> 1.) 
        | Parser.Fill (v, s) -> 
            let s' = ndarray_of_dim_init s (fun _dimidx -> v )  in
            s'
        | Parser.Enum (m, i, s) -> 
            let minv = ref m in
            ndarray_of_dim_init s (fun _dimidx ->  
                let cur = !minv in 
                let _ = minv :=  cur +. i in 
                cur
            ) 
        | Parser.Rand (b, s) -> 
            ndarray_of_dim_init s (fun _dimidx ->  
                Random.float b 
            ) 
    )
    | _ -> failwith "not implemented"
    (*| Range    (_frcell, _tocell) -> () *)
    (*| Relative (_motion, _crange) -> () *)
    (*| Refer    (_referral) -> () *)
    (*| Void ->  ()*)
;;



(* generates nested loops from a list of variable matches *)
let genloop ps (parms: (int list * Parser.crange) list) (out: int list) =

    (* load params into the  stack frame first as if they were function call arguments *)
    (* TODO: tranform into NDARRAYs *)

    (* create the output kernel first *)
    let outkern    = ndarray_of_dim    out in
    let outidx, ps = add_kernel outkern ps in 

    let (_idxs, ps) = List.fold_left (fun (kidxs, ps') (_shp, _cr) -> 
        let  _ndim      = range_to_ndarray _cr _shp in
        let (_kdx, ps') = add_kernel      _ndim ps' in
        (* TODO: pop params at the end as well!! *)
        (_kdx :: kidxs , ps')
    ) ([ ], ps) parms in

    (outidx, _idxs, fun (pre) (post) (_body) vlist -> 
        let rec genl vnum ({ label=vrn; dimen=bound; _ } as ein) psi lidx decl rem =
            let (sidx, ps) = add_const (SIndex 0)     psi in (* count from 0 *)
            let (vidx, ps) = add_named_var vrn ps in         (* capture as var once loaded *)
            let (sinc, ps) = add_const (SIndex 1)     ps in  (* increment by 1 *)
            let (eidx, ps) = add_const (SIndex bound) ps in  (* end at  eidx *)
            (* calculated as 1 jump instr + 6 for increment + 1 loop *)
            let jmp        = ref 8 in
            let decl'      = ((vrn, vidx) :: decl) in
            let ps'        = (
                (* Do what you want pre body invocation 
                   pass the state with current variable, remaining vars and
                   startindex for instructions in this loop 
                    
                   join the body then attach the params
                *)
                let prebody = pre vnum decl' ps ein rem lidx in
                let body = (
                    match rem with 
                    | [] -> 
                        let _islast = true in 
                        (*{ prebody with oprtns=(echoall ((vrn, vidx) :: decl)) }*)
                        let p = _body vnum decl' _islast ein (prebody)
                        (*in { p with oprtns=(echoall ((vrn, vidx) :: decl)) @ p.oprtns }*)
                        in p
                    | _hd' :: _rst -> 
                        let _islast = false in 
                        (* build the inner loop *)
                        (*(genl (vnum + 1) _hd' prebody (lidx + 11) decl' _rst)*)
                        _body vnum decl' _islast ein (genl (vnum + 1) _hd' prebody (lidx + 11) decl' _rst) 
                ) in
                (* do what you want post body invocation *)
                post vnum decl' body ein 
            ) in
            let _ = jmp := (!jmp + oplen ps') in
            { 
                ps' with oprtns=
                    (* attach former oprtns - we passed an empty earlier!! *)
                    (* WARNING -> MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
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
        | hd :: rest -> 
            (* start loops where our current operations end
            genl, parameters are: 
                counter 
                einmatch 
                presource 
                start-index 
                declared-vars 
                referenced-parameters 
                remainder-einmatches-and-params *)
            let g = genl 0 hd ps 3 [] rest in 
            { g with oprtns= [ IPush (SStr "=====VM START====="); IEchoNl; IPop
            ] @ g.oprtns }
    )
;;

let convert (s: presource) = 
    {
            oprtns =(Array.of_list s.oprtns) 
        ;   consts =(Array.of_list @@ List.rev s.consts)
        ;   cursor =0
        ;   kernels=(Array.of_list @@ List.rev s.kernels) 
    }
;;
