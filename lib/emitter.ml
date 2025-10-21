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
open Ndmodel;;
open Types;;

(* TODO: unsafe assert *)
let get_const idx { consts; _ } = 
    Array.get consts idx
;;

(* structure foreshadowing the final source defined int types -> easier to work
   with when parsing*)
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

let rec range_to_ndarray _grid n shp =
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
    | Parser.Range (cells, celle) -> 
        let adds = key_of_ref cells in
        let adde = key_of_ref celle in
        fetch_grid _grid adds adde (Fun.const 0.)
    | Parser.Scalar cell -> 
        let addr = key_of_ref cell in
        fetch_grid _grid addr addr (Fun.const 0.)
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
            let s' = Types.ndarray_of_dim_init s (fun _dimidx -> v )  in
            s'
        | Parser.Enum (m, i, s) -> 
            let minv = ref m in
            ndarray_of_dim_init s (fun _dimidx ->  
                let cur = !minv in 
                let _ = minv :=  cur +. i in 
                cur
            ) 
        | Parser.Rand (b, s) -> 
            (* TODO: seed random *)
            ndarray_of_dim_init s (fun _dimidx ->  
                Random.float b 
            ) 
        | Parser.Alt (slc, shp) -> 
            match slc with 
            | [] -> 
                ndarray_of_dim_init shp (Fun.const 0.) 
            | hd :: [] -> 
                ndarray_of_dim_init shp (Fun.const hd) 
            | _  ->
                let selc = Array.of_list slc in
                let len  = Array.length selc in 
                let loc  = ref 0 in
                ndarray_of_dim_init shp (fun _dimidx ->  
                    let v  = Array.unsafe_get selc (!loc) in 
                    let  _ = loc := (!loc + 1) mod len 
                    in v
                ) 
    )
    | Mask (_cr, _ml) -> 
        masked_to_ndarray _grid _ml _cr
    | _ -> failwith "not implemented"
    (*| Relative (_motion, _crange) -> () *)
    (*| Refer    (_referral) -> () *)
    (*| Void ->  ()*)
and masked_to_ndarray _grid _masks range = 
    (* TODO: we may not need to recalculate *)
    match calcshape range with 
    | Ok ishp -> 
        let ndarr = range_to_ndarray _grid range ishp in
        List.fold_left (fun acc mask -> 
            (match acc with
            | SNdim ((module M), data) ->
                (match mask with
                    | Parser.MinMax (a, b) -> 
                        let data' = Masks.minmaxscale (module M) data (a, b) in
                        SNdim ((module M), data')
                    | Parser.ZScore ->
                        let data' = Masks.zscore (module M) data in
                        SNdim ((module M), data')
                    | Parser.Mean ->
                        let mnval = Masks.mean (module M) data in 
                        let acc' = Ndarray.Scalar.make [||] mnval in
                        SNdim ((module Ndarray.Scalar), acc')
                    | Parser.Mode ->
                        let freq = Masks.frequencies (module M) (Masks.ModeTable.create 16) data in
                        let mode = Masks.mode freq in 
                        let acc' = Ndarray.Scalar.make [||] mode in
                        SNdim ((module Ndarray.Scalar), acc')
                    | Parser.Stddev -> 
                        let stdval = Masks.stddev (module M) data in 
                        let acc' = Ndarray.Scalar.make [||] stdval in
                        SNdim ((module Ndarray.Scalar), acc')
                    | Parser.Reshape nshp -> 
                        (match ndarray_of_dim nshp with 
                            | SNdim ((module M'), ndata) -> 
                                let _ =  Masks.reshape (module M) (module M') data ndata in
                                SNdim ((module M'), ndata)
                            | _ -> failwith "reshape error!"
                        )
                    | Parser.Write _cell -> 
                        (* execute an effect back to the grid *)
                        let start = key_of_ref _cell in
                        let _ = Masks.write (module M) start data _grid in
                        acc
                )
            | _ -> 
                failwith "Invalid mask argument!!"
            )
        ) ndarr _masks
    | Error s -> 
        failwith s
;;

(* generates nested loops from a list of variable matches *)
let genloop grid ps (parms: (int list * Parser.crange) list) (out: int list) =
    (* load params into the  stack frame first as if they were function call arguments *)
    (* create the output kernel first *)
    let outkern    = ndarray_of_dim    out in
    let outidx, ps = add_kernel outkern ps in 

    let (_idxs, ps) = List.fold_left (fun (kidxs, ps') (_shp, _cr) -> 
        let  _ndim      = range_to_ndarray grid _cr _shp in
        let (_kdx, ps') = add_kernel      _ndim ps' in
        (_kdx :: kidxs , ps')
    ) ([ ], ps) parms in

    (outidx, (List.rev _idxs), fun (pre) (post) (_body) vlist -> 
        let rec genl vnum ({ label=vrn; dimen=bound; _ } as ein) ps lidx decl rem =
            let (vidx, ps) = add_named_var vrn ps in         (* capture as var once loaded *)
            let decl'      = ((vrn, vidx) :: decl) in
            let (jmp, hdblk) =  Funcs.loopblock lidx vidx bound in
            (* get offset from start of the loop for the inner *)
            let offset = List.length hdblk in
            let ps' = (
                let prebody = pre vnum decl' ps ein rem lidx in
                let body = (
                    match rem with 
                    | [] -> 
                        let _islast = true in 
                        _body vnum decl' _islast ein (prebody)
                    | _hd' :: _rst -> 
                        let _islast = false in 
                        (* build the inner loop and body *)
                        _body vnum decl' _islast ein (genl (vnum + 1) _hd' prebody (lidx + offset) decl' _rst) 
                ) in
                (* do what you want post body invocation *)
                post vnum decl' body ein 
            ) in
            (* attach former oprtns - we passed an empty earlier!! *)
            { ps' with oprtns= Funcs.loop jmp lidx hdblk ps'.oprtns }
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
            { g with oprtns= [ IPush (SStr "    =====VM START====="); IEchoNl; IPop ] @ g.oprtns }
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
