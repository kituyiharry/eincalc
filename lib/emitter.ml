(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
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
    ;   pmasks: Parser.mask list (* masks executed after *)
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
    ;   pmasks=[]
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


let as_slice (type adata) mask curshp curdim 
    (module M : Ndarray.NDarray with type t = adata) 
    data sl
=
    (match shape_of_mask mask curshp with 
        | Ok sliceshp -> 
            (match ndarray_of_dim sliceshp with 
                | SNdim ((module M'), data') -> 
                    let slarr = Array.of_list @@ List.mapi (fun i slice ->
                        (match slice with 
                            | Parser.Along index -> 
                                (index, curdim.(i), 1)
                            | Parser.Select {start;len;skip} -> 
                                (match (start,len,skip) with 
                                    | (None, None, None) -> 
                                        (0,  curdim.(i) - 1, 1)
                                    | (Some st, None, None) -> 
                                        (st, curdim.(i) - 1, 1)
                                    | (Some st, Some ln, None) -> 
                                        (st, ln, 1)
                                    | (Some st, Some ln, Some sk) -> 
                                        (st, ln, sk)
                                    | (None, Some ln, Some sk) -> 
                                        (0, ln, sk)
                                    | (None, None, Some sk) -> 
                                        (0, curdim.(i) - 1, sk)
                                    | (None, Some ln, None) -> 
                                        (0, ln, 1)
                                    | (Some st, None, Some sk) ->
                                        (st, curdim.(i) - 1, sk)
                                )
                        )
                    ) sl in 
                    let _     = Masks.slice (module M) (module M') slarr data data' in
                    SNdim ((module M'), data')
                | _ -> failwith "reshape error!"
            )
        | Error e -> 
            failwith (Format.sprintf "shouldn't fail here but got %s!!" e)
    )
;;

let handle_masks (type data) _grid axis masks acc (module M: Ndarray.NDarray with type t = data) (data: data) =

    (* a way to squish axes like mean *)
    let collapse os ns len = 
        for i = 0 to (axis - 1) do 
            ns.(i) <- os.(i);
        done;
        for i = (axis + 1) to (len - 1) do 
            ns.(i-1) <- os.(i);
        done;
        ndarray_of_dimshape ns
    in

    match masks with 
    | [] -> 
        acc 
    | hd :: [] -> 
        let curdim= M.shape data in
        let len   = Array.length curdim in
        (match hd with
            | Parser.Mean -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') as acc' -> 
                        let _ = Masks.meanaxis axis (module M') (_data') (module M) (data)in
                        acc'
                    | _ -> 
                        failwith "axis collapse failure"
                )
            | Parser.Mode -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') as acc' -> 
                        let _ = Masks.modeaxis axis (module M') (_data') (module M) (data)in
                        acc'
                    | _ -> 
                        failwith "axis collapse failure"
                )
            | Parser.Stddev -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') as acc' -> 
                        let _ = Masks.stddevaxis axis (module M') (_data') (module M) (data)in
                        acc'
                    | _ -> 
                        failwith "axis collapse failure"
                )
            (* we will rewrite over the data to save some memory *)
            | Parser.MinMax (a, b) -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') -> 
                        let  _data'' = M'.make ns 0. in
                        let _ = Masks.minmaxscaleaxis axis 
                            (module M') (_data') 
                            (module M') (_data'') 
                            (module M)  (data) (a, b)
                        in acc
                    | _ -> 
                        failwith "minmax axis collapse failure"
                )
            | Parser.ZScore -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') -> 
                        let  _data'' = M'.make ns 0. in
                        let _ = Masks.zscoreaxis axis 
                            (module M') (_data') 
                            (module M') (_data'') 
                            (module M)  (data)
                        in acc
                    | _ -> 
                        failwith "minmax axis collapse failure"
                )
            | Parser.Write _cell -> 
                let start = key_of_ref _cell in
                let _ = Masks.writeaxis axis (module M) start data _grid in
                acc
            | Parser.Cumsum -> 
                let _ = Masks.cumsumaxis axis (module M) data in
                acc
            | Parser.Sum -> 
                let ns = Array.make (len - 1) 0 in
                (match collapse curdim ns len with 
                    | SNdim ((module M'), _data') as acc' -> 
                        let _ = Masks.sumaxis axis (module M') (_data') (module M) (data)in
                        acc'
                    | _ -> 
                        failwith "axis collapse failure"
                )
            | Parser.Slice sl -> 
                let curshp = Array.to_seq curdim |> List.of_seq in
                as_slice hd curshp curdim (module M) data sl
            | Parser.Reshape _ -> 
                failwith "axis reshape not supported atm!!"
            (* FIXME: implement actual plotting *)
            | Parser.Plot _pl -> 
                acc
            | Parser.Axis (_, _) -> 
                failwith "nested axis operations not allowed!!"
        )
    | _hd :: _rest -> 
        let _ = List.fold_left (fun acc _mask -> 
            acc
        ) acc masks in 
        acc
;;

let rec range_to_ndarray _grid n _cl shp =
    (* spreadsheet cell *)
    (* TODO: optimization to avoid having to create tensors for some functions  *)
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
        masked_to_ndarray _grid _ml _cl _cr
    | _ -> failwith "not implemented"
    (*| Relative (_motion, _crange) -> () *)
    (*| Refer    (_referral) -> () *)
    (*| Void ->  ()*)
(* TODO: remove all failwith calls and replace with Result *)
and masked_to_ndarray _grid _masks _cl range = 
    (* TODO: we may not need to recalculate *)
    match calcshape _cl range with 
    | Ok ishp -> 
        let ndarr = range_to_ndarray _grid range _cl ishp in
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
                    | Parser.Sum ->
                        let mnval = Masks.sum (module M) data in 
                        let acc' = Ndarray.Scalar.make [||] mnval in
                        SNdim ((module Ndarray.Scalar), acc')
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
                    | Parser.Slice sl -> 
                        let curdim = M.shape data in
                        let curshp = Array.to_seq curdim |> List.of_seq in
                        as_slice mask curshp curdim (module M) data sl
                    | Parser.Write _cell -> 
                        (* execute an effect back to the grid *)
                        let start = key_of_ref _cell in
                        let _ = Masks.write (module M) start data _grid in
                        acc
                    | Parser.Cumsum -> 
                        let _ = Masks.cumsum (module M) data in
                        acc
                    (* FIXME: implement actual plotting *)
                    | Parser.Plot _pl -> 
                        acc
                    | Parser.Axis (axis, masks) -> 
                        handle_masks _grid axis masks acc (module M) data
                )
            | _ -> 
                failwith "Invalid mask argument!!"
            )
        ) ndarr _masks
    | Error s -> 
        failwith s
and transform_mask _grid ndarr masks = 
    (* TODO: optimization for rank 0 tensors which are just reflected *)
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
                    | Parser.Sum ->
                        let mnval = Masks.sum (module M) data in 
                        let acc' = Ndarray.Scalar.make [||] mnval in
                        SNdim ((module Ndarray.Scalar), acc')
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
                    (* TODO: may be more efficient when converted to bigarray *)
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
                    | Parser.Slice sl -> 
                        let curdim = M.shape data in
                        let curshp = Array.to_seq curdim |> List.of_seq in
                        as_slice mask curshp curdim (module M) data sl
                    | Parser.Cumsum -> 
                        (* execute an effect back to the grid *)
                        let _ = Masks.cumsum (module M) data in
                        acc
                    (* FIXME: implement actual plotting *)
                    | Parser.Plot _pl -> 
                        acc
                    | Parser.Axis (axis, masks) -> 
                        handle_masks _grid axis masks acc (module M) data
                )
            | _ -> 
                failwith "Invalid mask argument!!"
        )
    ) ndarr masks
;;

(* generates nested loops from a list of variable matches *)
let genloop grid ps (parms: (int list * Parser.crange * Parser.mask list * (char * int) list) list) (out: int list) =
    (* load params into the  stack frame first as if they were function call arguments *)
    (* create the output kernel first *)
    let outkern    = ndarray_of_dim    out in
    let outidx, ps = add_kernel outkern ps in 

    let (_idxs, ps) = List.fold_left (fun (kidxs, ps') (_shp, _cr, _msks, _cl) -> 
        let  _ndim      = range_to_ndarray grid _cr _cl _shp in
        let  _ndim'     = transform_mask   grid _ndim _msks in
        let (_kdx, ps') = add_kernel       _ndim' ps' in
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
        ;   pmasks =s.pmasks
    }
;;
