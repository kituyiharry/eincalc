(*
 *   Copyright (C) Spinal 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Parser;;
open Emitter;;
open Genfunc;;
(*open Ndarray;;*)

let (>>==) = Result.bind;;

type vm = {
        spine:  Emitter.spinval array
    ;   source: Emitter.source
    ;   mutable stkidx: int 
    ;   mutable frmptr: int
};;

let get_stack idx { frmptr; spine; _ } = 
    Array.get spine (frmptr + idx)
;;

let set_stack idx sval { frmptr; spine; _ } = 
    Array.set spine (frmptr + idx) sval
;;

let debug_stack { spine; stkidx; _ } = 
    let _ = Format.printf "Stacktrace ============\n" in
    Array.iteri (fun x y ->
        if x > stkidx then
            ()
        else
            Format.printf "%d -> %s\n" x (Emitter.show_spinval y)
    ) spine 
;;

let push s sval = 
    let _ = Array.set s.spine (s.stkidx) sval in 
    s.stkidx <- (s.stkidx + 1)
;;

let pop s = 
    let v = Array.get s.spine (s.stkidx - 1) in 
    let _ = s.stkidx <- (s.stkidx - 1) in 
    v
;;

let peek s = 
    Array.get s.spine (s.stkidx - 1)
;;

let load_kernel_addr vm count = 
    let rec collect add indx = 
        if indx == count then 
            push vm (SAddr (Array.of_list @@ List.rev add))
        else
            (match pop vm with 
            | SIndex idx -> 
                collect (idx :: add) (indx + 1)
            | sp -> 
                let _ = debug_stack vm in 
                failwith (Format.sprintf "Expected index, found %s" (show_spinval sp))
            )
    in 
    collect [] 0
;;

let load_kernel_val vm = 
    let addr = pop vm in
    let indx = pop vm in
    match (indx, addr) with 
    | (SKern _i, SAddr _a) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), modl) -> 
                let _y = M.get modl _a in
                Format.printf "Loaded Kernel Value \n" 
            | _ -> failwith "invalid kernel!"
        )
    | (SAddr _a, SKern _i) -> 
        (match (vm.source.kernels.(_i)) with 
            | SNdim ((module M), modl) -> 
                let _ = M.get modl _a in
                Format.printf "Loaded Kernel Value\n"
            | _ -> failwith "invalid kernel!"
        )
    | _  -> 
        let _ = debug_stack vm in 
        failwith (Format.sprintf "Unable to load kernel using params %s and %s " (show_spinval indx) (show_spinval addr))
;;

let binop s f = 
    match f with 
    | IAdd -> push s @@ Emitter.sadd (pop s) (pop s)
    | IMul -> push s @@ Emitter.smul (pop s) (pop s)
    | _    -> failwith "Unhandled binary op"
;;

let reset_vm v =
    v.source.cursor <- 0; 
    { v with stkidx = 0; spine=(Array.make 16 SNil) }
;;

(* consume instructions and return the number of places to jump *)
let rec consume ({ Emitter.oprtns; cursor; _ } as s) apply = 
    if cursor >= Array.length oprtns then 
        ()
    else
        let _ = s.cursor <- s.cursor + apply oprtns.(cursor) in
        consume s apply
;;

let handle_op vm op = 
    (*let _ = Format.printf "handling: %s\n" (show_instr op) in*)
    match op with
    | INop          -> 1 
    | IPop          -> let _ = pop  vm in 1 
    | IPush v       -> let _ = push vm v in 1
    | ILoop x       -> let _ = vm.source.cursor <- x in 0 
    | IJump y       -> y 
    | IJumpFalse z  -> if (Emitter.strue @@ Emitter.seql (pop vm) (SBool false)) then !z else 1
    | IAdd          -> let _ = binop vm (IAdd) in 1 
    | IMul          -> let _ = binop vm (IMul) in 1 
    | INot          -> let _ = push  vm (Emitter.snot (pop vm)) in 1 
    | ILess         -> let _ = push  vm ((Emitter.sless   ) (pop vm) (pop vm)) in 1 
    | IGreater      -> let _ = push  vm ((Emitter.sgreater) (pop vm) (pop vm)) in 1 
    | IConst  _c    -> let _ = push  vm (get_const _c vm.source) in 1 
    | IGetVar _g    -> let _ = push  vm (get_stack _g vm) in 1 
    | ISetVar _g    -> let _ = set_stack _g (pop vm) vm in 1 
    | IEchoNl       -> let _ = (Format.printf " %s\n" (Emitter.show_spinval (peek vm))) in 1
    | IEcho         -> let _ = (Format.printf " %s"   (Emitter.show_spinval (peek vm))) in 1
    | IGetKern      -> let _ = load_kernel_val  vm in 1
    | ILoadAddr _a  -> let _ = load_kernel_addr vm _a in 1 
;;

let eval (pr: vm) = 
    consume pr.source (handle_op pr)
;;

let tosource (vw: program) = 
    (>>==) (Genfunc.transform vw) (fun x -> 
        let (_kidxs, gl) = Emitter.genloop (presempty "") (List.map (fun y -> (y.shape, y.param) ) x.inps) in 
        let _ = Format.printf "\nTree: %s \n\n" (show_eintree x) in
        let vl = (
            x.inps 
            |> List.map (fun ({ elems; _ }: eincomp) ->
                List.map (fun (x: einmatch) -> (x, (SKern (List.nth _kidxs x.param)))) elems
            )
            |> List.concat
            |> (gl 
                (* before body on each iteration *)
                (fun _i _dcl _par x _y _z _a -> x) 
                (* after body on each iteration *)
                (fun _i _dcl _par x _y -> x)) 
                (* body *)
                (fun _i _dcl _par islast _e ps -> 

                    (* all vars have been loaded, *)
                    if islast then 

                        (* get dimensions - this will be in order of declaration *)
                        let dims = List.map ( 
                            fun (_name,_sidx) -> (IGetVar _sidx)
                        ) _dcl in
                        let addr = dims @ [ ILoadAddr (List.length dims) ] in

                        let i = addr @ [
                            (*IEchoNl;*)
                            IPush _par;
                            IGetKern;
                            (*IPop;*)
                            (*IPop;*)
                            (*IPush _par; *)
                            (*IEchoNl; *)
                            (*IPop;*)
                        ] 

                        in { ps with oprtns=ps.oprtns @ i; }

                    else ps
                )
        ) in Ok vl 
    )
;;

let mkvm src = {
        spine  = Array.make 16 SNil
    ;   stkidx = 0
    ;   frmptr = 0 
    ;   source = src
}

