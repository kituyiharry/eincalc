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

let push s sval  = 
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

let handle_op vm = (function 
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
    | IEcho         -> let _ = (Format.printf " %s" (Emitter.show_spinval (peek vm))) in 1
);;

let eval (pr: vm) = 
    consume pr.source (handle_op pr)
;;

let tosource (vw: program) = 
    (>>==) (Genfunc.transform vw) (fun x -> 
        let vl = x.inps 
            |> List.map (fun { Genfunc.elems; _ } -> 
                elems
            )
            |> List.concat
            |> (Emitter.genloop (fun x _y _z _a -> 
                    x
                ) (fun x y ->
                    let _ = Format.printf "for %s \n" (show_einmatch y) in
                    { x with oprtns=x.oprtns @  
                        [
                            IPush (SStr (Format.sprintf "label %c -> with dimen %d" y.label y.dimen));
                            IEchoNl;
                            IPop;
                        ]
                    }
                )) (fun islast _e ps -> 
                    if islast then 
                        let i = Hashtbl.fold (fun a b acc -> 
                            [
                                IPush (SStr (Format.sprintf "%c is at" a));
                                IEcho;
                                IGetVar b; 
                                IEchoNl;
                                IPop;
                                IPop;
                            ] ::
                            acc
                        ) ps.nmdvar [] |> List.concat 
                        in { ps with oprtns=ps.oprtns @ i; }
                    else ps
                )
        in
        Ok vl 
    )
;;

let mkvm src = {
        spine  = Array.make 16 SNil
    ;   stkidx = 0
    ;   frmptr = 0 
    ;   source = src
}

