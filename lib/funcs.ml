(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)
open Types;;

(* perfrom an operation _op after left then do right*)
let compose _lft _op _rgt = 
    _lft @ [ _op ] @ _rgt 
;;

(* for loop behaviour  *)
let loopblock startloc loopcounteridx bound = 
    (* WARNING -> MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
    (* 8 is if the body that appears in the loop is empty *)
    let jmp  = ref 8 in
    let  blck = [
        (* load the indexes - loop initializer *)
        IPush  (SIndex 0); 
        IGetVar loopcounteridx; 
        (* check if less than bound *)
        IPush   (SIndex bound); 
        ILess       ; 
        (* jump out of the loop *)
        IJumpFalse jmp;
        (* jump over the increment*)
        IJump       6; 
        (* increment -> loop back here after executing loop body *)
        IGetVar    loopcounteridx;
        IPush      (SIndex 1); 
        IAdd;   
        ISetVar    loopcounteridx;   (* also pops the stack *)
        ILoop      (startloc + 1); 
        (* end increment *)
    ] in 
    (jmp, blck)
;;

let loop jmp startloc hdblck slot = 
    (* WARNING -> MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
    let _ = jmp := (!jmp + (List.length slot)) in
    hdblck @ slot @ 
    [  
        (* Go to the increment *)
        ILoop  (startloc + 6);
        (* pop the named variable *)
        IPop; 
    ]
;;

(* assign a variable a value from a constant *)
let set_const _cidx _vidx = 
    [
        IConst     _cidx; 
        IGetVar    _vidx;
    ]
;;

(* increment a value by a constant *)
let incr_const _vidx _incidx = 
    [
        IGetVar    _vidx;
        IConst     _incidx; 
        IAdd;         
        ISetVar    _vidx;   (* also pops the stack *)
    ]
;;

(* prints a kernel to the console *)
let print_kern _idx = 
    [
        IPush (SKern _idx);
        IEchoKern;
        IPop;
    ]
;;

(* load some dimension for use as an array index of sorts *)
let load_arr_addr _vars _vblk = 
    let dims = List.rev @@ List.map (fun e -> (IGetVar (Hashtbl.find _vblk e))) _vars in
    dims @ [ ILoadAddr (List.length dims); ]
;;

(* load a value indexed by addr in a kernel *)
let fetch_arr_var _kern _addr = 
    (* load each element for the parameter *)
    (* get dimensions - this will be in order of declaration *)
    _addr @ [ 
        IPush (SKern _kern);
        IGetKern; 
    ] 
;;

(* write a value indexed by addr in a kernel *)
let write_arr_var _kern _addr = 
    (* load each element for the parameter *)
    (* get dimensions - this will be in order of declaration *)
    _addr @ [ 
        IPush (SKern _kern);
        ISetKern; 
    ] 
;;
