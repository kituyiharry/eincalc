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

(*let optfetchmulttadd instr bound = *)
    (*let rec optimize oldins newins = *)
        (*(match oldins with *)
        (*| VGetLoadAddr (_addr, _vars) :: rem -> *)
            (*(match rem with *)
            (*| IPush k :: IGetKern :: rest -> *)
                (*optimize rest ((_addr, _vars, k) newins)*)
            (*| _ -> failwith "opfetchmultadd optimization error";*)
            (*)*)
        (*| IMul*)
        (*| [] -> newins*)
        (*)*)
    (*in optimize instr []*)
(*;;*)
 
(* for loop behaviour  *)
let optloopblock startloc loopcounteridx bound = 
    (* WARNING: MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
    (* 8 is if the body that appears if the loop is empty - modify it via the
       returned ref if the body has extra instructions. 
       it starts counting AFTER the IJumpFalse || VJumpFalseConst.
       there are 2 extra instructions not shown here which are considered in
       this accounting. (see loop) so here there are 3 instructions after the
       jumpfalseconst and 2 to end the loop   
    *)
    let jmp  = ref 5 in
    let  blck = [

        (* load the loop indexes - loop initializer *)
        IPush  (SIndex 0); 
        VJumpFalseConst (bound, loopcounteridx, jmp);
        (* jump over the increment, jump back here at the end of the loop! in loop function *)
        IJump       3; 
        (* increment -> loop back here after executing loop body *)
        VAddSetVarConst (1, loopcounteridx);
        ILoop      (startloc + 1); 
        (* end increment *)

    ] in (jmp, blck)
;;

let optloop jmp startloc hdblck slot = 
    (* WARNING: MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
    let _ = jmp := (!jmp + (List.length slot)) in
    hdblck @ slot @ 
    [  
        (* Go to the increment *)
        ILoop  (startloc + 3);
        (* pop the named variable *)
        IPop; 
    ]
;;

(* for loop behaviour  *)
let loopblock startloc loopcounteridx bound = 
    (* WARNING: MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
    (* 8 is if the body that appears if the loop is empty - modify it via the
       returned ref if the body has extra instructions.
       it starts counting AFTER the IJumpFalse || VJumpFalseConst.
       there are 2 extra instructions not shown here which are considered in
       this accounting. (see loop) so here there are 6 instructions after the
       jump and 2 to end the loop
       *)
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
    (* WARNING: MODIFYING THIS LIST AFFECTS VM OUTPUT SINCE JUMPS ARE HARD CODED!!!! *)
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

(* (Optimized!) load some dimension for use as an array index of sorts *)
let opt_load_arr_addr_data _vars _vblk = 
    let len  = List.length _vars in
    let arr  = Array.make len 0 in
    (*let dims = List.rev @@ List.map (fun e -> (IGetVar (Hashtbl.find _vblk e))) _vars in*)
    let _ = List.iteri (fun i e -> (arr.(i) <- (Hashtbl.find _vblk e))) _vars in
    (* array of var indexes and actual address array *)
    (arr, Array.make len 0);
;;

(* (Optimized!) load some dimension for use as an array index of sorts *)
let opt_load_arr_addr _vars _vblk = 
    let len  = List.length _vars in
    let arr  = Array.make len 0 in
    (*let dims = List.rev @@ List.map (fun e -> (IGetVar (Hashtbl.find _vblk e))) _vars in*)
    let _ = List.iteri (fun i e -> (arr.(i) <- (Hashtbl.find _vblk e))) _vars in
    [ VGetLoadAddr (arr, Array.make len 0); ]
;;

(* load some dimension for use as an array index of sorts *)
let load_arr_addr _vars _vblk = 
    (* TODO: can we avoid this List.rev ?? *)
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

(* print out a list of variables *)
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
