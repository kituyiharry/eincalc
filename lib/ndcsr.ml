(*
 *   Copyright (C) Eincalc 2025. All Rights Reserved.
 *   For internal use only. Do not redistribute.
 *
 *   Copyright laws and international treaties protect this app. Unauthorized 
 *   redistribution of this app without express, written permission from our legal 
 *   department may entail severe civil or criminal penalties.
 *
 *)

(* in our case this encodes non-empty clls *)
open Ndmodel;;

type rowindx = { 
        index: int 
    ;   row  : int
} 
and csrstore = {
        values : string array
    ;   columns: int    array
    ;   rows   : rowindx list (* index and row value *)
} [@@deriving show];;

(* assumes the sequence is already sorted *)
let encode (rowcount: int) (dstream: ((int * int) * spinmodel) Seq.t) = 
    let pointer = ref (-1) in
    let csr = Seq.fold_lefti (fun (state) i ((r, c), p) -> 
        if r > !pointer then
            (
                state.columns.(i) <- c;
                state.values.(i)  <- (ser p);
                pointer := r;
                { state with rows=(({ index=i; row=r }) :: state.rows);  }
            )
        else 
            (
                state.columns.(i) <- c;
                state.values.(i)  <- (ser p);
                state
            )
    ) 
    ({ 
            values =(Array.make rowcount "")
        ;   columns=(Array.make rowcount 0)
        ;   rows   =[]
        ;
    })
    dstream in 
    { csr with rows=(List.rev csr.rows); }
;;

(* assumes the sequence is already sorted *)
(* TODO: handle deser errors  or corruption ?? *)
let decode (tbl: spinmodel Grid.t) (csr: csrstore) = 
    let vseq = Array.to_seq csr.values |> Seq.zip (Array.to_seq csr.columns) in
    Seq.fold_lefti (fun (ri, rem) i (col, dat) ->
        (match rem with 
        | [] -> 
            Grid.add tbl (ri.row, col) (Result.get_ok (deser dat));
            (ri, rem)
        | hd' :: rest' -> 
            if i = hd'.index then (
                Grid.add tbl (hd'.row, col) (Result.get_ok (deser dat));
                (hd', rest')
            ) else (
                Grid.add tbl (ri.row, col) (Result.get_ok (deser dat));
                (ri,  rem)
            ) 
        )
    ) (List.hd csr.rows, List.tl csr.rows) vseq
;;
