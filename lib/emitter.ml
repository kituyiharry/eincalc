(*
    (I[3] -> K[3])
    for i = 0; i < 3; i = i + 1 {
        K[i] = I[i]
    }

    (I[3] -> K[1])
    for i = 0; i < 3; i = i + 1{
        K[i] += I[i]
    }

    (I[3],I[3] -> II[3]) 
    for i = 0; i < 3; i = i + 1{
        II[i] += I[I] * I[i]
    }

    - initialization
    - condition
    - increment

*)

type spinval = 
    | SNil
    | SNumber of float
    | SBool   of bool
 [@@deriving show];; 

type instr = 
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    | ILess              (* Lesser than *)
    | IGreater           (* Greater than *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    (* logic *)
    | ITrue
    | IFalse
    | INot               (* boolean inversion *)
    (* jumping *)
    | IJump      of int  (* Jump  *)
    | IJumpFalse of int  (* Jump if false *)
    | ILoop      of int  (* Jump to a specific location directly ? *)
    (* var ops *)
    | IConst     of int  (* load constant from position int onto the stack *)
    | IGetVar    of int  (* get the variable at a certain displacement from the  *)
;;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
};;

let  get_const idx { consts; _ } = 
    Array.get consts idx
;;


