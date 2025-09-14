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

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | _ -> failwith "Invalid add operands"
;;

let smul x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' *. y')
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
    | _ -> failwith "Invalid add operands"
;;

let sgreater x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (1))
    | _ -> failwith "Invalid add operands"
;;

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


