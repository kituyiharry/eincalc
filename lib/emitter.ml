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

let seql x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.equal x' y')
    | SBool   x',   SBool y' -> SBool (Bool.equal  x' y')
    | _ -> failwith "Invalid operands for equal"
;;

let strue x = 
    match x with 
    | SBool y' -> (Bool.equal y' true)
    | _ -> failwith "Expected bool operand!"
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
    (* arith *)
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    (* logic *)
    (*| ITrue*)
    (*| IFalse*)
    | INot               (* boolean inversion *)
    | ILess              (* Lesser than *)
    | IGreater           (* Greater than *)
    (* jumping *)
    | IJump      of int  (* Jump  *)
    | IJumpFalse of int  (* Jump if false *)
    | ILoop      of int  (* Jump to a specific location directly ? *)
    (* var ops *)
    | IConst     of int  (* load constant from position int onto the stack *)
    | IGetVar    of int  (* get the variable at a certain displacement from the stack index *)
    | ISetVar    of int  (* set value at certain displacement from stack index *)
    (* effects *)
    | IEcho              (* print value at the top of the stack *)
;;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
};;

let get_const idx { consts; _ } = 
    Array.get consts idx
;;

let gen_loop () =  
    (*let consts = [| 0; x; |] in *)
    [|
        IConst  0; 
        IGetVar 0;
        IConst  1;          (* load indixes *)
        IGreater ;          (* check condition *)
        IJumpFalse 9;  (* jump outside the loop *)
        IJump      5;  (* jump past the increment <- loop back here *)
        IGetVar 0;          (* perform the increment *)
        IConst  1;          (* const *)
        IAdd;               (* add *)
        ISetVar 0;          (* set variable *)
        ILoop   1;          (* loop *)
        INop;
        INop;
        ILoop 6;
        INop;
        INop;
    |]
;;


