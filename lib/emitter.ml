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

type spinval = 
    | SNil
    | SNumber of float
    | SIndex  of int
    | SBool   of bool
    | SStr    of string
 [@@deriving show];; 

let sadd x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SNumber (x' +. y')
    | SIndex x', SIndex y'   -> SIndex  (x' + y')
    | _ -> failwith "Invalid add operands"
;;

let seql x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.equal x' y')
    | SIndex  x', SIndex y' ->  SBool (Int.equal x' y')
    | SBool   x', SBool y' ->   SBool (Bool.equal  x' y')
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
    | SIndex x', SIndex y'   -> SIndex (x' * y')
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
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (-1))
    | _ -> failwith "Invalid add operands"
;;

let sgreater x y = 
    match (x, y) with 
    | SNumber x', SNumber y' -> SBool (Float.compare x' y' = (1))
    | SIndex x',  SIndex y'  -> SBool (Int.compare x' y'   = (1))
    | _ -> failwith "Invalid add operands"
;;

type instr = 
    (* arith *)
    | IAdd               (* binary add *)
    | IMul               (* binary multiply *)
    (* instr *)
    | INop               (* No operation *)
    | IPop               (* Pop of the stack *)
    | IPush      of spinval
    (* logic *)
    (*| ITrue*)
    (*| IFalse*)
    | INot               (* boolean inversion *)
    | ILess              (* Lesser than *)
    | IGreater           (* Greater than *)
    (* jumping *)
    | IJump      of int  (* Jump  *)
    | IJumpFalse of int ref  (* Jump if false *)
    | ILoop      of int  (* Jump to a specific location directly ? *)
    (* var ops *)
    | IConst     of int  (* load constant from position int onto the stack *)
    | IGetVar    of int  (* get the variable at a certain displacement from the stack index *)
    | ISetVar    of int  (* set value at certain displacement from stack index *)
    (* effects *)
    | IEcho              (* print value at the top of the stack *)
    | IEchoNl
;;

type source = {
        oprtns: instr array
    ;   consts: spinval array
    ;   mutable cursor: int
};;

let get_const idx { consts; _ } = 
    Array.get consts idx
;;
(*
    [|
                IConst      0; 
                IGetVar     0;
                IConst      2;    
                ILess        ;    

                IJumpFalse 11;

                IJump       6; 
                IGetVar     0; 
                IConst      1; 
                IAdd;         
                ISetVar     0; 
                ILoop       1; 
                IGetVar     0;
                IEcho;
                IPop;
                ILoop       6;
                INop;
                INop;
                IGetVar     0;
                IEcho;
            |]
*)

type presource = {
        consts: spinval list
    ;   oprtns: instr list
    ;   cnsidx: int
    ;   nmdvar: (string, int) Hashtbl.t 
    ;   varcnt: int
    ;   name:   string
};;


let presempty name = {
        consts=[] 
    ;   oprtns=[]
    ;   cnsidx=0
    ;   nmdvar=(Hashtbl.create 8) 
    ;   varcnt=0
    ;   name
};;

let add_const vlue ps = 
    (ps.cnsidx, { ps with consts = vlue :: ps.consts; cnsidx=ps.cnsidx+1 })
;;

let add_named_var vlue ps = 
    match Hashtbl.find_opt ps.nmdvar vlue with 
    | Some idx ->  (idx, ps) 
    | None ->
        let _ = Hashtbl.add ps.nmdvar vlue ps.varcnt in
        (ps.varcnt, { ps with varcnt=ps.varcnt+1 })
;;

let echoall vlist =
    List.map (fun (var, idx) -> 
        [
            IPush (SStr var);
            IEcho;
            IGetVar idx;
            IEcho;
            IPush (SStr " ");
            IEchoNl;
            IPop;
            IPop;
            IPop;
        ]
    ) vlist
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
let loop vrn = 
    let rec genl vrn count ps lidx decl rem =
        let (sidx, ps) = add_const (SIndex 0)     ps in (* count from 0 *)
        let (sinc, ps) = add_const (SIndex 1)     ps in (* increment by 1 *)
        let (eidx, ps) = add_const (SIndex count) ps in (* end at  eidx *)
        let (vidx, ps) = add_named_var vrn ps in
        let jmp        = ref 8 in
        let ps'        = (
            match rem with 
            | [] -> { ps with oprtns=(echoall ((vrn, vidx) :: decl)) }
            | (hd, count') :: rst -> genl hd count' ps (lidx + 11) ((vrn, vidx) :: decl) rst
        ) in
        let _ = jmp := (!jmp + List.length ps'.oprtns) in
        { 
            ps' with 
            oprtns=[ 

                (* load the indexes *)
                IConst  sidx; 
                IGetVar vidx; 
                IConst  eidx; 
                ILess       ; 

                (* jump out of the loop *)
                IJumpFalse jmp;

                (* jump over the increment*)
                IJump       6; 

                (* increment -> loop back here after executing loop body *)
                IGetVar    vidx;
                IConst     sinc; 
                IAdd;         
                ISetVar    vidx; 
                ILoop     (lidx + 1); 
                (* end increment *)
            ]

                (* do loop operations here *)

                @ ps'.oprtns @

                (* end loop operations *)
            [

                (* Go to the increment *)
                ILoop  (lidx + 6);

                (* pop the named variable *)
                IPop; 
            ]
        }
    in 
    match vrn with 
    | [] -> presempty ""
    | (hd,  count) :: rest -> genl hd count (presempty hd) 0 [] rest
;;

let convert (s: presource) = 
    {
        oprtns=(Array.of_list s.oprtns) 
        ;   consts=(Array.of_list @@ List.rev s.consts)
        ;   cursor=0
    }
;;
