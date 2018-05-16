open Env
open Types
open Mem


let current_address = ref 0
let result = ref (Array.make 0 UNIT)

let nb_of_instr_op = function
  | Plus -> 1
  | Minus -> 1
  | Times -> 1
  | Div   -> 1
  | Mod   -> 1
  | Eq    -> 1
  | Neq   -> 2
  | Leq   -> 1
  | Lt    -> 1
  | Geq   -> 2
  | Gt    -> 2
  | _     -> failwith "TODO: Or, And"


let rec nb_of_instr_in_pattern p = match p with
  | Var_Pat x -> 1
  | Pair_Pat(p1, p2) -> (nb_of_instr_in_pattern p1) + (nb_of_instr_in_pattern p2)
  | _ -> failwith "L'autre cas est une option qui n'a pas été traitée: les constructeurs"

let rec traite_pattern p = match p with
  | Var_Pat x -> !result.(!current_address) <- (LET x); incr current_address; 1
  | Pair_Pat(p1, p2) -> !result.(!current_address) <- DESTRUCT; incr current_address; let nb_of_let1 = traite_pattern p1 and nb_of_let2 = traite_pattern p2
    in nb_of_let1 + nb_of_let2
  | _ -> failwith "L'autre cas est une option qui n'a pas été traitée: les constructeurs"


let rec nb_of_instr = function
  | Cst  a -> 1
  | Bool b -> 1
  | Var  x -> 1
  | Bang e -> (nb_of_instr e) + 1 (* READ *)
  | Unit   -> 1
  | Pair (e1, e2) -> (nb_of_instr e2) + (nb_of_instr e1) + 1 (* PAIR *)
  | Neg e -> (nb_of_instr e) + 1
  | Bin (e1, op, e2) -> (nb_of_instr e2) + (nb_of_instr e1) + (nb_of_instr_op op)
  | PrInt e -> (nb_of_instr e) + 1
  (*| Let (p, e1, e2) -> (nb_of_instr e1) + (nb_of_instr e2) + 2 (*LET et ENDLET*)*)
  | Let (p, e1, e2) -> (nb_of_instr e1) + (nb_of_instr e2) + 3*(nb_of_instr_in_pattern p)-1 (*LET et ENDLET pour chaque var + un destruct par fois où on paire*)
  | LetRec (f, e1, e2) -> (nb_of_instr e1) + (nb_of_instr e2) + 2 (*REC et ENDLET*)
  | Match (_, _)->  failwith "TODO or to give up with"
  | IfElse (b, e1, e2) ->  (nb_of_instr b) + (nb_of_instr e2) + (nb_of_instr e1) + 2 (*JUMPIF et JUMP*)
  | Fun (p, e) ->  (nb_of_instr e) + 2 + 3*(nb_of_instr_in_pattern p) - 1 (*CLOSURE et RETURN, et les LET, ENDLET et DESTRUCT*)
  | App (e1, e2) ->  (nb_of_instr e2) + (nb_of_instr e1) + 1 (* APPLY *)
  | Aff (e1, e2) ->  (nb_of_instr e2) + (nb_of_instr e1) + 1 (* WRITE *)
  | Alloc e -> (nb_of_instr e) + 1 (* ALLOC *)
  | Try (e, x, eX) -> (nb_of_instr e) + (nb_of_instr eX) + 5 (* SETJMP, UNSETJMP, JUMP, LET, ENDLET *)
  | Raise e -> (nb_of_instr e) + 1 (* LONGJMP *)



let transform_op = function
  | Plus -> !result.(!current_address) <- ADD; incr current_address
  | Minus -> !result.(!current_address) <- SUB; incr current_address
  | Times -> !result.(!current_address) <- MULT; incr current_address
  | Div   -> !result.(!current_address) <- DIV; incr current_address
  | Mod   -> !result.(!current_address) <- MOD; incr current_address
  | Eq    -> !result.(!current_address) <- EQ; incr current_address
  | Neq   -> !result.(!current_address) <- EQ; !result.(!current_address + 1) <- NOT; current_address := !current_address + 2
  | Leq   -> !result.(!current_address) <- LE; incr current_address
  | Lt    -> !result.(!current_address) <- LT; incr current_address
  | Geq   -> !result.(!current_address) <- LT; !result.(!current_address + 1) <- NOT; current_address := !current_address + 2
  | Gt    -> !result.(!current_address) <- LE; !result.(!current_address) <- NOT; current_address := !current_address + 2
  | _     -> failwith "TODO: Or, And" (*TODO*)

let rec transform_SECD = function
  | Cst  a -> !result.(!current_address) <- (CONST a); incr current_address
  | Bool b -> !result.(!current_address) <- (BOOL b); incr current_address
  | Var  x -> !result.(!current_address) <- (ACCESS x); incr current_address
  | Bang e -> transform_SECD e; !result.(!current_address) <- READ; incr current_address
  | Unit   -> !result.(!current_address) <- UNIT; incr current_address
(*inutile, cf initialisation, mais je le laisse pour l'instant au cas où UNIT est un jour remplacé par EPSILON*)

  | Pair (e1, e2) -> transform_SECD e2; transform_SECD e1; !result.(!current_address) <- PAIR; incr current_address
  | Neg e -> transform_SECD e; !result.(!current_address) <- NOT; incr current_address
  | Bin (e1, op, e2) -> transform_SECD e2; transform_SECD e1; transform_op op
  | PrInt e -> transform_SECD e; !result.(!current_address) <- PRINT; incr current_address
  | Let (p, e1, e2) ->
  begin
    transform_SECD e1;
    let nb_of_let = traite_pattern p in
    transform_SECD e2;
    for i=0 to nb_of_let - 1 do
      !result.(!current_address) <- ENDLET;
      incr current_address
    done;
  end

  | LetRec (f, e1, e2) -> transform_SECD e1; !result.(!current_address) <- (REC f); incr current_address;
                          transform_SECD e2; !result.(!current_address) <- ENDLET; incr current_address
  | Match (_, _)->  failwith "TODO or to give up with..."
  | IfElse (b, e1, e2) -> transform_SECD b;
                          let save_address1 = !current_address in
                          incr current_address;
                          transform_SECD e2;
                          !result.(save_address1) <- (JUMPIF (!current_address + 1));
                          let save_address2 = !current_address in
                          incr current_address;
                          transform_SECD e1;
                          !result.(save_address2) <- (JUMP !current_address)
  | Fun (p, e) ->   let save_address = !current_address in
                    incr current_address;
                    let nb_of_let = traite_pattern p in
                    transform_SECD e;
                    for i=0 to nb_of_let - 1 do
                      !result.(!current_address) <- ENDLET;
                      incr current_address
                    done;
                    !result.(!current_address + 1) <- RETURN;
                    incr current_address;
                    !result.(save_address) <- (CLOSURE !current_address)
  | App (e1, e2) ->  transform_SECD e2; transform_SECD e1; !result.(!current_address) <- APPLY; incr current_address
  | Aff (e1, e2) ->  transform_SECD e2; transform_SECD e1; !result.(!current_address) <- WRITE; incr current_address
  | Alloc e -> transform_SECD e; !result.(!current_address) <- ALLOC; incr current_address
  | Try (e, x, eX) -> let save_address1 = !current_address in
                      incr current_address;
                      transform_SECD e;
                      !result.(!current_address) <- UNSETJMP; incr current_address;
                      let save_address2 = !current_address in
                      incr current_address;
                      !result.(save_address1) <- SETJMP(!current_address);
                      !result.(!current_address) <- LET(x); incr current_address;
                      transform_SECD eX;
                      !result.(!current_address) <- ENDLET; incr current_address;
                      !result.(save_address2) <- JUMP(!current_address)
  | Raise e -> transform_SECD e; !result.(!current_address) <- LONGJMP; incr current_address


let langage_SECD expr =
  let n = nb_of_instr expr in
  result := Array.make n UNIT;
  transform_SECD expr;
  !result;;
