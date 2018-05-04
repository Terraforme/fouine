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
  | _     -> failwith "todo : Or, And"

let rec nb_of_instr = function
  | Cst  a -> 1
  | Bool b -> 1
  | Var  x -> 1
  | Bang e -> failwith "TODO: références" (* TODO *)
  | Unit   -> 1
  | Pair (e1, e2) -> failwith "TODO: paires" (* TODO *)
  | Neg e -> (nb_of_instr e) + 1
  | Bin (e1, op, e2) -> (nb_of_instr e2) + (nb_of_instr e1) + (nb_of_instr_op op)
  | PrInt e -> (nb_of_instr e) + 1
  | Let (p, e1, e2) -> (nb_of_instr e1) + (nb_of_instr e2) + 2 (*let et endlet*)
  | LetRec (f, e1, e2) -> failwith "TODO: fonctions récursives"
  | Match (_, _)->  failwith "TODO or TO give up with"
  | IfElse (b, e1, e2) ->  (nb_of_instr b) + (nb_of_instr e2) + (nb_of_instr e1) + 2 (*JUMPIF et JUMP*)
  | Fun (p, e) ->  (nb_of_instr e) + 4 (*let, endlet, closure et return*)
  | App (e1, e2) ->  (nb_of_instr e2) + (nb_of_instr e1) + 1
  | Aff (e1, e2) ->  failwith "TODO: références"
  | Alloc e -> failwith "TODO: références"
  | Try (e, x, eX) -> failwith "TODO: exceptions"
  | Raise e -> failwith "TODO: exceptions"



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
  | _     -> failwith "todo : Or, And"

let extract_var = function
  |  Var_Pat x -> x
  | _ -> failwith "Not a singleton"

let rec transform_SECD = function
  | Cst  a -> !result.(!current_address) <- (CONST a); incr current_address
  | Bool b -> !result.(!current_address) <- (BOOL b); incr current_address
  | Var  x -> !result.(!current_address) <- (ACCESS x); incr current_address
  | Bang e -> failwith "TODO: références" (* TODO *)
  | Unit   -> !result.(!current_address) <- UNIT; incr current_address
(*inutile, cf initialisation, mais je le laisse pour l'instant au cas où UNIT est un jour remplacé par EPSILON*)

  | Pair (e1, e2) -> failwith "TODO: paires" (* TODO *)
  | Neg e -> transform_SECD e; !result.(!current_address) <- NOT; incr current_address
  | Bin (e1, op, e2) -> transform_SECD e2; transform_SECD e1; transform_op op
  | PrInt e -> transform_SECD e; !result.(!current_address) <- PRINT; incr current_address
  | Let (p, e1, e2) -> transform_SECD e1; !result.(!current_address) <- (LET (extract_var p)); incr current_address;
                       transform_SECD e2; !result.(!current_address) <- ENDLET; incr current_address
  | LetRec (f, e1, e2) -> failwith "TODO: fonctions récursives"
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
  | Fun (p, e) ->  let v = extract_var p in
                    let save_address = !current_address in
                    !result.(!current_address + 1) <- (LET v);
                    current_address := !current_address + 2;
                    transform_SECD e;
                    !result.(!current_address) <- ENDLET;
                    !result.(!current_address + 1) <- RETURN;
                    current_address := !current_address + 2;
                    !result.(save_address) <- (CLOSURE !current_address)
  | App (e1, e2) ->  transform_SECD e2; transform_SECD e1; !result.(!current_address) <- APPLY; incr current_address
  | Aff (e1, e2) ->  failwith "TODO: références" (*TODO*)
  | Alloc e -> failwith "TODO: références" (*TODO*)
  | Try (e, x, eX) -> failwith "TODO: exceptions" (*TODO*)
  | Raise e -> failwith "TODO: exceptions" (*TODO*)


let langage_SECD expr =
  let n = nb_of_instr expr in
  result := Array.make n UNIT;
  transform_SECD expr;
  !result;;















(*
let rec langage_SECD expr = match expr with
  | Cst c -> [CONST c]
  | Bool b -> [ BOOL b ]
  | Var v -> [ACCESS v]
  | Bang e0 -> failwith "TODO" (*TODO*)
  | Unit -> []
  | Pair(e1, e2) -> failwith "TODO" (*TODO*)
  | Neg(e0) -> failwith "TODO" (*TODO*)
  | Bin(e1, op, e2) ->
    begin
    match op with
      | Plus -> (langage_SECD e2)@(langage_SECD e1)@[ADD]
      | Minus -> (langage_SECD e2)@(langage_SECD e1)@[SUB]
      | Times -> (langage_SECD e2)@(langage_SECD e1)@[MULT]
      | Div -> (langage_SECD e2)@(langage_SECD e1)@[DIV]
      | Mod -> (langage_SECD e2)@(langage_SECD e1)@[MOD]
      | Eq -> (langage_SECD e2)@(langage_SECD e1)@[EQ]
      | Neq -> (langage_SECD e2)@(langage_SECD e1)@[EQ; NOT]
      | Leq -> (langage_SECD e2)@(langage_SECD e1)@[LE]
      | Lt -> (langage_SECD e2)@(langage_SECD e1)@[LT]
      | Geq -> (langage_SECD e2)@(langage_SECD e1)@[LT; NOT]
      | Gt -> (langage_SECD e2)@(langage_SECD e1)@[LE; NOT]
      | Or -> langage_SECD (IfElse (e1, Bool true, e2))
      | And-> langage_SECD (IfElse (e1, e2, Bool false))
    end
  | PrInt e0 -> (langage_SECD e0)@[PRINT]
  | Let(pat, e1, e2) ->
  begin
    match pat with
    | Var_Pat v -> (langage_SECD e1)@[LET(v)]@(langage_SECD e2)@[ENDLET]
    | Pair_Pat(pat1, pat2) -> failwith "TODO" (*TODO*)
    | Cons_Pat(v, pat2) -> failwith "TODO" (*TODO*)
  end
  | LetRec(v, e1, e2) -> failwith "TODO" (*TODO*)
  | Match(e0, patm) -> failwith "TODO"
  | IfElse(bexpr, e1, e2) ->
    (langage_SECD bexpr)@[SWITCH((langage_SECD e1) @ [RETURN], (langage_SECD e2) @ [RETURN])] (* besoin d'un return *)
  | Fun(pat, e0) ->
  begin
    match pat with
    | Var_Pat v -> [CLOSURE(v, (langage_SECD e0) @ [RETURN])]
    | Pair_Pat(pat1, pat2) -> failwith "TODO" (*TODO*)
    | Cons_Pat(v, pat2) -> failwith "TODO" (*TODO*)
  end
  | App(e1, e2) -> (langage_SECD e2)@(langage_SECD e1)@[APPLY]
  | Aff(e1, e2) -> failwith "TODO" (*TODO*)
  | Alloc(expr0) -> failwith "TODO" (*TODO*)
  | Try(expr1, v, expr2) -> failwith "TODO" (*TODO*)
  | Raise expr0 -> failwith "TODO" (*TODO*)
;;*)
