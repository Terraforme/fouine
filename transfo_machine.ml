open Env
open Types
open Mem

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
;;
