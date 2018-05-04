open Env
open Types
open Mem


let cline = ref 0

let transform_op = function
  | Plus -> [|ADD|]
  | Minus -> [|SUB|]
  | Times -> [|MULT|]
  | Div   -> [|DIV|]
  | Mod   -> [|MOD|]
  | Eq    -> [|EQ|]
  | Neq   -> [|EQ;NOT|]
  | Leq   -> [|LE|]
  | Lt    -> [|LT|]
  | Geq   -> [|LT;NOT|]
  | Gt    -> [|LE;NOT|]
  | _     -> failwith "todo : Or, And"

let extract_var = function
  |  Var_Pat x -> x
  | _ -> failwith "Not a singleton"

let rec transform_SECD = function
  | Cst  a -> [| CONST a |]
  | Bool b -> [| BOOL b |]
  | Var  x -> [| ACCESS x |]
  | Bang e -> failwith "bang" 
  | Unit   -> failwith "unit"
  | Pair (e1, e2) -> failwith "pair"
  | Neg e -> let code = transform_SECD e in
             Array.append code [|NOT|]
  | Bin (e1, op, e2) -> let code2 = transform_SECD e2 in
                        let code1 = transform_SECD e1 in
                        let code = Array.append code2 code1 in
                        Array.append code (transform_op op) 
  | PrInt e -> Array.append (transform_SECD e) [|PRINT|]
  | Let (p, e1, e2) -> let code1 = Array.append (transform_SECD e1) [|LET (extract_var p)|] in
                       let code2 = Array.append (transform_SECD e2) [|ENDLET|] in
                       Array.append code1 code2
                       
  | LetRec (f, e1, e2) -> failwith "Gabzcr"
  | Match (_, _)->  failwith "Gabzcr"
  | IfElse (b, e1, e2) ->  failwith "Gabzcr"
  | Fun (p, e) ->  failwith "Gabzcr"
  | App (e1, e2) ->  failwith "Gabzcr"
  | Aff (e1, e2) ->  failwith "Gabzcr"
  | Alloc e -> failwith "Gabzcr"
  | Try (e, x, eX) -> failwith "Gabzcr"
  | Raise e -> failwith "Gabzcr"
   

let langage_SECD expr = 
  cline := 0;
  transform_SECD expr;;
    














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
