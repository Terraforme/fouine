open Env
open Types
open Mem

let rec connecte expr_mem expr_trans = match expr_mem with
  | Let(pat, expr1, Unit) -> Let(pat, expr1, expr_trans) (* ne devrait pas se produire (on finit sur un let rec dans la mémoire fouine)*)
  | Let(pat, expr1, expr2) -> Let(pat, expr1, connecte expr2 expr_trans)
  | LetRec(v, expr1, Unit) -> LetRec(v, expr1, expr_trans) (* C'est le cas important!!!*)
  | LetRec(pat, expr1, expr2) -> LetRec(pat, expr1, connecte expr2 expr_trans)
  | e -> e
;;
(*
  | Cst c -> Cst c
  | Bool b -> Bool b
  | Var v -> Var v
  | Bang expr0 -> bang expr0
  | Unit -> Unit
  | Pair(expr1, expr2) -> Pair(expr1, expr2)
  | Neg(expr0) -> Neg(expr0)
  | Bin(expr1, op, expr2)
  | PrInt expr0 -> PrInt expr0

  | IfElse(bexpr, expr1, expr2) ->  IfElse(bexpr, expr1, expr2)
  | Fun(pat, expr0) -> Fun(pat, expr0)
  | App(expr1, expr2) -> App(expr1, expr2)
*) 
