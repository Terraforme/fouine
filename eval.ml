open Env
open Types


(* On définit exec_f la fonction qui prend un programme, un environnement, et l'exécute
Renvoie à priori un 'int'

eval : pgm_f -> env_f -> int *)


let rec eval expr env = match expr with
  | Bin (expr1, op, expr2) -> bin_eval op expr1 expr2 env
  | Var x                  -> env_read x env
  | Cst c                  -> c
  | Let (x, expr1, expr2)  -> eval expr2 (env_aff x (eval expr1 env) env)
  | If (bexpr, expr)       -> 1 (* TODO *)
  | IfElse (bexpr, expr1, expr2) -> 1 (* TODO *)

and bin_eval op expr1 expr2 env = match op with
  | Plus  -> (eval expr1 env) + (eval expr2 env)
  | Minus -> (eval expr1 env) - (eval expr2 env)
  | Times -> (eval expr1 env) * (eval expr2 env)
  | Div   -> (eval expr1 env) / (eval expr2 env)
  | Mod   -> (eval expr1 env) mod (eval expr2 env)




(* ************** Fonctions d'affichage ************** *)

let rec op2str = function
  | Plus  -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div   -> "Div"
  | Mod   -> "Mod"

let rec expr2str = function
  | Bin (expr1, op, expr2) -> (op2str op) ^
  "(" ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | Var x -> x
  | Cst c -> string_of_int c
  | Let (x, expr1, expr2) -> "Let(" ^ x ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | If (bexpr, expr) -> "TODO"
  | IfElse (bexpr, expr1, expr2) -> "TODO"


let print_expr expr = print_string (expr2str expr)


let rec pretty_op2str = function
  | Plus  -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Div   -> " / "
  | Mod   -> " % "

let pretty_expr2str expr =
  let rec pretty_str_aux indent = function
    | Bin (expr1, op, expr2) -> (pretty_str_aux indent expr1) ^ (pretty_op2str op) ^ (pretty_str_aux indent expr2)
    | Var x -> x
    | Cst c -> string_of_int c
    | Let (x, expr1, expr2) -> "let " ^ x ^ " = "
    ^ (pretty_str_aux indent expr1) ^ " in " ^ (pretty_str_aux indent expr2) ^ "\n"
    | If (bexpr, expr) -> "TODO"
    | IfElse (bexpr, expr1, expr2) -> "TODO"
  in
  pretty_str_aux 0 expr

let pretty_print_expr expr = print_string (pretty_expr2str expr);;
