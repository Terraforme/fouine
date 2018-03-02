open Env
open Types


(* On définit exec_f la fonction qui prend un programme, un environnement, et l'exécute
Renvoie à priori un 'int'

exec : pgm_f -> env_f -> int *)


let rec exec expr env = match expr with
  | Bin (expr1, op, expr2) -> bin_exec op expr1 expr2 env
  | Var x                  -> env_read x env
  | Cst c                  -> c
  | Let (x, expr1, expr2)  -> exec expr2 (env_aff x (exec expr1 env))

and bin_exec op expr1 expr2 env = match op with
  | Plus  -> (exec expr1 env) + (exec expr2 env)
  | Minus -> (exec expr1 env) - (exec expr2 env)
  | Times -> (exec expr1 env) * (exec expr2 env)
  | Div   -> (exec expr1 env) / (exec expr2 env)
  | Mod   -> (exec expr1 env) mod (exec expr2 env)



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
  in
  pretty_str_aux 0 expr

let pretty_print_expr expr = print_strint (pretty_expr2str expr);;
