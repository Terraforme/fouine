open Env
open Types


(* On définit exec_f la fonction qui prend un programme, un environnement, et l'exécute
Renvoie à priori un 'int'

exec : pgm_f -> env_f -> int *)


(* fonction d'affichage *)
let rec affiche_expr e =
  let aff_aux s a b =
      begin
	print_string s;
	affiche_expr a;
	print_string ", ";
	affiche_expr b;
	print_string ")"
      end
  in
  match e with
  | Var s -> print_string s
  | Cst k -> print_int k
  | Plus(e1,e2) -> aff_aux "Plus(" e1 e2
  | Minus(e1,e2) -> aff_aux "Minus(" e1 e2
  | Times(e1,e2) -> aff_aux "Times(" e1 e2
  | Div(e1,e2) -> aff_aux "Div(" e1 e2
  | Mod(e1,e2) -> aff_aux "Mod(" e1 e2
;;

(* sémantique opérationnelle à grands pas *)
let rec compute env = function
  | Var x -> env_read x env
  | Cst k -> k
  | Plus(e1,e2) -> (compute env e1) + (compute env e2)
  | Minus(e1,e2) -> (compute env e1) - (compute env e2)
  | Times(e1,e2) -> (compute env e1) * (compute env e2)
  | Div(e1,e2) -> (compute env e1) / (compute env e2)
  | Mod(e1,e2) -> (compute env e1) mod (compute env e2)
;;


let rec exec pgm env = match pgm with
  | Expr expr -> (compute env expr) (* TODO : evaluer l'expression *)
  | Let (x, expr, pgm_0) -> exec pgm_0 (env_aff x (exec expr env) env)
  (* TODO : remplacer (env_aff x (1)) par (env_aff x (calculer valeur)) *)




(* ************** Fonctions d'affichage ************** *)
