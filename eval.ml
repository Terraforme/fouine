open Types

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
let rec compute = function
  | Var s -> 0 (* TODO aller chercher la bonne valeur dans l'environnement *)
  | Cst k -> k
  | Plus(e1,e2) -> (compute e1) + (compute e2)
  | Minus(e1,e2) -> (compute e1) - (compute e2)
  | Times(e1,e2) -> (compute e1) * (compute e2)
  | Div(e1,e2) -> (compute e1) / (compute e2)
  | Mod(e1,e2) -> (compute e1) mod (compute e2)
;;
