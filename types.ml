(* On définit dans ce fichier les types principaux, nécessaires
 * partout dans le code. *)

(* Considérons le code trés simple suivant :

let f = fun x ->
    let y = x in
    x * y

Ici, f, x, y sont des 'identificateurs'. f désigne une fonction,
x et y désignent des variables. 'let', 'in', 'fun' sont mots-clés.
'*' est une opération. Quant à '=' et '->', il s'agit de ponctuations.

   Il est nécessaire de donner un type pour les identificateurs,
pour les mots-clés, les opérations, variables et fonctions.

Par convention, tous nos types auront l'extension '_f' pour
spécifier qu'il s'agit de types spécifiques à 'fouine'. *)



type var_f = string

(* ************ PROGRAMMES ************** *)
(* Ils sont vus comme des arbres.
 * Chaque noeud est étiqueté par un mot clé :
Par exemple
                    (root)
                      |
                     (IF)
                    /    \
                 then..  else..
  De maniére évidente, les feuilles seront des
variables, des appels de fonctions, des tests,
bref, ce seront des expressions arithmétiques.
*)

type operator_f = Plus | Minus | Times | Div | Mod;;
type cmp_op_f = Eq | Neq | Leq | Lt | Geq | Gt;;
type bool_op_f = Or | And;;

type expr_f =
  | Bin of expr_f * operator_f * expr_f
  | Var of var_f
  | Cst of int
  | PrInt of expr_f
  | Let of var_f * expr_f * expr_f  (* let <var_f> = <expr_f> in <exec_f>   *)
  | LetRec of var_f * expr_f * expr_f
  | If of bexpr_f * expr_f
  | IfElse of bexpr_f * expr_f * expr_f
  | Fun of var_f * expr_f (* car les fonctions sont un objet fun var -> expr *)
  | App of expr_f * expr_f
and bexpr_f =
  | True
  | False
  | Cmp of expr_f * cmp_op_f * expr_f
  | Bin_op of bexpr_f * bool_op_f * bexpr_f
  | Not of bexpr_f
;;

  (* | If  of bexpr_f * expr_f * expr_f     (* if  <bexpr_f> then <exec_f> else <exec_f> *)*)



type env_f = (var_f * val_f) list(* Une fonction est un objet fun x -> ... *)
                                    (* Dans le cas des variables, on ajoute un environnement pour les clôtures *)
and val_f = Int of int | Fun_var of var_f * expr_f * env_f
;;
