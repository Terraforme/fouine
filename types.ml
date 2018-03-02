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

type a_expr_f =
  | Var   of var_f
  | Cst   of int
  | Plus  of a_expr_f * a_expr_f
  | Minus of a_expr_f * a_expr_f
  | Times of a_expr_f * a_expr_f
  | Div   of a_expr_f * a_expr_f
  | Mod   of a_expr_f * a_expr_f

type bexpr_f =
  | Var_b   of var_f
  | Cst_b   of int
  | Eq    of bexpr_f * bexpr_f
  | NEq   of bexpr_f * bexpr_f
  | Not   of bexpr_f * bexpr_f
  | Leq   of bexpr_f * bexpr_f
  | Lt    of bexpr_f * bexpr_f
  | Geq   of bexpr_f * bexpr_f
  | Gt    of bexpr_f * bexpr_f
;;

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


type expr_f =
  | Expr of a_expr_f
  | Let of var_f * expr_f * expr_f  (* let <var_f> = <a_expr_f> in <exec_f>   *)

  (* | If  of bexpr_f * expr_f * expr_f     (* if  <bexpr_f> then <exec_f> else <exec_f> *)*)

type fun_f = (var_f list) * expr_f (* TODO *)
type val_f = Int of int | Fun of fun_f
;;
