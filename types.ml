(* On définit dans ce fichier les types principaux, nécessaires
 * partout dans le code. *)

type var_f = string

(* Les variables sont des chaînes de caractères.
   Elles sont associées à des valeurs dans l'environnement *)

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
bref, ce seront des expressions.
*)


(* Quand on considère les expressions arithmétiques :
   Elles sont de la formule X opérateur Y

Donc de la formule expr_f * opérateur * expr_f

  On définit les différents opérateurs ci-après
  Qed des expressions booléennes et de comparaisons *)

type operator_f = Plus | Minus | Times | Div | Mod;;
type cmp_op_f   = Eq   | Neq   | Leq   | Lt  | Geq | Gt;;
type bool_op_f  = Or   | And;;

type expr_f =
  | Cst    of int                          (* Feuille : constante *)
  | Var    of var_f                        (* Feuille : variable *)
  | Bang   of expr_f                       (* Feuille : le déréférençage *)
  | Unit                                   (* Feuille : le type unit *)
  | Pair   of expr_f * expr_f              (* Un couple d'expressions *)
  | Bin    of expr_f * operator_f * expr_f (* opérations binaires *)
  | PrInt  of expr_f
  | Let    of pattern_f * expr_f * expr_f      (* let <var_f> = <expr_f> in <exec_f>   *)
  | LetRec of var_f * expr_f * expr_f      (* let rec *)
  | If     of bexpr_f * expr_f
  | IfElse of bexpr_f * expr_f * expr_f
  | Fun    of pattern_f * expr_f               (* car les fonctions sont un objet fun var -> expr *)
  | App    of expr_f * expr_f              (* Ce sont les applications *)
  | Aff    of expr_f * expr_f               (* Affectation i.e le `:=`*)
  | Alloc  of expr_f                       (* Allocation mémoire *)
  | Try    of expr_f * var_f * expr_f      (* Le 'try ... with E ... -> ...' *)
  | Raise  of int                       (* raise E ... : qui sera un int en pratique *)

(* On co-définit les expressions booléennes - pour les tests *)
and bexpr_f =
  | True
  | False
  | Cmp      of expr_f  * cmp_op_f  * expr_f
  | Bin_op   of bexpr_f * bool_op_f * bexpr_f
  | Not      of bexpr_f

and pattern_f =
  | Var_Pat  of var_f
  | Pair_Pat of var_f * pattern_f
;;


(* Les environnements sont ni plus ni moins
  des listes d'association *)

type env_f = (var_f * val_f) list

(* Une fonction est un objet fun x -> ... *)
(* Dans le cas des variables, on ajoute un environnement pour les clôtures *)
and val_f = Int         of int
          | Pair_val    of val_f * val_f
          | Ref         of int32
          | Unit
          | Fun_val     of pattern_f * expr_f * env_f
;;
