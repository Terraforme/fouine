(* On d�finit dans ce fichier les types principaux, n�cessaires
 * partout dans le code. *)

(* Consid�rons le code tr�s simple suivant :

let f = fun x ->
    let y = x in
    x * y

Ici, f, x, y sont des 'identificateurs'. f d�signe une fonction,
x et y d�signent des variables. 'let', 'in', 'fun' sont mots-cl�s.
'*' est une op�ration. Quant � '=' et '->', il s'agit de ponctuations.

   Il est n�cessaire de donner un type pour les identificateurs,
pour les mots-cl�s, les op�rations, variables et fonctions.

Par convention, tous nos types auront l'extension '_f' pour
sp�cifier qu'il s'agit de types sp�cifiques � 'fouine'. *)



type identifier_f = string


type function_f = unit
type variable_f = int
type const_f    = int

type entity     = Fun of function_f | Var of variable_f | Cst of const_f

type key_word_f = Let | Fun | If ;;

type expr_f =
  | Var   of string
  | Cst   of int
  | Plus  of expr_f * expr_f
  | Minus of expr_f * expr_f
  | Times of expr_f * expr_f
  | Div   of expr_f * expr_f
  | Mod   of expr_f * expr_f
;;

(* ************ PROGRAMMES ************** *)
(* Ils sont vus comme des arbres, �tiquet�s par : *)
