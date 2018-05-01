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

type operator_f = Plus | Minus | Times | Div | Mod | Eq   | Neq   | Leq   | Lt  | Geq | Gt | Or | And;;

type expr_f =
  | Cst    of int                          (* Feuille : constante *)
  | Bool   of bool                         (* Feuille : booléen  *)
  | Var    of var_f                        (* Feuille : variable *)
  | Bang   of expr_f                       (* Feuille : le déréférençage *)
  | Unit                                   (* Feuille : le type unit *)
  | Pair   of expr_f * expr_f              (* Un couple d'expressions *)
  | Neg    of expr_f                       (* Négation de Booléens *)
  | Bin    of expr_f * operator_f * expr_f (* opérations binaires *)
  | PrInt  of expr_f                       (* built-in prInt *)
  | Let    of pattern_f * expr_f * expr_f  (* let <var_f> = <expr_f> in <exec_f>   *)
  | LetRec of var_f   * expr_f * expr_f    (* let rec *)
  | Match  of expr_f  * pmatch_f           (* match [expr_f] with [pattern_matching] *)
  | IfElse of expr_f * expr_f * expr_f     (* If .. then .. else *)
  | Fun    of pattern_f * expr_f           (* car les fonctions sont un objet fun var -> expr *)
  | App    of expr_f * expr_f              (* Ce sont les applications *)
  | Aff    of expr_f * expr_f              (* Affectation i.e le `:=`*)
  | Alloc  of expr_f                       (* Allocation mémoire *)
  | Try    of expr_f * var_f * expr_f      (* Le 'try ... with E ... -> g...' *)
  | Raise  of expr_f                       (* raise E ... : qui sera un int en pratique *)

and pattern_f =
  | Var_Pat  of var_f
  | Pair_Pat of pattern_f  * pattern_f
  | Cons_Pat of string * pattern_f  (* 'Cons' comme 'Constructor'
                                    autrement dit tout ce qui est de la forme
                                    Something (pattern) *)

and pmatch_f  = (pattern_f * expr_f) list
;;


(* Les environnements sont ni plus ni moins
  des listes d'association *)

type env_f = (var_f * val_f) list

(* Une fonction est un objet fun x -> ... *)
(* Dans le cas des variables, on ajoute un environnement pour les clôtures *)

(* Rappel sur les types en OCamL : plusieurs constructeurs :

type ::= | int | float | bool | char | unit | string .....
         /* | (constructeur) of ... ? */  
         | type * type
         | type -> type

Sachant que Fouine n'est pas typé, on ne distingue pas types et valeurs : *)

and val_f = Unit
          | Bool        of bool
          | Int         of int
          | Ref         of int
          | Cons        of string * val_f
          | Pair_val    of val_f * val_f
          | Fun_val     of pattern_f * expr_f * env_f
;;

type instr_f =
  | CONST of int
  | ACCESS of var_f
  | ADD 
  | SUB
  | MULT
  | DIV
  | MOD
  (*if then else et booléens :  TODO*)
  | PRINT
  | LET of var_f    (* les LET sont atomiques (i.e pas de let (a, b) = (0, 1) *)
  | ENDLET
  | CLOSURE of var_f * asm_f
  | APPLY
  | RETURN
  (* ref et exceptions : TODO*)

(* on réinvente la liste avec les notations du cours 
and asm_f == instr_f list*)
and asm_f =
    EPSILON
  | SEQ of instr_f*asm_f
;;
