(* On définit dans ce fichier les types principaux, nécessaires
 * partout dans le code. *)

type var_f = string (* les variables fouine *)

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

type env_f = (var_f * val_f) list (* Les environnements ne sont ni plus ni moins que des listes d'association *)

and val_f = Unit
          | Bool        of bool
          | Int         of int
          | Ref         of int
          | Cons        of string * val_f
          | Pair_val    of val_f * val_f
          | Fun_val     of pattern_f * expr_f * env_f
;;

type instr_f =
  | UNIT

  | CONST of int
  | BOOL  of bool
  | ACCESS of var_f

  | ADD
  | SUB
  | MULT
  | DIV
  | MOD

  (* Opérateurs booléens *)
  | NOT
  | EQ
  | LT
  | LE

  | PRINT
  | LET of var_f    (* les LET sont atomiques (i.e pas de let (a, b) = (0, 1) : on gère ces cas avec PAIR et DESTRUCT *)
  | REC of var_f    (* le let rec *)
  | ENDLET

  (* Instrucfions de branchement *)
  | JUMP   of int
  | JUMPIF of int
  | CLOSURE of int
  | APPLY
  | RETURN

  (* Gestion De La Mémoire *)
  | READ
  | WRITE
  | ALLOC

  (* Exceptions *)
  | SETJMP of int
  | UNSETJMP
  | LONGJMP

  | PAIR
  | DESTRUCT

and asm_f = instr_f array;;
