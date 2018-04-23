open Types

(* ************** Fonctions d'affichage ************** *)
(* Il y a là un affichage en mode 'arbre' - l'afficheur
standard, et un pretty printer capable de fournir du code
qui pourra être directement utilisé comme code OCamL *)



(* Le premier Printer *)
(* Sous forme d'arbre : non ambigü mais parfois
difficile à utiliser car les notations sont lourdes *)

let op2str = function
  | Plus  -> "Plus"
  | Minus -> "Minus"
  | Times -> "Times"
  | Div   -> "Div"
  | Mod   -> "Mod"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Leq -> "Leq"
  | Lt -> "Lt"
  | Geq -> "Geq"
  | Gt -> "Gt"
  | And -> "And"
  | Or -> "Or"


let cmp_op2str = function
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Leq -> "Leq"
  | Lt -> "Lt"
  | Geq -> "Geq"
  | Gt -> "Gt"

let bool_op2str = function
  | And -> "And"
  | Or -> "Or"

let rec expr2str = function
  | Neg expr               -> "Neg(" ^ (expr2str expr) ^ ")"
  | Bin (expr1, op, expr2) -> "Bin("^(expr2str expr1)^","^(op2str op) ^ "," ^ (expr2str expr2) ^ ")"
  | Var x     -> "Var "^x
  | Bang expr -> "Bang(" ^ (expr2str expr)^")"
  | Cst c     -> "Cst "^string_of_int c
  | Bool b    -> "Bool " ^ string_of_bool b
  | Unit      -> "Unit"

  | PrInt expr            -> "prInt(" ^ (expr2str expr) ^ ")"
  | Let (x, expr1, expr2) -> "Let(" ^ (pattern2str x) ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | LetRec (f, expr1, expr2) -> "LetRec(" ^ f ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
	| Match (expr, pmatch)     -> "Match(" ^ (expr2str expr) ^ ", "
	^ (pmatch2str pmatch) ^ ")"

  | If (bexpr, expr) -> "If(" ^ bexpr2str(bexpr) ^ ", " ^ expr2str(expr) ^ ")"
  | IfElse (bexpr, expr1, expr2) -> "IfElse(" ^ expr2str(bexpr) ^ ", " ^ expr2str(expr1) ^ ", " ^ expr2str(expr2) ^ ")"

  | Fun (var, expr) -> "Fun(" ^ (pattern2str var) ^ ", " ^ (expr2str expr) ^ ")"
  | App (expr1, expr2) -> "App(" ^ (expr2str expr1) ^ "," ^ (expr2str expr2) ^ ")"

  | Aff (expr1, expr2) -> "Aff(" ^ (expr2str expr1)^ "," ^ (expr2str expr2) ^ ")"
  | Alloc expr -> "Alloc(" ^ (expr2str expr) ^ ")"

  | Pair (expr1, expr2) -> "Pair("^(expr2str expr1)^" , "^(expr2str expr2)^")"

  | Try (expr1, var, expr2) -> "Try(" ^ (expr2str expr1) ^ ", " ^ var ^ ", " ^ (expr2str expr2) ^ ")"
  | Raise a -> "Raise( " ^ (expr2str a) ^ ")"


and bexpr2str = function
  | True -> "True"
  | False -> "False"
  | Cmp (expr1, op, expr2) -> "Cmp("^ expr2str(expr1) ^ ", "^ cmp_op2str(op) ^ ", " ^ expr2str(expr2) ^ ")"
  | Bin_op(bexpr1, op, bexpr2) -> "Bin_op(" ^ bexpr2str(bexpr1)^ ", " ^ bool_op2str(op) ^ ", " ^ bexpr2str(bexpr2) ^ ")"
  | Not bexpr -> "Not(" ^ bexpr2str(bexpr) ^ ")"

and pattern2str = function
  | Var_Pat x -> "Var_Pat " ^ x
  | Pair_Pat (pat1, pat2) -> "Pair_Pat(" ^ (pattern2str pat1) ^ "," ^ (pattern2str pat2) ^ ")"
	| Cons_Pat (c, pattern) -> "Cons_Pat(" ^ c  ^ "," ^ (pattern2str pattern) ^ ")"

and pmatch2str  = function
	| [] -> ""
	| (pat, expr) :: []     -> (pattern2str pat) ^ " -> " ^ (expr2str expr)
	| (pat, expr) :: pmatch -> (pattern2str pat) ^ " -> " ^ (expr2str expr)
                             ^ " | " ^ (pmatch2str pmatch)

let print_expr expr = print_string (expr2str expr) ; print_newline ()



(* Le pretty printer :
Le code qu'il donne peut être normalement utilisé
en code OCamL *)

(*let cons_color = "\027[33;1m"*)
let cons_color = "\027[0;1m"
let var_color  = "\027[0;2m"
let cst_color  = "\027[31;2m"
let def_color  = "\027[0;m"


let pretty_op2str = function
  | Plus  -> print_string " + "
  | Minus -> print_string " - "
  | Times -> print_string " * "
  | Div   -> print_string " / "
  | Mod   -> print_string " % "
  | Eq -> print_string " = "
  | Neq -> print_string " <> "
  | Leq -> print_string " <= "
  | Lt -> print_string " < "
  | Geq -> print_string " >= "
  | Gt -> print_string " > "
  | Or -> print_string " || "
  | And -> print_string " && "

let pretty_cmp = function
  | Eq -> print_string " = "
  | Neq -> print_string " <> "
  | Leq -> print_string " <= "
  | Lt -> print_string " < "
  | Geq -> print_string " >= "
  | Gt -> print_string " > "

let pretty_bool_op = function
  | Or -> print_string " || "
  | And -> print_string " && "


let rec pretty_pattern = function
  | Var_Pat x -> print_string x
  | Pair_Pat (pat1, pat2) ->
    begin
      print_string "(";
      pretty_pattern pat1;
      print_string ", ";
      pretty_pattern pat2;
      print_string ")"
    end
	| Cons_Pat (c, pattern) ->
    begin
      print_string (c ^ "(");
      pretty_pattern pattern;
      print_string ")"
    end


let pretty_print_expr expr =

  let print_tab n =
    for i = 0 to n-1 do print_string "  " done
  in

  let rec pretty_aux indent = function
    | Neg expr ->
      begin
        print_string (cons_color ^ "not " ^ def_color ^ "(" );
        pretty_aux (indent + 1) expr;
        print_string ")"
      end
    | Bin (expr1, op, expr2) ->
      begin
        print_string "(";
        pretty_aux indent expr1;
        pretty_op2str op;
        pretty_aux indent expr2;
        print_string ")"
      end
    | Var x -> print_string (var_color ^ x ^ def_color)
    | Cst c -> print_string (cst_color ^ (string_of_int c) ^ def_color)
    | Unit -> print_string "()"
    | Bool b -> print_string (string_of_bool b)
    | Bang expr ->
      begin
        print_string "!(";
        pretty_aux indent expr;
        print_string ")";
      end
    | PrInt expr ->
      begin
        print_string "prInt (";
        pretty_aux (indent+1) expr;
        (* print_newline ();
        print_tab indent; *)
        print_string ")\n";
      end
    | Let (x, expr1, expr2) ->
      begin
        print_string (cons_color ^ "let " ^ var_color);
        pretty_pattern x;
        print_string (def_color ^ " = ");
        pretty_aux (indent+1) expr1;
        (* print_newline ();
        print_tab indent; *)
        print_string (cons_color ^ " in\n" ^ def_color);
        print_tab indent;
        pretty_aux indent expr2
      end
    | LetRec (f, expr1, expr2) ->
      begin
        print_string (cons_color ^ "let rec " ^ var_color ^ f ^ def_color ^ " = ");
        pretty_aux (indent+1) expr1;
        (* print_newline ();
        print_tab indent; *)
        print_string (cons_color ^ " in\n" ^ def_color);
        print_tab indent;
        pretty_aux indent expr2
      end
		| Match (expr, patmatch) -> failwith "TODO - match"
    | If (bexpr, expr) ->
      begin
        print_string (cons_color ^ "if " ^ def_color);
        bpretty_aux indent bexpr;
        print_newline ();
        print_tab indent;
        print_string (cons_color ^ "then\n" ^ def_color);
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr
      end
    | IfElse (bexpr, expr1, expr2) ->
      begin
        print_string (cons_color ^ "if " ^ def_color);
        pretty_aux indent bexpr;
        print_newline ();
        print_tab indent;
        print_string (cons_color ^ "then\n" ^ def_color);
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr1;
        print_newline ();
        print_tab indent;
        print_string (cons_color ^ "else\n" ^ def_color);
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr2;
      end
    | Fun (x, expr) ->
      begin
        print_string (cons_color ^ "fun " ^ def_color);
        pretty_pattern x;
        print_string " -> ";
        pretty_aux (indent+1) expr;
      end

    | App (expr1, expr2) ->
      begin
        pretty_aux indent expr1;
        print_string " (";
        pretty_aux indent expr2;
        print_string ")"
      end
    | Aff (expr1, expr2) ->
      begin
        print_string "(";
        pretty_aux indent expr1;
        print_string ") := (";
        pretty_aux indent expr2;
        print_string ")"
      end
    | Alloc expr ->
      begin
        print_string (cons_color ^ "ref (" ^ def_color);
        pretty_aux indent expr;
        print_string ")"
      end
    | Pair (expr1, expr2) ->
      begin
        print_string "(";
        pretty_aux indent expr1;
        print_string ", ";
        pretty_aux indent expr2;
        print_string ")"
      end
    | Try (expr1, x, expr2) ->
      begin
        print_string (cons_color ^ "try\n" ^ def_color);
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr1;
        print_string "\n";
        print_tab indent;
        print_string (cons_color ^ "with" ^ def_color ^ " E " ^ x ^ " -> ");
        pretty_aux (indent + 1) expr2;
      end
    | Raise a ->
      begin
        print_string (cons_color ^ "raise" ^ def_color ^ "(E ");
				pretty_aux (indent + 1) a;
				print_string ")"
      end

  and bpretty_aux indent = function
    | True -> print_string "true"
    | False -> print_string "false"
    | Cmp (expr1, cmp, expr2) ->
      begin
        pretty_aux (indent+1) expr1;
        pretty_cmp cmp;
        pretty_aux (indent+1) expr2
      end
    | Bin_op (bexpr1, op, bexpr2) ->
      begin
        print_string "(";
        bpretty_aux indent bexpr1;
        print_string ") ";
        pretty_bool_op op;
        print_string " (";
        bpretty_aux indent bexpr2;
        print_string ")"
      end
    | Not bexpr ->
      begin
        print_string "not(";
        bpretty_aux indent bexpr;
        print_string ")"
      end
  in


  pretty_aux 0 expr;
  print_newline ()
;;




(* On définit une un pretty-printer de valeurs *)

(* Le printeur de valeurs est définit co-inductivement avec le printeur
de clôtures. *)

let rec pretty_closure = function
(* pretty_closure : env_f -> unit
Prend en paramètre un environnement et tente de l'afficher joliment *)
	| [] -> print_string "[]"
	| (var, value) :: closure ->
		begin
			print_string (var ^ " ::= ");
			pretty_value 0 value;
			print_string " :: ";
			pretty_closure closure
		end

and pretty_value opt = function
(* pretty_value : int -> val_f -> unit
Prend en paramètre une valeur et tente de l'afficher correctement
L'argument entier est un renseignement pour améliorer l'affichage	*)

(* opt : FIXME inutilisé pour l'instant
	| 0 -> comportement usuel (i.e par défaut)
	| 1 -> liste de couples déjà parenthésée

FIXME :

si

type tree = Nil | Node of int * tree * tree ;;
let f (Node(x, _, _)) = print_int x ;;

Warning 8: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched:
Nil
val f : tree -> unit = <fun>

*)

	| Unit 	   -> print_string "()"
	| Bool b   -> print_string (string_of_bool b)
	| Int x 	 -> print_int x
	| Ref addr -> print_string ("ref " ^ (string_of_int addr))
	| Cons (c, value) ->
		begin
			print_string (c ^ "(");
			pretty_value 0 value;
			print_string ")"
		end
	| Pair_val (val1, val2) ->
		begin
			print_string "(";  pretty_value 0 val1;
			print_string ", "; pretty_value 0 val2;
			print_string ")"
		end
	| Fun_val (pat, expr, env) ->
		begin
			pretty_pattern pat;
			print_string " -> ";
			pretty_print_expr expr;
			print_string "\nin closure ";
			pretty_closure env
		end
