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
  | Bin (expr1, op, expr2) -> (op2str op) ^
  "(" ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | Var x -> x
  | Bang expr -> "!" ^ (expr2str expr)
  | Cst c -> string_of_int c
  | PrInt expr -> "prInt(" ^ (expr2str expr) ^ ")"
  | Let (x, expr1, expr2) -> "Let(" ^ (pattern2str x) ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | LetRec (f, expr1, expr2) -> "LetRec(" ^ f ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | If (bexpr, expr) -> "If(" ^ bexpr2str(bexpr) ^ ", " ^ expr2str(expr) ^ ")"
  | IfElse (bexpr, expr1, expr2) -> "If(" ^ bexpr2str(bexpr) ^ ", " ^ expr2str(expr1) ^ ", " ^ expr2str(expr2) ^ ")"
  | Fun (var, expr) -> "Fun " ^ (pattern2str var) ^ " -> (" ^ (expr2str expr) ^ ")"
  | App (expr1, expr2) -> (expr2str expr1) ^ "(" ^ (expr2str expr2) ^ ")"
  | Aff (expr1, expr2) -> "(" ^ (expr2str expr1)^ ") := " ^ "(" ^ (expr2str expr2) ^ ")"
  | Alloc expr -> "Alloc(" ^ (expr2str expr) ^ ")"
  | Pair (expr1, expr2) -> "Pair("^(expr2str expr1)^" , "^(expr2str expr2)^")"
  | Unit -> "()"
  | Try (expr1, var, expr2) -> "Try(" ^ (expr2str expr1) ^ ", with E " ^ var ^ " -> " ^ (expr2str expr2) ^ ")"
  | Raise expr -> "Raise(" ^ (expr2str expr) ^ ")"

and bexpr2str = function
  | True -> "true"
  | False -> "false"
  | Cmp (expr1, op, expr2) -> cmp_op2str(op) ^ "(" ^ expr2str(expr1) ^ ", " ^ expr2str(expr2) ^ ")"
  | Bin_op(bexpr1, op, bexpr2) -> bool_op2str(op) ^ "(" ^ bexpr2str(bexpr1) ^ ", " ^ bexpr2str(bexpr2) ^ ")"
  | Not bexpr -> "Not(" ^ bexpr2str(bexpr) ^ ")"

and pattern2str = function
  | Var_Pat x -> x
  | Pair_Pat (x, pattern) -> "Pair(" ^ x ^ "," ^ (pattern2str pattern) ^ ")"

let print_expr expr = print_string (expr2str expr) ; print_newline ()



(* Le pretty printer :
Le code qu'il donne peut être normalement utilisé
en code OCamL *)


let pretty_op2str = function
  | Plus  -> print_string " + "
  | Minus -> print_string " - "
  | Times -> print_string " * "
  | Div   -> print_string " / "
  | Mod   -> print_string " % "

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
  | Pair_Pat (x, pattern) ->
    begin
      print_string ("(" ^ x);
      pretty_pattern pattern;
      print_string ")"
    end


let pretty_print_expr expr =

  let print_tab n =
    for i = 0 to n-1 do print_string "\t" done
  in

  let rec pretty_aux indent = function
    | Bin (expr1, op, expr2) ->
      begin
        print_string "(";
        pretty_aux indent expr1;
        pretty_op2str op;
        pretty_aux indent expr2;
        print_string ")"
      end
    | Var x -> print_string x
    | Cst c -> print_int c
    | Unit -> print_string "()"
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
        print_string ("let ");
        pretty_pattern x;
        print_string " = ";
        pretty_aux (indent+1) expr1;
        (* print_newline ();
        print_tab indent; *)
        print_string " in\n";
        print_tab indent;
        pretty_aux indent expr2
      end
    | LetRec (f, expr1, expr2) ->
      begin
        print_string ("let rec " ^ f ^ " = ");
        pretty_aux (indent+1) expr1;
        (* print_newline ();
        print_tab indent; *)
        print_string " in\n";
        print_tab indent;
        pretty_aux indent expr2
      end
    | If (bexpr, expr) ->
      begin
        print_string "if ";
        bpretty_aux indent bexpr;
        print_newline ();
        print_tab indent;
        print_string "then\n";
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr
      end
    | IfElse (bexpr, expr1, expr2) ->
      begin
        print_string "if ";
        bpretty_aux indent bexpr;
        print_newline ();
        print_tab indent;
        print_string "then\n";
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr1;
        print_newline ();
        print_tab indent;
        print_string "else\n";
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr2;
      end
    | Fun (x, expr) ->
      begin
        print_string "fun ";
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
        print_string "ref (";
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
        print_string "try\n";
        print_tab (indent + 1);
        pretty_aux (indent + 1) expr1;
        print_string "\n";
        print_tab indent;
        print_string ("with E " ^ x ^ " -> ");
        pretty_aux (indent + 1) expr2;
      end
    | Raise expr ->
      begin
        print_string "raise ";
        pretty_aux (indent + 1) expr;
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
