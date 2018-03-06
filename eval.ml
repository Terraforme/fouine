open Env
open Types




let rec eval expr env = match expr with
(* eval : expr_f -> env_f -> val_f
Prend en paramètres une expression et un environnement,
et évalue l'expression sur cet environnement. La valeur
renvoyée est soit un entier, soit une fonction *)

  | Bin (expr1, op, expr2) -> bin_eval op expr1 expr2 env
  | Var x                  -> env_read x env
  | Cst c                  -> Int c
  | PrInt expr             ->
    begin
      let value = eval expr env in
      let _ = match value with
      | Int a -> print_int a
      | _     -> failwith "ERROR : prInt : functional value"
      in
      print_newline ();
      value
    end
  | Let (x, expr1, expr2)  ->
    begin
      let value = eval expr1 env in
      eval expr2 (env_aff x value env)
    end
  | If (bexpr, expr)       ->
    begin
      if (bool_eval bexpr env) then
        eval expr env
      else
        Int 0
    end
  | IfElse (bexpr, expr1, expr2) ->
    begin
      if (bool_eval bexpr env) then
        eval expr1 env
      else
        eval expr2 env
    end
  | Fun (x, expr0) -> Fun_var (x, expr0, env)
  | App (expr1, expr2) ->
    begin
      match (eval expr1 env) with
      | Fun_var (x, expr0, env0) ->
        eval expr0 (env_aff x (eval expr2 env) env0)
      | _ -> failwith "ERROR : eval (App) : expecting a function"
    end


and bin_eval op expr1 expr2 env =
(* bin_eval : operator_f -> expr_f -> expr_f -> env_f -> val_f
Sert à faire des opérations arithmétiques *)
  match (eval expr1 env, eval expr2 env) with
  | (Int a, Int b) ->
    begin
      match op with
      | Plus  -> Int(a + b)
      | Minus -> Int(a - b)
      | Times -> Int(a * b)
      | Div   -> Int(a / b)
      | Mod   -> Int(a mod b)
    end
  | _, _ -> failwith "ERROR : bin_eval : functional values "

and bool_eval bexpr env = match bexpr with
(* bool_eval : bexpr_f -> env_f -> bool
Évalue l'expression booléenne en entrée sur l'environnement donné *)
  | True -> true
  | False -> false
  | Cmp (expr1, cmp, expr2)     -> cmp_eval cmp expr1 expr2 env
  | Bin_op (bexpr1, op, bexpr2) -> bool_op_eval op bexpr1 bexpr2 env
  | Not bexpr                   -> not (bool_eval bexpr env)

and cmp_eval cmp expr1 expr2 env =
(* cmp_eval : cmp_op_f -> expr_f -> expr_f -> env_f -> bool_eval
Sert à comparer deux expressions *)
  match (eval expr1 env, eval expr2 env) with
  | (Int a, Int b) ->
    begin
      match cmp with
      | Eq  -> a = b
      | Neq -> a <> b
      | Leq -> a <= b
      | Lt  -> a < b
      | Geq -> a >= b
      | Gt  -> a > b
    end
  | _,_ -> failwith "ERROR : cmp_eval : functional values"

and bool_op_eval op bexpr1 bexpr2 env = match op with
(* bool_op_eval : bool_op_f -> bexpr_f -> bexpr_f -> env_f -> bool_op_f
Sert à faire des opérations booléennes *)
  | Or  -> (bool_eval bexpr1 env) || (bool_eval bexpr2 env)
  | And -> (bool_eval bexpr1 env) && (bool_eval bexpr2 env)













(* ************** Fonctions d'affichage ************** *)

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
  | Cst c -> string_of_int c
  | PrInt expr -> "prInt(" ^ (expr2str expr) ^ ")"
  | Let (x, expr1, expr2) -> "Let(" ^ x ^ ", "
  ^ (expr2str expr1) ^ ", " ^ (expr2str expr2) ^ ")"
  | If (bexpr, expr) -> "If(" ^ bexpr2str(bexpr) ^ ", " ^ expr2str(expr) ^ ")"
  | IfElse (bexpr, expr1, expr2) -> "If(" ^ bexpr2str(bexpr) ^ ", " ^ expr2str(expr1) ^ ", " ^ expr2str(expr2) ^ ")"
  | Fun (var, expr) -> "Fun " ^ var ^ " -> (" ^ (expr2str expr) ^ ")"
  | App (expr1, expr2) -> (expr2str expr1) ^ "(" ^ (expr2str expr2) ^ ")"

and bexpr2str = function
  | True -> "true"
  | False -> "false"
  | Cmp (expr1, op, expr2) -> cmp_op2str(op) ^ "(" ^ expr2str(expr1) ^ ", " ^ expr2str(expr2) ^ ")"
  | Bin_op(bexpr1, op, bexpr2) -> bool_op2str(op) ^ "(" ^ bexpr2str(bexpr1) ^ ", " ^ bexpr2str(bexpr2) ^ ")"
  | Not bexpr -> "Not(" ^ bexpr2str(bexpr) ^ ")"

let print_expr expr = print_string (expr2str expr) ; print_newline ()


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
        print_string ("let " ^ x ^ " = ");
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
      print_string ("fun " ^ x ^ " -> ");
      pretty_aux (indent+1) expr;

    | App (expr1, expr2) ->
      pretty_aux indent expr1;
      print_string "(";
      pretty_aux indent expr2;
      print_string ")"

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
