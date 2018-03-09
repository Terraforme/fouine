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
  | LetRec (f, expr1, expr2) ->
    begin
      match expr1 with
      (* Deux cas pour f : soit c'est directement une fonction (rec)
         soit c'est une expression autre *)
      | Fun (x, expr0) ->
      (* Cas spécial ici.... la fonction env_aff ne suffit pas *)
        let rec env0 = (f, Fun_var(x, expr0, env0)) :: env in
        eval expr2 env0
      | _ -> let value = eval expr1 env in
             eval expr2 (env_aff f value env)
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
