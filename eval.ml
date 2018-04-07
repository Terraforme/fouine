open Env
open Types
open Mem



let rec regroup_pair pair1 pair2 = match pair1 with
  | Pair_val(value, pair1) -> Pair_val(value, regroup_pair pair1 pair2)
  | _ -> Pair_val(pair1, pair2)
;;

(* FIXME : exceptions ... *)

let rec eval expr env mem (* except *)= match expr with
(* eval : expr_f -> env_f -> mem_f -> val_f * mem_f
Prend en paramètres une expression et un environnement,
et évalue l'expression sur cet environnement. La valeur
renvoyée est soit un entier, soit une fonction.

On passe la mémoire en argument. Bien-sûr, après l'évaluation
d'une expression, il faut renvoyer en plus de la valeur *)

  | Bin (expr1, op, expr2) -> bin_eval op expr1 expr2 env mem
  | Var x                  -> env_read x env, mem
  | Bang expr              ->
    begin
      let (value, mem) = eval expr env mem in
      match value with
      | Ref addr -> let value = read_mem addr mem
                    in value, mem
      | _ -> failwith "ERROR : Dereferencing non-addr"
    end
  | Cst c                  -> Int c, mem
  | PrInt expr             ->
  (* On autorise seulement l'affichage d'entiers *)
    begin
      let (value, mem') = eval expr env mem in
      let _ = match value with
      | Int a -> print_int a
      | _     -> failwith "ERROR : prInt : not an integer"
      in
      print_newline ();
      (value, mem')
    end
  | Let (pattern, expr1, expr2)  ->
  (* pattern : pour faire 'let (x, y) = c in' *)
    begin
      let (value, mem') = eval expr1 env mem in
      eval expr2 (pat_env_aff pattern value env) mem'
    end
  | LetRec (f, expr1, expr2) ->
  (* f n'est pas un pattern : cf doc *)
    begin
      match expr1 with
      (* Deux cas pour f : soit c'est directement une fonction (rec)
         soit c'est une expression autre *)
      | Fun (pattern, expr0) ->
      (* Cas spécial ici.... la fonction env_aff ne suffit pas *)
        let rec env0 = (f, Fun_val(pattern, expr0, env0)) :: env in
        eval expr2 env0 mem
      | _ -> let (value, mem') = eval expr1 env mem in
             eval expr2 (env_aff f value env) mem'
    end
  | If (bexpr, expr)       ->
  (* On pourrait se passer de ce constructeur en pratique
  C'est une relique du passé. *)
    begin
      let (value, mem') = bool_eval bexpr env mem in
      if value then
        eval expr env mem'
      else
        Int 0, mem'
    end
  | IfElse (bexpr, expr1, expr2) ->
    begin
      let (value, mem') = bool_eval bexpr env mem in
      if value then
        eval expr1 env mem'
      else
        eval expr2 env mem'
    end
  | Fun (x, expr0) -> Fun_val (x, expr0, env), mem
  | App (expr1, expr2) ->
  (* On évalue bien d'abord l'argument, puis la fonction *)
    begin
      let (value, mem) = eval expr2 env mem in
      let (f,     mem) = eval expr1 env mem in
      match f with
      | Fun_val (pattern, expr0, env0) ->
        eval expr0 (pat_env_aff pattern value env0) mem
      | _ -> failwith "ERROR : eval (App) : expecting a function"
    end
  | Aff (expr1, expr2) ->
  (* Affectation : le terme gauche doit être une référence *)
    begin
      let (value0, mem) = eval expr2 env mem in
      let (value, mem) = eval expr1 env mem in
      match value with
      | Ref addr ->
          let mem = set_mem addr value0 mem in
          (Unit, mem)
      | _ -> failwith "ERROR : affecting non-addr"
    end
  | Alloc (expr) ->
  (* L'allocation mémoire : le terme gauche a toutes les libertés *)
    begin
      let (value, mem) = eval expr env mem in
      let mem, addr = alloc_mem value mem in
      addr, mem
    end
  | Pair (expr1, expr2) ->
  (* Les pairs ont une structure un peu particulière *)
    let (value, mem) = eval expr2 env mem in
    let (value0, mem) = eval expr1 env mem in
    (regroup_pair value0 value), mem
  | Unit -> (Unit, mem)
  | Raise _ -> failwith "TODO"
  | Try (_,_,_) -> failwith "TODO"

(* Pour les opérations ninaire : '+', '-' (...) '=', '<'
il y quelques subtilités sur les ordres, car '+' est
en réalité une fonction donc faire a + b c'est faire
((+) a) b). *)

and bin_eval op expr1 expr2 env mem0 =
(* bin_eval :
operator_f -> expr_f -> expr_f -> env_f -> mem_f-> val_f * mem_f
Sert à faire des opérations arithmétiques *)
  let (val2, mem1) = eval expr2 env mem0 in
  let (val1, mem2) = eval expr1 env mem1 in
  match (val1, val2) with
  | (Int a, Int b) ->
    begin
      match op with
      | Plus  -> (Int(a + b), mem2)
      | Minus -> (Int(a - b), mem2)
      | Times -> (Int(a * b), mem2)
      | Div   -> (Int(a / b), mem2)
      | Mod   -> (Int(a mod b), mem2)
    end
  | _, _ -> failwith "ERROR : bin_eval : non-Int values "

and bool_eval bexpr env mem = match bexpr with
(* bool_eval : bexpr_f -> env_f -> mem_f -> bool
Évalue l'expression booléenne en entrée sur l'environnement donné *)
  | True -> (true, mem)
  | False -> (false, mem)
  | Cmp (expr1, cmp, expr2)     -> cmp_eval cmp expr1 expr2 env mem
  | Bin_op (bexpr1, op, bexpr2) -> bool_op_eval op bexpr1 bexpr2 env mem
  | Not bexpr                   ->
    let (val0, mem') = bool_eval bexpr env mem in
    (not val0, mem')

and cmp_eval cmp expr1 expr2 env mem0 =
(* cmp_eval : cmp_op_f -> expr_f -> expr_f -> env_f -> mem_f -> bool_eval
Sert à comparer deux expressions *)
  let (val2, mem1) = eval expr2 env mem0 in
  let (val1, mem2) = eval expr1 env mem1 in
  match (val1, val2) with
  | (Int a, Int b) ->
    begin
      match cmp with
      | Eq  -> (a = b,  mem2)
      | Neq -> (a <> b, mem2)
      | Leq -> (a <= b, mem2)
      | Lt  -> (a < b,  mem2)
      | Geq -> (a >= b, mem2)
      | Gt  -> (a > b,  mem2)
    end
  | _,_ -> failwith "ERROR : cmp_eval : functional values"

and bool_op_eval op bexpr1 bexpr2 env mem0 = match op with
(* bool_op_eval : bool_op_f -> bexpr_f -> bexpr_f -> env_f -> mem_f -> bool_op_f
Sert à faire des opérations booléennes *)
  | Or  ->
    begin
      let (val1, mem1) = bool_eval bexpr1 env mem0 in
      if val1 = true then (true, mem1)
      else
        let (val2, mem2) = bool_eval bexpr2 env mem1 in
        (val2, mem2)
    end
  | And ->
    begin
      let (val1, mem1) = bool_eval bexpr1 env mem0 in
      if val1 = false then (false, mem1)
      else
        let (val2, mem2) = bool_eval bexpr2 env mem1 in
        (val2, mem2)
    end
