open Env
open Types
open Mem



let rec regroup_pair pair1 pair2 = match pair1 with
  | Pair_val(value, pair1) -> Pair_val(value, regroup_pair pair1 pair2)
  | _ -> Pair_val(pair1, pair2)
;;

(* FIXME : exceptions ... *)

let rec eval expr env mem (* except *)= match expr with
(* eval : expr_f -> env_f -> mem_f -> val_f * mem_f * int option
Prend en paramètres une expression et un environnement,
et évalue l'expression sur cet environnement. La valeur renvoyée
est de type val_f décrite dans `types.ml`. On renvoie aussi
le nouvel état de la mémoire ainsi qu'une valeur d'exception.
Cette valeur d'exception est assez artificielle : elle demande 
juste à être renvoyée telle quelle jusqu'au prochain `try` pour 
être rattrapée. 

On passe la mémoire en argument. Bien-sûr, après l'évaluation
d'une expression, il faut la renvoyer en plus de la valeur *)

  | Bin (expr1, op, expr2) -> bin_eval op expr1 expr2 env mem
  | Var x                  -> env_read x env, mem, None
  | Bang expr              ->
    begin
      let (value, mem, e) = eval expr env mem in
			if e = None then
				match value with
				| Ref addr -> let value = read_mem addr mem
				              in value, mem, None
				| _ -> failwith "ERROR : Dereferencing non-addr"
			else (Unit, mem, e)
    end
  | Cst c                  -> Int c, mem, None
  | PrInt expr             ->
  (* On autorise seulement l'affichage d'entiers *)
    begin
      let (value, mem', e) = eval expr env mem in
			if e = None then
				begin
					let _ = match value with
					| Int a -> print_int a
					| _     -> failwith "ERROR : prInt : not an integer"
					in
					print_newline ();
					(value, mem', None)
				end
			else (Unit, mem', e)
    end
  | Let (pattern, expr1, expr2)  ->
  (* pattern : pour faire 'let (x, y) = c in' *)
    begin
      let  (value, mem', e) = eval expr1 env mem in
			if   e = None 			    then eval expr2 (pat_env_aff pattern value env) mem'
			else (Unit, mem', e)
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
      | _ -> let (value, mem', e) = eval expr1 env mem in
						 if e = None then eval expr2 (env_aff f value env) mem'
						 else (Unit, mem', e)
    end
  | If (bexpr, expr)       ->
  (* On pourrait se passer de ce constructeur en pratique
  C'est une relique du passé. *)
    begin
      let (value, mem', e) = bool_eval bexpr env mem in
			if e = None then 
				begin
				  if value then
				    eval expr env mem'
				  else
				    Int 0, mem', None
				end
			else (Unit, mem', e)
    end
  | IfElse (bexpr, expr1, expr2) ->
    begin
      let (value, mem', e) = bool_eval bexpr env mem in
			if e = None then
				begin
				  if value then
				    eval expr1 env mem'
				  else
				    eval expr2 env mem'
				end
			else (Unit, mem', e)
    end
  | Fun (x, expr0) -> Fun_val (x, expr0, env), mem, None
  | App (expr1, expr2) ->
  (* On évalue bien d'abord l'argument, puis la fonction *)
    begin
      let (value, mem, e) = eval expr2 env mem in
			if e = None then 
				begin
				  let (f, mem, e) = eval expr1 env mem in
					if e = None then
						match f with
						| Fun_val (pattern, expr0, env0) ->
						  eval expr0 (pat_env_aff pattern value env0) mem
						| _ -> failwith "ERROR : eval (App) : expecting a function"
					else (Unit, mem, e)
				end
			else (Unit, mem, e)
    end
  | Aff (expr1, expr2) ->
  (* Affectation : le terme gauche doit être une référence *)
    begin
      let (value0, mem, e) = eval expr2 env mem in
			if e = None then
		    let (value, mem, e) = eval expr1 env mem in
				if e = None then 
				  match value with
				  | Ref addr ->
				      let mem = set_mem addr value0 mem in
				      (Unit, mem, None)
				  | _ -> failwith "ERROR : affecting non-addr"
				else (Unit, mem, e)
			else (Unit, mem, e)
    end
  | Alloc (expr) ->
  (* L'allocation mémoire : le terme gauche a toutes les libertés *)
    begin
      let (value, mem, e) = eval expr env mem in
			if e = None then 
		    let mem, addr = alloc_mem value mem in
		    addr, mem, None
			else (Unit, mem, e)
    end
  | Pair (expr1, expr2) ->
  (* Les pairs ont une structure un peu particulière *)
    let (value, mem, e) = eval expr2 env mem in
		if e = None then
		  let (value0, mem, e) = eval expr1 env mem in
			if e = None then (regroup_pair value0 value), mem, None
			else (Unit, mem, e)
		else (Unit, mem, e)
  | Unit -> (Unit, mem, None)
  | Raise x -> (Unit, mem, Some x) 
  | Try (expr1, var_except, expr2) -> 
	(* Syntaxe: try expr1 with E var_except -> expr2 *)
		let (value, mem, e) = eval expr1 env mem in
		if e = None then (value, mem, None)
		else match e with
			| Some e_value -> eval expr2 (env_aff var_except (Int e_value) env) mem
			| _ -> failwith "Try : impossible case"


(* Pour les opérations ninaire : '+', '-' (...) '=', '<'
il y quelques subtilités sur les ordres, car '+' est
en réalité une fonction donc faire a + b c'est faire
((+) a) b). *)

and bin_eval op expr1 expr2 env mem0 =
(* bin_eval :
operator_f -> expr_f -> expr_f -> env_f -> mem_f-> val_f * mem_f
Sert à faire des opérations arithmétiques *)
  let (val2, mem1, e) = eval expr2 env mem0 in
	if e = None then 
		let (val1, mem2, e) = eval expr1 env mem1 in
		if e = None then
			match (val1, val2) with
			| (Int a, Int b) ->
				begin
					match op with
					| Plus  -> (Int(a + b), mem2, None)
					| Minus -> (Int(a - b), mem2, None)
					| Times -> (Int(a * b), mem2, None)
					| Div   -> (Int(a / b), mem2, None)
					| Mod   -> (Int(a mod b), mem2, None)
				end
			| _, _ -> failwith "ERROR : bin_eval : non-Int values "
		else (Unit, mem2, e)
	else (Unit, mem1, e)

and bool_eval bexpr env mem = match bexpr with
(* bool_eval : bexpr_f -> env_f -> mem_f -> bool
Évalue l'expression booléenne en entrée sur l'environnement donné *)
  | True -> (true, mem, None)
  | False -> (false, mem, None)
  | Cmp (expr1, cmp, expr2)     -> cmp_eval cmp expr1 expr2 env mem
  | Bin_op (bexpr1, op, bexpr2) -> bool_op_eval op bexpr1 bexpr2 env mem
  | Not bexpr                   ->
    let (val0, mem', e) = bool_eval bexpr env mem in
		if e = None then (not val0, mem', None)
		else (false, mem', e)	

and cmp_eval cmp expr1 expr2 env mem0 =
(* cmp_eval : cmp_op_f -> expr_f -> expr_f -> env_f -> mem_f -> bool_eval
Sert à comparer deux expressions *)
  let (val2, mem1, e) = eval expr2 env mem0 in
	if e = None then 
		let (val1, mem2, e) = eval expr1 env mem1 in
		if e = None then
			match (val1, val2) with
			| (Int a, Int b) ->
				begin
				  match cmp with
				  | Eq  -> (a = b,  mem2, None)
				  | Neq -> (a <> b, mem2, None)
				  | Leq -> (a <= b, mem2, None)
				  | Lt  -> (a < b,  mem2, None)
				  | Geq -> (a >= b, mem2, None)
				  | Gt  -> (a > b,  mem2, None)
				end
			| _,_ -> failwith "ERROR : cmp_eval : functional values"
		else (false, mem2, e)
	else (false, mem1, e)

and bool_op_eval op bexpr1 bexpr2 env mem0 = match op with
(* bool_op_eval : bool_op_f -> bexpr_f -> bexpr_f -> env_f -> mem_f -> bool_op_f
Sert à faire des opérations booléennes *)
  | Or  ->
    begin
      let (val1, mem1, e) = bool_eval bexpr1 env mem0 in
			if e = None then 
		    if val1 = true then (true, mem1, None)
		    else
		      let (val2, mem2, e) = bool_eval bexpr2 env mem1 in
					if e = None then (val2, mem2, None)
					else (false, mem2, e)
			else (false, mem1, e)
    end
  | And ->
    begin
      let (val1, mem1, e) = bool_eval bexpr1 env mem0 in
			if e = None then 
		    if val1 = false then (false, mem1, None)
		    else
		      let (val2, mem2, e) = bool_eval bexpr2 env mem1 in
		      if e = None then (val2, mem2, None)
					else (false, mem2, e)
			else (false, mem1, e)
    end
