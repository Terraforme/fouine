open Env
open Types
open Mem
open Printer


let id = fun x -> x ;;

let rec regroup_pair pair1 pair2 = match pair1 with
  | Pair_val(value, pair1) -> Pair_val(value, regroup_pair pair1 pair2)
  | _ -> Pair_val(pair1, pair2)
;;

(* FIXME : exceptions ...  :
Environnements locaux des exceptions *)


let rec eval expr env k k' = match expr with
(* eval :
expr_f -> env_f -> (val_f -> val_f) -> ((val_f -> val_f) * env_f) list -> val_f

La fonction d'évaluation principale. Fonctionne en style par continuation
La mémoire est globale et d'utilisation transparente.
Pour les continuations : k correspond à la continuation normale et
                         k' à une pile de couple (continuations * environnement)
                            correspondant aux scénarios exceptionnels *)
  | Neg expr               -> 
        eval expr env (function   Bool b -> k (Bool (not b)) 
                                | _ -> failwith "eval : non-Bool value") k'
  | Bin (expr1, op, expr2) ->
    begin match op with
    | Or -> eval expr1 env (function
                            | Bool b1 -> if b1 then Bool true else 
                                         eval expr2 env (function
                                                         | Bool b2 as val2 -> val2
                                                         | _ -> debug_print expr expr2;
                                                                failwith "eval : non-Bool value") k'
                            | _ -> debug_print expr expr1; failwith "eval : non-Bool value") 
                            k'
    | And ->  eval expr1 env (fun val1 -> match val1 with 
                            | Bool b1 -> if not b1 then Bool false else 
                                         eval expr2 env (function
                                                         | Bool b2 as val2 -> val2
                                                         | _ -> failwith "eval : non-Bool value") k'
                            | _ -> failwith "eval : non-Bool value") 
                            k'
    | _ -> eval expr2 env (fun val2 ->
                           eval expr1 env (fun val1 -> k (aeval op val1 val2)) k') k'
    end
  | Bool b                 -> k (Bool b)
  | Var x                  -> k (env_read x env)
  | Bang expr              ->
         eval expr env (function   Ref  addr -> k (read_mem addr)
                                 | _ -> debug_print (Bang expr) expr; 
                                        failwith "! : Dereferecing non-addr") k'
  | Cst c                  -> k (Int c)
  | PrInt expr             ->
  (* On autorise seulement l'affichage d'entiers *)
    eval expr env (function Int a -> begin print_int a; print_newline ();
                                       k (Int a)
                                     end
                            | _ -> debug_print (PrInt expr) expr; failwith "prInt : non-int") k'
  | Let (pattern, expr1, expr2)  ->
  (* pattern : pour faire 'let (x, y) = c in' *)
    eval expr1 env (fun value -> eval expr2 (pat_env_aff pattern value env) k k') k'
  | LetRec (f, expr1, expr2) ->
  (* f n'est pas un pattern : cf doc *)
		begin
      match expr1 with | Fun (pat, expr0) ->
                         let rec env0 = (f, Fun_val (pat, expr0, env0)) :: env in
                         eval expr2 env0 k k'
                       | _ -> eval (Let (Var_Pat f, expr1, expr2)) env k k'
    end
	| Match (expr, pmatch)   -> failwith "TODO - Matchings"
  | If (bexpr, expr)       ->
  (* On pourrait se passer de ce constructeur en pratique
  C'est une relique du passé. *)
    failwith "TODO"
  | IfElse (bexpr, expr1, expr2) ->
    eval bexpr env (function Bool b -> eval (if b then expr1 else expr2) env k k'
                             | _ -> debug_print expr bexpr; failwith "eval : non-Boolean value in a test") k'
  | Fun (x, expr0) -> k (Fun_val (x, expr0, env))
  | App (expr1, expr2) ->
  (* On évalue bien d'abord l'argument, puis la fonction *)
    eval expr2 env (fun value -> eval expr1 env
                   (function Fun_val (x, expr0, env) ->
                                eval expr0 (pat_env_aff x value env) k k'
                             | _ -> debug_print expr expr1; failwith "App : applicator is not a function")
                    k') k'
  | Aff (expr1, expr2) ->
  (* Affectation : le terme gauche doit être une référence *)
		eval expr2 env (fun value -> eval expr1 env
                   (function Ref addr -> begin set_mem addr value; k Unit end
                             | _ -> debug_print expr expr1; 
                                    failwith "Aff : affecting non-ref" ) k') k'
  | Alloc (expr) ->
  (* L'allocation mémoire : le terme gauche a toutes les libertés *)
    eval expr env (fun value -> let addr = alloc_mem value in k (Ref addr)) k'
  | Pair (expr1, expr2) ->
		eval expr2 env (fun val2 ->
                    eval expr1 env (fun val1 -> k (Pair_val(val1, val2))) k') k'
  | Unit -> k Unit
  | Raise expr ->
		begin
		  match k' with
		  | [] -> failwith "Raise : Nothing to catch exception"
		  | k_exn :: k' -> eval expr env k_exn k'
    end
  | Try (expr1, var_except, expr2) ->
	(* Syntaxe: try expr1 with E var_except -> expr2 *)
    eval expr1 env k ((fun exn -> eval expr2 (env_aff var_except exn env) k k') :: k')


(* Pour les opérations ninaire : '+', '-' (...) '=', '<'
il y quelques subtilités sur les ordres, car '+' est
en réalité une fonction donc faire a + b c'est faire
((+) a) b). *)

and aeval op val1 val2 =
(* FIXME *)
(* aeval :
operator_f -> val_f -> val_f -> val_f
Sert à faire des opérations arithmétiques *)
  match val1, val2 with
  | Int a, Int b ->
    begin match op with
      | Plus  -> Int(a + b)
      | Minus -> Int(a - b)
      | Times -> Int(a * b)
      | Div   -> Int(a / b)
      | Mod   -> Int(a mod b)
      | Eq    -> Bool(a =  b)
      | Neq   -> Bool(a <> b)
      | Leq   -> Bool(a <= b)
      | Lt    -> Bool(a <  b)
      | Geq   -> Bool(a >= b)
      | Gt    -> Bool(a >  b)
      | _     -> failwith "aeval : non-Int operator"
    end
  | Bool b1, Bool b2 -> 
    begin match op with
      | Or    -> Bool(b1 || b2)
      | And   -> Bool(b1 && b2)
      | _     -> failwith "aeval : non-Bool operator"
    end
  | Unit, Unit -> 
    begin match op with
      | Eq  -> Bool true
      | Neq -> Bool false
      | Lt  -> Bool false
      | Leq -> Bool true
      | Gt  -> Bool false
      | Geq -> Bool true
      | _   -> failwith "aeval : non-Unit operator"
    end
   | _, _ ->
    begin match op with
      | Eq  -> Bool false
      | Neq -> Bool true
      | Lt  -> Bool false
      | Leq -> Bool false
      | Gt  -> Bool false
      | Geq -> Bool false
      | _   -> failwith "aeval : operation between different types"
    end
   

and beval bexpr env k k' = match bexpr with
(* beval :

bexpr_f -> env_f -> (bool -> val_f) -> ((val_f -> val_f) * env_f) list -> val_f

Évalue l'expression booléenne en entrée sur l'environnement donné
La continuation normale considérée est différentes des autres :
de type "bool" -> val_f au lieu de val_f -> val_f *)
  | True -> k true
  | False -> k false
  | Cmp (expr1, cmp, expr2)     ->
    eval expr2 env (fun val2 -> eval expr1 env
                               (fun val1 -> k (cmp_eval cmp val1 val2)) k') k'
  | Bin_op (bexpr1, op, bexpr2) ->
    (* Je n'encapsule pas cette section à cause de l'évaluation fainéante *)
    begin match op with
    | Or  -> beval bexpr1 env (fun b1 -> if b1 then k true
                                         else beval bexpr2 env
                                             (fun b2 -> k b2) k' ) k'
    | And -> beval bexpr1 env (fun b1 -> if not b1 then k false
                                         else beval bexpr2 env
                                             (fun b2 -> k b2) k' ) k'
    end
  | Not bexpr                   -> beval bexpr env (fun b -> k (not b)) k'

and cmp_eval cmp val1 val2 =
(* cmp_eval : cmp_op_f -> val_f -> val_f -> bool
Sert à comparer deux valeurs (à priori entières) *)
	match val1, val2 with
  | Int a, Int b ->
    begin
      match cmp with
      | Eq  -> a =  b
      | Neq -> a <> b
      | Leq -> a <= b
      | Lt  -> a <  b
      | Geq -> a >= b
      | Gt  -> a >  b
    end
  | Unit, Unit -> true
  | Unit,_ | _, Unit -> false
  | _, _ -> failwith "cmp_eval : non-int values"
