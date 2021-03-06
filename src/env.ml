open Types


(* Fonctions pour manipuler l'environnement : *)

(* On définit 3 fonctions

env_read  : var_f -> env_f -> val_f
      qui cherche une variable dans un environnement et renvoie sa valeur

env_aff   : var_f -> val_f -> env_f -> env_f
      qui affecte une variable à une nouvelle valeur dans l'environnement
      renvoie un nouvel environnement à jour

env_unaff : var_f -> env_f -> env_f
      désaffecte la valeur d'une variable et renvoie le nouvel environnement
      dans les faits, cette fonction est inutile*)

let rec env_read x env =
  if x = "_" then failwith "ERROR : trying to read '_'"
  else match env with
  | [] -> failwith ("ERROR : Read failed : variable " ^ x ^ " not in environment")
  | (y, _) :: e when x <> y -> env_read x e
  | (x, value) :: _       -> value

let rec env_aff x value env =
  if x = "_" then env
  else (x, value) :: env

let rec pat_env_aff pattern value env =
  match (pattern, value) with
  | (Var_Pat x, _) -> env_aff x value env
  | (Pair_Pat (pat1, pat2), Pair_val(val1, val2))
    -> pat_env_aff pat2 val2 (pat_env_aff pat1 val1 env)
  | (_,_) -> failwith "ERROR : Pattern Matching failed"

let rec env_unaff x = function
  | [] -> failwith "ERROR : Unaffectation failed : variable not in environment"
  | (y, value) :: e when x <> y -> (y, value) :: (env_unaff x e)
  | (x, value) :: e    -> e
