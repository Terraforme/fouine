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
  | [] -> failwith ("Read failed : variable " ^ x ^ " not in environment")
  | (y, _) :: e when x <> y -> env_read x e
  | (x, value) :: _       -> value

let rec env_aff x value env =
  if x = "_" then env
  else (x, value) :: env

let rec pat_env_aff pattern value env =
  match (pattern, value) with
  | (Var_Pat x, _) -> env_aff x value env
  (*| (Var_Pat x, Int _) -> env_aff x value env
  | (Var_Pat x, Fun_var (_,_,_)) -> env_aff x value env
  | (Var_Pat x, Ref _) -> env_aff x value env*)
  | (Pair_Pat (x, pattern), Pair_val(value0, value_next))
    -> pat_env_aff pattern value_next (env_aff x value0 env)
  | (_,_) -> failwith "error : Pattern Matching failed"

let rec env_unaff x = function
  | [] -> failwith "Unaffectation failed : variable not in environment"
  | (y, value) :: e when x <> y -> (y, value) :: (env_unaff x e)
  | (x, value) :: e    -> e
