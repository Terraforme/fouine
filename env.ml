open Types


type env_f = (var_f, val_f) list


(* Fonctions pour manipuler l'environnement : *)

(* On définit 3 fonctions

env_read  : var_f -> env_f -> val_f
      qui cherche une variable dans un environnement et renvoie sa valeur
env_aff   : var_f -> val_f -> env_f -> env_f
      qui affecte une variable à une nouvelle valeur dans l'environnement
      renvoie un nouvel environnement à jour
env_unaff : var_f -> env_f -> env_f
      désaffecte la valeur d'une variable et renvoie le nouvel environnement *)

let rec env_read x = function
  | [] -> failwith "Read failed : variable not in environment"
  | (y, _) :: e when x <> y -> env_read x e
  | (x, val) :: _       -> val

let rec env_aff x value env =
  (x, value) :: env

let rec env_unaff x = function
  | [] -> failwith "Unaffectation failed : variable not in environment"
  | (y, val) :: e when x <> y -> (y, val) :: (env_unaff x e)
  | (x, val) :: e    -> e
