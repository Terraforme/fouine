open Types

(** On définit ici les types nécessaires au type checker *)

(* Ainsi que les variables globales, méthodes principales *)

type type_f = 
  | Unit_t
  | Int_t 
  | Bool_t 
  | Poly_t  of int
  | Weak_t  of int
  | Ref_t   of type_f
  | Pair_t  of type_f * type_f
  | Fun_t   of type_f * type_f
;;

type tenv_f = (var_f * int) list
type type_vindex_f = type_f array
;;

let tindex = Array.make 1000000 Unit_t
let tnext  = ref 0 
;;

(* INDEX *)

let new_poly () = 
(* Crée un nouveau type polymorphe et renvoie son id *)
  tindex.(!tnext) <- Poly_t !tnext;
  incr tnext;
  !tnext - 1

let new_weak () = 
(* Crée un nouveau type polymorphe faible et renvoie son id *)
  tindex.(!tnext) <- Weak_t !tnext;
  incr tnext;
  !tnext - 1

let type_of_id i = tindex.(i)
(* ENVIRONNEMENT *)
(* Lecture de l'environnement *)

let rec tenv_get_id x = function
(* Donne l'id (tindex) de x dans l'environnement *)
  | [] -> failwith ("Unknown variable " ^ x)
  | (y, _) :: env when y <> x -> tenv_get_id x env
  | (x, i) :: _               -> i

let rec tenv_read x = function 
(* Renvoie le type associé à x dans l'environnement *)
  | [] -> failwith ("Unknown variable " ^ x)
  | (y, _) :: env when y <> x -> tenv_read x env
  | (x, i) :: _               -> tindex.(i)

let rec tenv_read_pat p env = match p with
(* Renvoie le type associé au pattern p dans l'environnement *)
  | Var_Pat x -> tenv_read x env
  | Pair_Pat (p1, p2) -> Pair_t(tenv_read_pat p1 env, tenv_read_pat p2 env)
  | _                 -> failwith "Unsupported pattern"


(* Mise à jour de l'environnement *)

let tenv_new x t env = 
(* ALloue un nouveau type t pour x *)
  tindex.(!tnext) <- t;
  incr tnext;
  (x, !tnext - 1) :: env

let tenv_new_poly x env = 
(* Alloue un nouveau type polymorphe pour x *)
  tindex.(!tnext) <- Poly_t !tnext;
  incr tnext;
  (x, !tnext - 1) :: env

let rec tenv_new_poly_pat p env = match p with
(* Alloue pour chaque variable dans le pattern un nouveau type polymorphe *)
  | Var_Pat x -> tenv_new_poly x env
  | Pair_Pat (p1, p2) -> let env = tenv_new_poly_pat p1 env in
                         tenv_new_poly_pat p2 env
  | _ -> failwith "Unsupported pattern"
  
let tenv_new_weak x env = 
(* Alloue un nouveau type polymorphe faible pour x *)
  tindex.(!tnext) <- Weak_t !tnext;
  incr tnext;
  (x, !tnext - 1) :: env

let rec tenv_new_weak_pat p env = match p with
(* Alloue pour chaque variable dans le pattern un nouveau type polymorphe faible *)
  | Var_Pat x -> tenv_new_weak x env
  | Pair_Pat (p1, p2) -> let env = tenv_new_weak_pat p1 env in
                         tenv_new_weak_pat p2 env
  | _ -> failwith "Unsupported pattern"

let tenv_upd x s env = 
(* Change le type de x dans l'index *)
  let i = tenv_get_id x env in
  tindex.(i) <- s

let rec tenv_upd_pat p s env = match p with
(* Change le type de toutes les variables dans le pattern p *)
  | Var_Pat x -> tenv_upd x s env
  | Pair_Pat (p1, p2) -> begin match s with
                          | Pair_t (s, t) -> tenv_upd_pat p1 s env;
                                             tenv_upd_pat p2 t env
                          | _ -> failwith "Unmatching pattern"
                         end
  | _ -> failwith "Unsupported pattern"

