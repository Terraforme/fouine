open Types
open CheckerTypes
open TypePrinter
open Unification



let rec is_generalizable expr = match expr with
  | Var _  -> true
  | Cst _  -> true
  | Bool _ -> true
  | Unit   -> true
  | Bang e -> false
  | Pair (e1, e2) -> (is_generalizable e1) && (is_generalizable e2)
  | Neg e -> true
  | Bin (e1, op, e2) -> false
  | PrInt e -> true
  | IfElse (_, e1, e2) -> (is_generalizable e1) && (is_generalizable e2)
  | Let (_, e1, e2) -> (is_generalizable e1) && (is_generalizable e2)
  | LetRec(f, e1, e2) -> (is_generalizable e1) && (is_generalizable e2)
  | Match (_, _) -> failwith "Unsupported match"
  | Fun (x, e) -> true
  | App (e1, e2) -> false
  | Aff (e1, e2) -> false
  | Alloc e -> false
  | Try (e1, _, e2) -> (is_generalizable e1) && (is_generalizable e2)
  | Raise e -> true


let rec print_constraints = function
  | [] -> ()
  | (s, t) :: cl -> print_type s; print_string " = "; print_type t; print_newline();
                    print_constraints cl
let rec print_substitution = function 
  | [] -> ()
  | (i, t) :: sub -> print_int i; print_string " is "; print_type t; print_newline();
                     print_substitution sub
                     
let type_op = function
(* Selon l'opérateur op : 'a -> 'b -> 'c, renvoie ('a, 'b, 'c) *)
  | Plus | Minus | Times | Div | Mod  -> (Int_t, Int_t, Int_t)
  | Eq   | Neq   | Leq   | Lt  | Geq | Gt -> (Weak_t(new_poly ()), Weak_t(new_poly ()), Bool_t)
  | Or | And -> (Bool_t, Bool_t, Bool_t)

let add_constraint_on x t env c = (tenv_read x env, t) :: c

let rec add_constraint_on_pattern p t env c = match (p, t) with
  | (Var_Pat x, _) -> (tenv_read x env, t) :: c
  | (Pair_Pat (p1, p2), Pair_t(t1, t2)) -> let c1 = add_constraint_on_pattern p1 t1 env c in
                                           add_constraint_on_pattern p2 t2 env c1
  | (_, _) -> failwith "Unmatching pattern"

let rec substitute id s t = match t with
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t 
  | Poly_t id' -> if id = id' then s else t
  | Weak_t id' -> if id = id' then s else t
  | Fun_t  (t0, t1) -> Fun_t (substitute id s t0, substitute id s t1)
  | Pair_t (t0, t1) -> Pair_t(substitute id s t0, substitute id s t1)
  | Ref_t t0 -> Ref_t (substitute id s t0)

let apply_sub sub t =
  List.fold_right (fun (id, s) -> substitute id s) sub t

let rec is_present x = function
  | Unit_t | Int_t | Bool_t -> false
  | Poly_t y | Weak_t y -> if x = y then true else false
  | Ref_t  s -> is_present x s
  | Fun_t (s, t) | Pair_t (s, t) -> if is_present x s then true else is_present x t

let rec atomic_unify s t =  
  (*print_string "unifying ";
  print_type s;
  print_string " <= ";
  print_type t;
  print_newline ();*)
  match (s, t) with
(* Cas sans variables *)
  | (Int_t, Int_t)   -> []
  | (Bool_t, Bool_t) -> []
  | (Unit_t, Unit_t) -> []
  | (Poly_t x, Poly_t y) -> if x = y then [] else [x, t]
  | (Weak_t x, Weak_t y) -> if x = y then [] else [x, t]
  | (Ref_t s0, Ref_t t0) -> atomic_unify s0 t0
  | (Fun_t(s1, s2), Fun_t(t1, t2))   -> 
      let sub' = atomic_unify s1 t1 in
      let sub  = atomic_unify (apply_sub sub' s2) (apply_sub sub' t2) in
      sub @ sub'
  | (Pair_t(s1, s2), Pair_t(t1, t2)) -> 
      let sub' = atomic_unify s1 t1 in
      let sub  = atomic_unify (apply_sub sub' s2) (apply_sub sub' t2) in
      sub @ sub'
(* Cas avec variables - non terminal *)
  | (Weak_t x, Ref_t r) | (Ref_t r, Weak_t x) -> 
      if is_present x r then failwith "Impossible to unify : cycle"
      else [(x, Ref_t r)]
  | (Weak_t x, Fun_t(r1, r2)) | (Fun_t(r1, r2), Weak_t x) -> 
      if is_present x r1 || is_present x r2 then failwith "Impossible to unify : cycle"
      else [(x, Fun_t(r1, r2))]
  | (Weak_t x, Pair_t(r1, r2)) | (Pair_t(r1, r2), Weak_t x) ->
      if is_present x r1 || is_present x r2 then failwith "Impossible to unify : cycle"
      else [(x, Pair_t(r1, r2))]
(* Cas variables - terminal *)
  | (Weak_t x, _) -> [x, t]
  | (_, Weak_t x) -> [x, s]
  | (Poly_t x, _) -> [] (* FIXME : instancier le type *)
  | (_, Poly_t x) -> failwith ((string_of_type s) ^ " cannot be more general than " ^ (string_of_type t))
  | _ -> failwith ("Impossible to unify : unmatching types " ^ (string_of_type s) ^ " and " ^ (string_of_type t))
  
and unify sub = function
  | [] -> sub
  | (s, t) :: cl -> let sub' = unify sub cl in
                    let sub  = atomic_unify (apply_sub sub' s) (apply_sub sub' t) in
                    sub @ sub'


(* Généralisation du let *)
(* Les variables sont initialisées avec le polymorphsime faible lors d'un let.
 * i.e on a let x = e1 in e2. Lorsqu'on a fini l'appel sur e1, on unifie les 
 * contraintes trouver. Cela permet de typer x. Ensuite, on GÉNÉRALISE le 
 * type de x : si id(x) = i, on parcourt sub(i) et on remplace tous les Weak_t j
 * par des Poly_t j, si j > i *)

let rec gen_rebuilt t i = match t with
  | Int_t | Unit_t | Bool_t -> t
  | Poly_t _ -> t
  | Weak_t j -> if (j >= i) then Poly_t j else t
  | Ref_t  t      -> Ref_t (gen_rebuilt t i)
  | Pair_t (s, t) -> Pair_t (gen_rebuilt s i, gen_rebuilt t i)
  | Fun_t  (s, t) -> Fun_t  (gen_rebuilt s i, gen_rebuilt t i)

let generalize x sub env = 
  let i = tenv_get_id x env in
  let t = apply_sub sub (type_of_id i) in
  tindex.(i) <- gen_rebuilt t i

let rec generalize_pattern p sub env = match p with
  | Var_Pat x        -> generalize x sub env
  | Pair_Pat(p1, p2) -> generalize_pattern p1 sub env; generalize_pattern p2 sub env
  | _ -> failwith "generalize_pattern : unsupported pattern"

let generalize_expr expr t = if is_generalizable expr then gen_rebuilt t 0
                             else t


(* Instantiation des variables *)
(* Une instance est une liste d'association int * type_f où id est l'id d'un type *)

let rec is_instantiated id instance = match instance with
  | [] -> false
  | (id', _) :: instance when id <> id' -> is_instantiated id instance
  | (id,  _) :: _                       -> true

let rec instance_of id instance = match instance with
  | [] -> failwith "type not instantiated"
  | (id', _) :: instance when id <> id' -> instance_of id instance
  | (id,  t) :: _                       -> t

let new_instance id instance =
  let id' = new_weak () in
  (id, type_of_id id') :: instance

let instantiate x env =
(* Construit une instance pour le type de x *)
  let t = tenv_read x env 
  and rinstance = ref [] in
  
  let rec built_instance t = match t with
    | Int_t | Bool_t | Unit_t -> t
    | Weak_t i  -> t
    | Poly_t id -> if not (is_instantiated id !rinstance) 
                   then rinstance := new_instance id !rinstance;
                   instance_of id !rinstance
    | Ref_t t       -> Ref_t  (built_instance t)
    | Fun_t  (s, t) -> Fun_t  (built_instance s, built_instance t)
    | Pair_t (s, t) -> Pair_t (built_instance s, built_instance t)
  in
  built_instance t

(* Notation : [(s, t)] :: c i.e s <= t i.e s plus général que t *)
let rec collect_constraint expr env sub = match expr with
  | Var x               -> instantiate x env, [], sub
  | Bool _              -> Bool_t, [], sub
  | Cst _               -> Int_t, [], sub
  | Unit                -> Unit_t, [], sub
  | Pair (e1, e2)       -> let s, c1, sub = collect_constraint e1 env sub in
                           let t, c2, sub = collect_constraint e2 env sub in
                           Pair_t (s, t), c2 @ c1, sub
  | Bang e              -> let s, c, sub1 = collect_constraint e env sub in
                           begin match s with
                           | Ref_t t -> t, c, sub
                           | _ -> failwith "Dereferencing non ref"
                           end
                            
  | Neg e               -> let s, c, sub = collect_constraint e env sub in
                           Bool_t, (Bool_t, s) :: c, sub
  | Bin (e1, op, e2)    -> let (s, t, r) = type_op op in
                           let s1, c1, sub = collect_constraint e1 env sub in
                           let t1, c2, sub = collect_constraint e2 env sub in
                           r, (t, t1) :: (s, s1) :: (c2 @ c1), sub
  | PrInt e             -> let s, c, sub = collect_constraint e env sub in
                           Int_t, (Int_t, apply_sub sub s) :: c, sub
  | Let (pat, e1, e2)   -> let env = tenv_new_weak_pat pat env in
                           let s, c, sub = collect_constraint e1 env sub in
                           let c = add_constraint_on_pattern pat s env c in
                           let sub = (unify sub c) in
                           if (is_generalizable expr) then generalize_pattern pat sub env;
                           collect_constraint e2 env sub
  | LetRec (f, e1, e2)  -> (* Pour le let rec : on ne rajoute pas de contraintes
                            * après le typage de e1 : elles ont déjà été calculées
                            * si f est effectivement récursive *)
                           (* Donc doit regarder si on f a été "typée" *)
                           let env = tenv_new_weak f env in
                           let s, c, sub = collect_constraint e1 env sub in
                           let sub = try 
                                       let c = add_constraint_on f s env c in
                                       unify sub c
                                     with Failure x -> unify sub c
                              in
                           if (is_generalizable expr) then generalize f sub env;
                           collect_constraint e2 env sub
	| Match (e, p)        -> failwith "TODO - Matchings"
  | IfElse (b, e1, e2)  -> let sb, cb, sub = collect_constraint b env sub in
                           let s, c1, sub = collect_constraint e1 env sub in
                           let t, c2, sub = collect_constraint e2 env sub in
                           s, (s, t) :: (t, s) :: (c2 @ c1 @ cb), sub
  | Fun (p, e)          -> let env = tenv_new_weak_pat p env in
                           let s, c, sub = collect_constraint e env sub in
                           let t = tenv_read_pat p env in
                           Fun_t (apply_sub sub t, apply_sub sub s), c, sub
  | App (e1, e2)        -> let s, c1, sub = collect_constraint e1 env sub in
                           let t, c2, sub = collect_constraint e2 env sub in
                           let i = new_weak () in
                           Weak_t i, (s, Fun_t(t, Weak_t i)) :: (c2 @ c1), sub 
  | Aff (e1, e2)        -> let s, c1, sub = collect_constraint e1 env sub in
                           let t, c2, sub = collect_constraint e2 env sub in
                           Unit_t, (s, Ref_t t) :: (Ref_t t, s) :: (c2 @ c1), sub
  | Alloc (e)           -> let s, c, sub = collect_constraint e env sub in
                           Ref_t s, c, sub
  | Raise e             -> let i = new_weak () in
                           let s, c, sub = collect_constraint e env sub in
                           type_of_id i, (Int_t, s) :: c, sub
  | Try (e, x, exn)     -> let s, c1, sub = collect_constraint e   env sub in
                           let env = tenv_new x Int_t env in
                           let t, c2, sub = collect_constraint exn env sub in
                           s, (s, t) :: (t, s) :: (c2 @ c1), sub



let type_checker expr = 
  tnext := 0;
  let t, c, sub = collect_constraint expr [] [] in
  let sub = unify sub c in
  let t' = apply_sub sub t in
  generalize_expr expr t'
