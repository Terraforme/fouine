open Types
open CheckerTypes
open TypePrinter
open Unification

let cl = ref [];;

let type_op = function
(* Selon l'opérateur op : 'a -> 'b -> 'c, renvoie ('a, 'b, 'c) *)
  | Plus | Minus | Times | Div | Mod  -> (Int_t, Int_t, Int_t)
  | Eq   | Neq   | Leq   | Lt  | Geq | Gt -> (Poly_t(new_poly ()), Poly_t(new_poly ()), Bool_t)
  | Or | And -> (Bool_t, Bool_t, Bool_t)

let add_constraint = fun c -> cl := c :: !cl
let rec add_constraint_on_pattern p t (env : tenv_f) = match (p, t) with
  | (Var_Pat x, _) -> add_constraint (tenv_read x env, t)
  | (Pair_Pat (p1, p2), Pair_t(t1, t2)) -> add_constraint_on_pattern p1 t1 env;
                                           add_constraint_on_pattern p2 t2 env
  | (_, _) -> failwith "Unmatching pattern"
;;
  
  
let rec collect_constraint expr (env : tenv_f) = match expr with
  | Var x               -> tenv_read x env
  | Bool _              -> Bool_t
  | Cst _               -> Int_t
  | Unit                -> Unit_t
  | Pair (e1, e2)       -> Pair_t (collect_constraint e1 env, collect_constraint e2 env)
  | Bang e              -> let t = collect_constraint e env in 
                           begin match t with
                           | Ref_t s -> s
                           | _ -> failwith "Dereferencing a non-reference"
                           end
                            
  | Neg e               -> add_constraint (collect_constraint e env, Bool_t); Bool_t
  | Bin (e1, op, e2)    -> let (s, t, r) = type_op op in
                           add_constraint (collect_constraint e1 env, s);
                           add_constraint (collect_constraint e2 env, t);
                           r
  | PrInt e             -> add_constraint (collect_constraint e env, Int_t); Int_t
  | Let (pat, e1, e2)   -> let env = tenv_new_poly_pat pat env in
                           let t = collect_constraint e1 env in
                           add_constraint_on_pattern pat t env;
                           collect_constraint e2 env
  | LetRec (f, e1, e2)  -> let env = tenv_new_poly f env in
                           let s  = collect_constraint e1 env 
                           and s0 = tenv_read f env in
                           add_constraint (s0, s);
                           collect_constraint e2 env
	| Match (e, p)        -> failwith "TODO - Matchings"
  | IfElse (b, e1, e2)  -> let r = collect_constraint b env in
                           add_constraint (r, Bool_t);
                           let s = collect_constraint e1 env 
                           and t = collect_constraint e2 env in
                           add_constraint (s, t); 
                           s
  | Fun (p, e)          -> let env = tenv_new_poly_pat p env in
                           let t = collect_constraint e env
                           and s = tenv_read_pat p env in
                           Fun_t(s, t)
  | App (e1, e2)        -> let s = collect_constraint e1 env in
                           let t = collect_constraint e2 env in
                           let i = new_poly () in
                           add_constraint (s, Fun_t(t, Poly_t i));
                           Poly_t i
  | Aff (e1, e2)        -> let s = collect_constraint e1 env in
                           let t = collect_constraint e2 env in
                           add_constraint (s, Ref_t t);
                           Unit_t
  | Alloc (e)           -> let s = collect_constraint e env in
                           Ref_t s
  | Raise e             -> failwith "raise todo"
  | Try (e, x, exn)     -> failwith "try todo"

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

let rec atomic_unify s t = match (s, t) with
(* Cas sans variables *)
  | (Int_t, Int_t)   -> []
  | (Bool_t, Bool_t) -> []
  | (Unit_t, Unit_t) -> []
  | (Poly_t x, Poly_t y) -> if x = y then [] else [x, t]
  | (Weak_t x, Weak_t y) -> if x = y then [] else [x, t]
  | (Ref_t s0, Ref_t t0) -> atomic_unify s0 t0
  | (Fun_t(s1, s2), Fun_t(t1, t2))   -> unify [(s1, t1); (s2, t2)]
  | (Pair_t(s1, s2), Pair_t(t1, t2)) -> unify [(s1, t1); (s2, t2)]
(* Cas avec variables - non terminal *)
  | (Poly_t x, Ref_t r) | (Ref_t r, Poly_t x) -> 
      if is_present x r then failwith "Impossible to unify : cycle"
      else [(x, r)]
  | (Poly_t x, Fun_t(r1, r2)) | (Fun_t(r1, r2), Poly_t x) -> 
      if is_present x r1 || is_present x r2 then failwith "Impossible to unify : cycle"
      else [(x, t)]
  | (Poly_t x, Pair_t(r1, r2)) | (Pair_t(r1, r2), Poly_t x) -> 
      if is_present x r1 || is_present x r2 then failwith "Impossible to unify : cycle"
      else [(x, t)]
(* Cas variables - terminal *)
  | (Poly_t x, _) -> [x, t]
  | (_, Poly_t x) -> [x, t]
  | _ -> failwith ("Impossible to unify : unmatching types " ^ (string_of_type s) ^ " and " ^ (string_of_type t))
  
and unify = function
  | [] -> []
  | (s, t) :: cl -> let sub' = unify cl in
                    let sub  = atomic_unify (apply_sub sub' s) (apply_sub sub' t) in
                    sub @ sub'

let rec print_constraints = function
  | [] -> ()
  | (s, t) :: cl -> print_type s; print_string " = "; print_type t; print_newline();
                    print_constraints cl
let rec print_substitution = function 
  | [] -> ()
  | (i, t) :: sub -> print_int i; print_string " is "; print_type t; print_newline();
                     print_substitution sub

let type_checker expr = 
  cl := [];
  tnext := 0;
  let t = collect_constraint expr [] in
  print_constraints !cl;
  let sub = unify (List.rev !cl) in
  (*print_string "Substitution is :\n";
  print_substitution sub;*)
  apply_sub sub t
