open CheckerTypes
open TypePrinter

let uerr s t = failwith ("Cannot unify '" ^ (string_of_type s) ^ "' with '" ^ (string_of_type t) ^ "'")

let upd_id i s = 
  tindex.(i) <- s; 
  print_string ("Updated (" ^ (string_of_int i) ^ ") : " ^ (string_of_type tindex.(i)) ^ "\n")
;;



(*
let rec unify s t = 
  print_string ("Unifying " ^ (string_of_type s) ^ " with " ^ (string_of_type t) ^ "\n");
  match s with
(* unifie le type s et t, avec s <= t 
 * s : c'est le type qu'on a 
 * t : c'est le type qu'on attend *)
  | Poly_t x -> t
  | Weak_t x -> upd_id x t; t
  | Unit_t   -> begin match t with 
                | Unit_t -> Unit_t 
                | Poly_t x -> Unit_t
                | Weak_t x -> upd_id x s; s
                | _ -> uerr s t 
                end
  | Int_t    -> begin match t with 
                | Int_t -> Int_t 
                | Poly_t x -> Int_t
                | Weak_t x -> upd_id x s; s
                | _ -> uerr s t
                end
  | Bool_t   -> begin match t with 
                | Bool_t -> Bool_t 
                | Poly_t x -> Bool_t
                | Weak_t x -> tindex.(x) <- s; s
                | _ -> uerr s t 
                end
  | Pair_t (s1, s2) -> begin match t with
                        | Pair_t (t1, t2) -> Pair_t (unify s1 t1, unify s2 t2)
                        | Poly_t x -> s
                        | Weak_t x -> upd_id x s; s
                        | _ -> uerr s t
                       end
  | Fun_t (s1, s2) -> begin match t with 
                        | Fun_t (t1, t2) -> Fun_t (unify s1 t1, unify s2 t2)
                        | Poly_t x -> s
                        | Weak_t x -> upd_id x s; s
                        | _ -> uerr s t
                        end
  | _ -> failwith "Unsupported type"
;;*)
