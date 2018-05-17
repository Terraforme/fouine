open Types

type type_f = 
  | Int_t 
  | Bool_t 
  | Var_t   of int
  | Ref_t   of type_f
  | Pair_t  of type_f * type_f
  | Fun_t   of type_f * type_f
;;

let rec string_of_type = function
  | Int_t         -> "int"
  | Bool_t        -> "bool"
  | Var_t x       -> "'" ^ (string_of_int x)
  | Ref_t t       -> "ref " ^ (string_of_type t)
  | Fun_t  (s, t) -> (string_of_type s) ^ " -> " (string_of_type t)
  | Pair_t (s, t) -> "(" ^ (string_of_type s) ^ " * " ^ (string_of_type t) ^ ")"

let print_type t = print_string (string_of_type t)
  
  
  
