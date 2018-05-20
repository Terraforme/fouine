open CheckerTypes
(* Affichage *)

let rec string_of_type = function
  | Int_t         -> "int"
  | Bool_t        -> "bool"
  | Unit_t        -> "unit"
  | Poly_t x      -> "'" ^ (Printf.sprintf "%c" (Char.chr (x + 97)))
  | Weak_t x      -> "'_" ^ (Printf.sprintf "%c" (Char.chr (x + 97)))
  | Ref_t  t      -> (string_of_type t) ^ " ref"
  | Fun_t  (s, t) -> "(" ^ (string_of_type s) ^ " -> " ^ (string_of_type t) ^ ")"
  | Pair_t (s, t) -> "(" ^ (string_of_type s) ^ " * " ^ (string_of_type t) ^ ")"

let print_type t = print_string (string_of_type t)

let print_index () = 
  for i = 0 to !tnext - 1 do
    print_string ((string_of_int i) ^ ": " ^ (string_of_type tindex.(i)) ^ "\n")
  done;
