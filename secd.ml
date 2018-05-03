open Types
open Env
open Eval

(* On définit ici la machine SECD :
on passe la pile et l'environnement en paramètres .
L'environnement reprend la définition usuelle définit dans `env.ml`.
Pour la pile ; on peut y stocker des valeurs, des environnements, et une clôture. *)


(* e comme 'element' *)
type mval_f =  
    INT of int 
  | BOOL of bool
  | ENV of menv_f 
  | CLO of var_f  * asm_f * menv_f 
  | ASM of asm_f
and  menv_f = (var_f * mval_f) list

type stack_f = mval_f list
;;


let rec menv_read x = function 
    [] -> failwith ("menv_read : " ^ x ^ " not found")
  | (y, _) :: env when x <> y -> menv_read x env
  | (x, v) :: env             -> v
let rec menv_aff x v env = match x with
  | "_" -> env
  | x   -> (x, v) :: env

(* Quelques alias pour la clarté *)

let top  stack = List.hd stack
let pop  stack = List.tl stack
let push x stack = x :: stack
;;

(* Des top plus précis, pour récupérer un type particulier *)
let itop stack = match (List.hd stack) with 
  | INT v -> v
  | _     -> failwith "itop : Poping Non-int"
let btop stack = match (List.hd stack) with
  | BOOL b -> b
  | _      -> failwith "btop : Poping Non-boolean"
let etop stack = match (List.hd stack) with
  | ENV e -> e
  | _     -> failwith "etop : Poping Non-env"
let ctop stack = match (List.hd stack) with
  | CLO (x, c', e') -> (x, c', e')
  | _           -> failwith "ctop : Poping Non-closure"
let atop stack = match (List.hd stack) with 
  | ASM c -> c
  | _     -> failwith "atom : Poping Non-asm"
;;
  
(* Des push plus précis, pour pusher des types particuliers *)
let ipush x stack = (INT x)  :: stack
let bpush b stack = (BOOL b) :: stack
let epush e stack = (ENV e)  :: stack
let cpush x c' e stack = (CLO (x, c', e)) :: stack 
let apush c stack = (ASM c) :: stack
;;

(* Fonction prINt *)
let prInt = function 
  | INT a -> print_int a; print_newline (); a;
  | _ -> failwith "prInt : non-Int value"
;;



let rec print_SECD code = match code with
    [] -> ()
  | instr :: code -> 
    begin 
    (match instr with
      | CONST a -> print_string ("CONST(" ^ (string_of_int a) ^ ")\n")
      | BOOL  b -> print_string ("BOOL(" ^ (string_of_bool b) ^ ")\n")
      | ACCESS x -> print_string ("ACCESS(" ^ x ^ ")\n")
      | ADD -> print_string "ADD\n"
      | SUB -> print_string "SUB\n"
      | MULT -> print_string "MULT\n"
      | DIV -> print_string "DIV\n"
      | MOD -> print_string "MOD\n"
      | NOT -> print_string "NOT\n"
      | EQ -> print_string "EQ\n"
      | LE -> print_string "LE\n"
      | LT -> print_string "LT\n"
      | PRINT -> print_string "PRINT\n"
      | LET x -> print_string ("LET(" ^ x ^ ")\n")
      | ENDLET -> print_string "ENDLET\n"
      | CLOSURE (x, c') ->
        begin
          print_string ("CLOSURE(" ^ x ^ ")\n");
          print_SECD c';
          print_string "ENDCLOSURE\n"
        end
      | SWITCH (t, f) -> 
        begin
          print_string "SWITCH TRUE\n"; print_SECD t;
          print_string "SWITCH FALSE\n"; print_SECD f;
          print_string "ENDSWITCH\n"
        end
      | APPLY -> print_string "APPLY\n"
      | RETURN -> print_string "RETURN\n");
      
      print_SECD code
    end

(* La machine en elle-même *) (*FIXME*)
let rec secdm code s e = match code with
(* FIXME : val secdm = asm_f -> val_f list -> env_f -> val_f list 
La machine renvoie l'état de sa pile *)
  | [] -> s
  | instr :: c -> 
    begin match instr with
      | CONST a  -> secdm c (ipush a s) e
      | BOOL  b  -> secdm c (bpush b s) e
      | ACCESS x -> secdm c (push (menv_read x e) s) e
      | ADD      -> 
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (ipush (v1 + v2) s'') e 
      | SUB      ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (ipush (v1 - v2) s'') e
      | MULT     ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (ipush (v1 * v2) s'') e
      | DIV      ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (ipush (v1 / v2) s'') e
      | MOD      ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (ipush (v1 mod v2) s'') e
      | EQ       ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (bpush (v1 = v2) s'') e
      | LE       ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (bpush (v1 <= v2) s'') e
      | LT       ->
          let v1 = itop s  in let s'  = pop s  in
          let v2 = itop s' in let s'' = pop s' in
          secdm c (bpush (v1 < v2) s'') e
      | NOT      ->
          let b =  btop s in let s' = pop s in
          secdm c (bpush (not b) s') e
      | PRINT    -> let _ = prInt (top s) in secdm c s e
      | LET (x)  -> 
          let v = top s   in let s'  = pop s in
          secdm c s' (menv_aff x v e)
      | ENDLET   -> secdm c s (pop e)
      | SWITCH (t, f) ->
          let b = btop s in let s' = pop s in (*
          let _ = print_string "Going on : \n" in
          let _ = print_SECD (if b then t else f) in 
          let _ = print_string "then : \n" in
          let _ = print_SECD c in *)
          secdm (if b then t else f) (apush c (epush e s')) e
      | CLOSURE (x, c') -> 
          secdm c (cpush x c' e s) e
      | APPLY -> 
          let (x, c', e') = ctop s in let s0 = pop s in
          let v = top s0 in let s' = pop s0 in
          secdm c' (apush c (epush e s')) (menv_aff x v e')
      | RETURN -> 
          let v = top s in let s = pop s in
          let c' = atop s in let s = pop s in
          let e' = etop s in let s = pop s in
          secdm c' (push v s) e'
    end
;;


