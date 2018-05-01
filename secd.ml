open Types
open Env
open Eval

(* On définit ici la machine SECD :
on passe la pile et l'environnement en paramètres .
L'environnement reprend la définition usuelle définit dans `env.ml`.
Pour la pile ; on peut y stocker des valeurs, des environnements, et une clôture. *)


(* e comme 'element' *)
type estack_f = VAL of val_f | ENV of env_f | CLO of var_f * asm_f | ASM of asm_f
type stack_f  = estack_f list
;;

(* Quelques alias pour la clarté *)

let top  stack = List.hd stack
let pop  stack = List.tl stack
let push x stack = x :: stack
;;

(* Des top plus précis, pour récupérer un type particulier *)
let vtop stack = match (List.hd stack) with 
  | VAL v -> v
  | _     -> failwith "vtop : Poping Non-value"
let etop stack = match (List.hd stack) with
  | ENV e -> e
  | _     -> failwith "etop : Poping Non-env"
let ctop stack = match (List.hd stack) with
  | CLO (x, c') -> (x, c')
  | _           -> failwith "ctop : Poping Non-closure"
let atop stack = match (List.hd stack) with 
  | ASM c -> c
  | _     -> failwith "atom : Poping Non-asm"
;;
  
(* Des push plus précis, pour pusher des types particuliers *)
let vpush x stack = (VAL x) :: stack
let epush e stack = (ENV e) :: stack
let cpush x c' stack = (CLO (x, c')) :: stack 
let apush c stack = (ASM c) :: stack
;;

(* Fonction prINt *)
let prInt = function 
  | Int a -> print_int a; print_newline (); a;
  | _ -> failwith "prInt : non-Int value"
;;

(* La machine en elle-même *) (*FIXME*)
let rec secdm code s e = match code with
(* val secdm = asm_f -> val_f list -> env_f -> val_f list 
La machine renvoie l'état de sa pile *)
  | EPSILON -> s
  | SEQ (instr, c) -> 
    begin match instr with
      | CONST a  -> secdm c (vpush (Int a) s) e
      | ACCESS x -> secdm c (vpush (env_read x e) s) e
      | ADD      -> 
          let v1 = vtop s  in let s'  = pop s  in
          let v2 = vtop s' in let s'' = pop s' in
          secdm c (vpush (aeval Plus  v1 v2) s'') e 
      | SUB      ->
          let v1 = vtop s  in let s'  = pop s  in
          let v2 = vtop s' in let s'' = pop s' in
          secdm c (vpush (aeval Minus v1 v2) s'') e
      | MULT     ->
          let v1 = vtop s  in let s'  = pop s  in
          let v2 = vtop s' in let s'' = pop s' in
          secdm c (vpush (aeval Times v1 v2) s'') e
      | DIV      ->
          let v1 = vtop s  in let s'  = pop s  in
          let v2 = vtop s' in let s'' = pop s' in
          secdm c (vpush (aeval Div   v1 v2) s'') e
      | MOD      ->
          let v1 = vtop s  in let s'  = pop s  in
          let v2 = vtop s' in let s'' = pop s' in
          secdm c (vpush (aeval Mod   v1 v2) s'') e
      | PRINT    -> let _ = prInt (vtop s) in secdm c s e
      | LET (x)  -> 
          let v = vtop s   in let s'  = pop s in
          secdm c s' (env_aff x v e)
      | ENDLET   -> secdm c s (pop e)
      
      | CLOSURE (x, c') -> 
          let s'  = epush e s   in
          let s'' = cpush x c' s' in
          secdm c s'' e
      | APPLY -> 
          let (x, c') = ctop s in let s = pop s    in
          let e'  = etop s in let s   = pop s      in
          let v   = vtop s in let s   = pop s      in
          let s   = epush e s in let s = apush c s in
          let e'' = env_aff x v e' in
          secdm c' s e''
      | RETURN -> 
          let v  = vtop s  in let s0  = pop s  in
          let c' = atop s0 in let s1 = pop s0 in
          let e' = etop s1 in let s2 = pop s1 in 
          let s' = vpush v s2 in 
          secdm c' s' e'
    end
;;
