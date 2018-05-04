open Types
open Env
open Eval

(* On définit ici la machine SECD :
on passe la pile et l'environnement en paramètres .
L'environnement reprend la définition usuelle définit dans `env.ml`.
Pour la pile ; on peut y stocker des valeurs, des environnements, et une clôture. *)


let pc = ref 0;

(* e comme 'element' *)
type mval_f =  
    INT of int 
  | BOOL of bool
  | ENV  of menv_f 
  | CLO  of int * menv_f 
  | ADDR of int
and  menv_f = (var_f * mval_f) list

type stack_f = mval_f list
;;


let rec menv_read x = function 
    [] -> failwith ("menv_read : " ^ x ^ " not found")
  | (y, _) :: env when x <> y -> menv_read x env
  | (x, v) :: env             -> v
let rec menv_aff x v env = (x, v) :: env

(* Quelques alias pour la clarté *)

let top  stack = List.hd stack
let pop  stack = match stack with [] -> failwith ("tail : at ligne " ^ (string_of_int !pc))
                                  | _ :: t -> t
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
  | CLO (c', e') -> (c', e')
  | _           -> failwith "ctop : Poping Non-closure"
let atop stack = match (List.hd stack) with 
  | ADDR c -> c
  | _      -> failwith "atom : Poping Non-asm"
;;
  
(* Des push plus précis, pour pusher des types particuliers *)
let ipush x stack = (INT x)  :: stack
let bpush b stack = (BOOL b) :: stack
let epush e stack = (ENV e)  :: stack
let cpush c' e stack = (CLO (c', e)) :: stack 
let apush c stack = (ADDR c) :: stack
;;

(* Fonction prINt *)
let prInt a = print_int a; print_newline ()



let print_instr = function 
    EPSILON -> print_string "EPSILON"
  | CONST a -> print_string ("CONST " ^ (string_of_int a)  ^ "\n")
  | BOOL b  -> print_string ("BOOL "  ^ (string_of_bool b) ^ "\n")
  | ACCESS x -> print_string ("ACCESS " ^ x ^ "\n")
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
  | LET x -> print_string ("LET " ^ x ^ "\n")
  | ENDLET -> print_string "ENDLET\n"
  | JUMP a    -> print_string ("JUMP " ^ (string_of_int a) ^ "\n")
  | JUMPIF a  -> print_string ("JUMPIF " ^ (string_of_int a) ^ "\n")
  | CLOSURE a -> print_string ("CLOSURE " ^ (string_of_int a) ^ "\n")
  | APPLY -> print_string "APPLY"
  | RETURN -> print_string "RETURN"
 

let print_SECD code = 
  pc := 0;
  let n  = Array.length code in
  while !pc < n && code.(!pc) <> EPSILON do
    print_string ((string_of_int !pc) ^ ": ");
    print_instr code.(!pc);
    incr pc
  done;
  ();;


let treat_instr instr stack env pc = match instr with 
(* val treat_instr :  instr_f -> stack_f ref -> menv_f ref -> int ref -> unit *)
    EPSILON -> failwith "EPSILON : end of program unexpected"
  | CONST  a -> let _ = stack := ipush a !stack in incr pc
  | BOOL   b -> let _ = stack := bpush b !stack in incr pc
  | ACCESS x -> let _ = stack := push (menv_read x !env) !stack in incr pc
  
  | ADD  -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := ipush (v1 + v2) !stack in incr pc
  | SUB  -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := ipush (v1 - v2) !stack in incr pc
  | MULT -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := ipush (v1 * v2) !stack in incr pc
  | DIV  -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := ipush (v1 / v2) !stack in incr pc
  | MOD  -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := ipush (v1 mod v2) !stack  in incr pc
            
  | NOT  -> let b = btop !stack in let _ = stack := pop !stack in
            let _ = stack := bpush (not b) !stack in incr pc
  
  | EQ   -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := bpush (v1 = v2) !stack in incr pc
  | LT   -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := bpush (v1 < v2) !stack in incr pc
  | LE   -> let v1 = itop !stack in let _ = stack := pop !stack in
            let v2 = itop !stack in let _ = stack := pop !stack in
            let _  = stack := bpush (v1 <= v2) !stack in incr pc
  
  | PRINT  -> let v = itop !stack in let _ = prInt v in incr pc
  | LET x  -> let v = top !stack  in let _ = stack := pop !stack in
              let _ = env := menv_aff x v !env in incr pc
  | ENDLET -> let _ = env := pop !env in incr pc
  | JUMP   a -> pc := a
  | JUMPIF a -> let b = btop !stack in let _ = stack := pop !stack in
                if b then pc := a else incr pc
  
  | CLOSURE a -> let _ = stack := cpush !pc !env !stack in incr pc
  | APPLY  -> let (addr, env') = ctop !stack in let _ = stack := pop !stack in
              let _ = stack := apush (!pc + 1) !stack in 
              let _ = stack := epush !env !stack in
              let _ = env := env' in pc := addr  
  | RETURN -> let retv = top !stack in let _ = stack := pop !stack in
              let env' = etop !stack in let _ = stack := pop !stack in
              let pc'  = atop !stack in let _ = stack := pop !stack in
              let _ = stack := push retv !stack in 
              let _ = env   := env' in pc := pc' 
  

let secd code = 
(* val secd : asm_f -> stack_f
La machine secd : prend du code en argument et l'évalue 
La stack et l'environnement sont des références qui seront modifiées
au cours de l'exécution du code *)
  let n = Array.length code in
  let stack = ref [] in
  let env   = ref [] in
  let pc    = ref 0  in
  
  while !pc < n && code.(!pc) <> EPSILON do
    (*print_string ("At ligne " ^ (string_of_int !pc) ^ ":");
    print_instr code.(!pc);*)
    treat_instr code.(!pc) stack env pc;
    (*print_string ("\tnew line : " ^(string_of_int !pc) ^ "\n")*)
  done;
  !stack;;

















(*
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

*)
