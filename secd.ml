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
    EPSILON -> print_string "EPSILON\n"
  | CONST a -> print_string ("CONST " ^ (string_of_int a)  ^ "\n")
  | BOOL b  -> print_string ("BOOL "  ^ (string_of_bool b) ^ "\n")
  | UNIT -> print_string "UNIT\n"
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
  | APPLY -> print_string "APPLY\n"
  | RETURN -> print_string "RETURN\n"


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

  | ADD  -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := ipush (v1 + v2) !stack; 
            incr pc
  | SUB  -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := ipush (v1 - v2) !stack; 
            incr pc
  | MULT -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := ipush (v1 * v2) !stack; 
            incr pc
  | DIV  -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := ipush (v1 / v2) !stack; 
            incr pc
  | MOD  -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := ipush (v1 mod v2) !stack; 
            incr pc

  | NOT  -> let b = btop !stack in stack := pop !stack;
            stack := bpush (not b) !stack ; incr pc

  | EQ   -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := bpush (v1 = v2) !stack ; incr pc
  | LT   -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := bpush (v1 < v2) !stack ; incr pc
  | LE   -> let v1 = itop !stack in stack := pop !stack;
            let v2 = itop !stack in stack := pop !stack;
            stack := bpush (v1 <= v2) !stack ; incr pc

  | PRINT  -> let v = itop !stack in prInt v ; incr pc
  | LET x  -> let v = top !stack  in stack := pop !stack;
              env := menv_aff x v !env ; incr pc
  | ENDLET -> env := pop !env ; incr pc
  | JUMP   a -> pc := a
  | JUMPIF a -> let b = btop !stack in stack := pop !stack ;
                if b then pc := a else incr pc

  | CLOSURE a -> stack := cpush (!pc + 1) !env !stack ; pc := a
  | APPLY  -> let (addr, env') = ctop !stack in stack := pop !stack ;
              let v = top !stack in let _ = stack := pop !stack in
              stack := apush (!pc + 1) !stack ;
              stack := epush !env !stack ;
              env := env' ; stack := push v !stack ; pc := addr
  | RETURN -> let retv = top !stack in stack := pop !stack ;
              let env' = etop !stack in stack := pop !stack ;
              let pc'  = atop !stack in stack := pop !stack ;
              stack := push retv !stack ;
              env   := env' ; pc := pc'
  | UNIT -> failwith "TODO: Victor"


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

