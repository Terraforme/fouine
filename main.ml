open Types
open Eval
open Exec

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)))

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  (*try*)
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	let a = exec result [] in
  print_int a;
  print_newline();
  flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)
;;

let _ = calc()
