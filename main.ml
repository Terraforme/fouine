open Types
open Eval

let rec compile = function
  | [] -> ()
  | expr :: expr_list ->
    begin
      affiche_expr expr;
      print_string ", value computed : ";
      print_int (compute expr);
      print_newline();
      compile expr_list
    end

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
	compile result; flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)
;;

let _ = calc()
