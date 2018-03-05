open Types
open Eval


type exec_mod_f = Normal | Parsing | Debug

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)))

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () exec_mod =
  let expr = parse () in
  match exec_mod with
  | Normal -> let _ = eval expr [] in ()
  | Parsing ->
    begin
      print_string "Raw parsing : \n";
      print_expr expr;
      print_string "\n\nBeautful parsing : \n";
      pretty_print_expr expr;
      print_newline ()
    end
  | Debug ->
    begin
      print_string "Raw parsing : \n";
      print_expr expr;
      print_string "\n\nBeautful parsing : \n";
      pretty_print_expr expr;
      print_string "\n\nExécution : \n";
      let a = eval expr [] in
      print_string "value:\t";
      print_int a
    end
  ;
  flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)
;;

let _ = calc () Debug
