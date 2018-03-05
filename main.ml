open Types
open Eval

type exec_mod_f = Normal | Parsing | Debug;;

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)))

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let debug_option = ref false and parsing_only_option = ref false;;
let speclist = [("-debug", Arg.Set debug_option, "Switch to debug mode");
  ("-p", Arg.Set parsing_only_option, "Print only the result of parsing (do not use with -debug option)")]
in let usage_msg = "\nThis is an ocaml interpreter for the fouine language.\n";
in Arg.parse speclist print_endline usage_msg;;
let parse () = Parser.main Lexer.token lexbuf;;

(* la fonction que l'on lance ci-dessous *)
let calc exec_mod =
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

let exec_mod = if !debug_option then Debug else if !parsing_only_option then Parsing else Normal in
calc exec_mod;;
