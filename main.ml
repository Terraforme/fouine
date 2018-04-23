open Types
open Eval
open Printer
open Mem
open Continuations
open Transfo_ref

(* Pour la commodité : *)

type exec_mod_f = Normal | Continuation | Parsing | Debug;;

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)));;

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let debug_option = ref false and parsing_only_option = ref false and c_option = ref false;;

let anon_fonction s = () in (* print_endline ?*)
let speclist = [("-debug", Arg.Set debug_option, "Switch to debug mode");
  ("-p", Arg.Set parsing_only_option, "Print only the result of parsing (do not use with -debug option)"); ("-C", Arg.Set c_option, "Perform Continuation Transformation") ]
in let usage_msg = "\nThis is an ocaml interpreter for the fouine language.\n";
in Arg.parse speclist anon_fonction usage_msg;;

let parse () = Parser.main Lexer.token lexbuf;;

(* la fonction que l'on lance ci-dessous *)
let calc exec_mod =
  let expr = parse () in
  match exec_mod with
  | Normal -> let _ = eval expr [] id [] in ()
  | Continuation -> let _ = eval (ctransform expr) [] id [] in ()
  | Parsing ->
  (* On a rajouté une option de parsing *)
    begin
      print_string "Raw parsing : \n";
      print_expr expr;
      (*print_string "\n\nBeautiful parsing : \n";
      pretty_print_expr expr;*)
      print_newline ()
    end
  | Debug ->
    begin
      print_string "Raw parsing : \n";
      print_expr expr;
      print_string "\n\nBeautiful parsing : \n";
      pretty_print_expr expr;

      print_string "\nContinuation : \n";
      let cexpr = ccont expr in
      pretty_print_expr cexpr;
      print_string "\n\nExécution (Normale) : \n";
      let value = eval expr [] id [] in
      print_string "\nExécution (continuation) : \n";
      let _ = eval (ctransform expr) [] id [] in

			print_string "\nvalue:\t";
			pretty_value 0 value;
			print_newline ()
    end
  ;
  flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)
;;

let exec_mod = if !debug_option then Debug else if !parsing_only_option then Parsing else if !c_option then Continuation else Normal in
calc exec_mod;;
