open Types
open Eval
open Printer
open Mem
open Continuations
open Transfo_ref
open Connection

(* Pour la commodité : *)

type exec_mod_f = Normal | Continuation | Parsing | Debug | References | CR | RC;;

let debug_option = ref false and parsing_only_option = ref false and c_option = ref false and r_option = ref false
  and cr_option = ref false and rc_option = ref false
  and outcode_option = ref  false;;

let anon_fonction s = () in (* print_endline ?*)
let speclist = [("-debug", Arg.Set debug_option, "Switch to debug mode");
  ("-p", Arg.Set parsing_only_option, "Print only the result of parsing (do not use with -debug option)");
  ("-E", Arg.Set c_option, "Perform Continuation Transformation");
  ("-R", Arg.Set r_option, "Perform Transformation erasing the imperative aspects");
  ("-ER", Arg.Set cr_option, "Perform Continuation Transformation, then Transformation erasing the imperative aspects");
  ("-RE", Arg.Set rc_option, "Perform Transformation erasing the imperative aspects");
  ("-outcode", Arg.Set outcode_option, "Print the result of the Transformation (use with -E, -R, -ER or -RE)")]
in let usage_msg = "\nThis is an ocaml interpreter for the fouine language.\n";
in Arg.parse speclist anon_fonction usage_msg;;


let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)));;
let lexbuf2 = Lexing.from_channel (Pervasives.open_in ("mem_fouine_sans_commentaires"));;

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf;;

(* la fonction que l'on lance ci-dessous *)
let calc exec_mod =
  let expr = parse () in
  match exec_mod with
  | Normal -> let _ = eval expr [] id [] in ()
  | Continuation -> if !outcode_option
    then let cexpr = ccont expr in pretty_print_expr cexpr
    else let _ = eval (ctransform expr) [] id [] in ()
  | References -> if !outcode_option then
      let expr_trans = transforme_ref expr in
      pretty_print_expr expr_trans;
    else
      begin
        let expr_mem = Parser.main Lexer.token lexbuf2 in
        (*let expr_finale = App(Let(Var_Pat "_", expr_mem, transforme_ref expr), Unit) in*)
        let expr_finale = connecte expr_mem (App(transforme_ref expr, Unit)) in

        pretty_print_expr (expr_finale);
        (*pour le déboggage -> a enlever apres:*)
        (*pretty_print_expr expr_finale;*)
        (*print_expr expr_mem;*)

        let _ = eval expr_finale [] id [] in ()
      end

 (* FIXME: CR et RC ne fonctionnent pas, il doit y avoir un problème de compatibilité de types.
 Notes: Le code transformé par élimination des aspects impératifs renvoie un couple (value, s) *)
  | CR -> if !outcode_option then
      let expr_trans = transforme_ref (ccont expr) in
      pretty_print_expr expr_trans
    else
    begin
      let expr_mem = Parser.main Lexer.token lexbuf2 in
      let expr_finale = App(Let(Var_Pat "_", expr_mem, transforme_ref (ctransform expr)), Unit) in
      let _ = eval expr_finale [] id [] in ()
    end
    | RC -> if !outcode_option then
        let expr_trans = ccont (transforme_ref expr) in
        pretty_print_expr expr_trans
      else
      begin
        let expr_mem = Parser.main Lexer.token lexbuf2 in
        let expr_inter = App(Let(Var_Pat "_", expr_mem, transforme_ref expr), Unit) in
        let expr_finale = ctransform expr_inter in
        let _ = eval expr_finale [] id [] in ()
      end


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

      print_string "\nTransformation impérative : \n";
      let expr_mem = Parser.main Lexer.token lexbuf2 in
      let expr_trans = transforme_ref expr in
      let expr_finale = App(Let(Var_Pat "_", expr_mem, expr_trans), Unit) in
      pretty_print_expr expr_trans;

      print_string "\nContinuation : \n";
      let cexpr = ccont expr in
      pretty_print_expr cexpr;
      print_string "\n\nExécution (Normale) : \n";
      let value = eval expr [] id [] in
      print_string "\n\nExécution (Impérative) : \n";
      let value2 = eval expr_finale [] id [] in
      print_string "\nExécution (continuation) : \n";
      let _ = eval (ctransform expr) [] id [] in

			print_string "\nvalue(continuation):\t";
			pretty_value 0 value;
      print_string "\nvalue(Impérative):\t";
      pretty_value 0 value2;
			print_newline ()
    end
  ;
  flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)
;;

let exec_mod = if !debug_option then Debug else if !parsing_only_option then Parsing
else if !c_option then Continuation else if !r_option then References
else if !cr_option then CR else if !rc_option then RC else Normal in
calc exec_mod;;
