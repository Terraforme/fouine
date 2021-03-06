open Types
open Eval
open Printer
open Mem
open Continuations
open Transfo_ref
open Transfo_machine
open Secd
open TypePrinter
open TypeChecker

type exec_mod_f = Normal | Continuation | Parsing | Debug | References | CR | RC | Machine | Stackcode | Type | Complete;;

let debug_option = ref false and parsing_only_option = ref false and c_option = ref false and r_option = ref false
  and cr_option = ref false and rc_option = ref false
  and outcode_option = ref  false
  and machine_option = ref  false
  and stackcode_option = ref false
  and type_option = ref false
  and complete_option = ref false;;

let anon_fonction s = () in
let speclist = [("-debug", Arg.Set debug_option, "Switch to debug mode");
  ("-p", Arg.Set parsing_only_option, "Print only the result of parsing (do not use with -debug option)");
  ("-E", Arg.Set c_option, "Perform Continuation Transformation");
  ("-R", Arg.Set r_option, "Perform Transformation erasing the imperative aspects");
  ("-ER", Arg.Set cr_option, "Perform Continuation Transformation, then Transformation erasing the imperative aspects");
  ("-RE", Arg.Set rc_option, "Perform Transformation erasing the imperative aspects");
  ("-outcode", Arg.Set outcode_option, "Print the result of the Transformation (use with -E, -R, -ER or -RE)");
  ("-machine", Arg.Set machine_option, "Switch to Machine SECD mod");
  ("-stackcode", Arg.Set stackcode_option, "Print stackcode");
  ("-type", Arg.Set type_option, "Type - checker");
  ("-a", Arg.Set complete_option, "Print on format [type]-[execution value]")]
in let usage_msg = "\nThis is an ocaml interpreter for the fouine language.\n";
in Arg.parse speclist anon_fonction usage_msg;;


let lexbuf = Lexing.from_channel (Pervasives.open_in (Sys.argv.(1)));;
let lexbuf2 = Lexing.from_channel (Pervasives.open_in ("annexe/mem_fouine_sans_commentaires"));;

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let parse () = Parser.main Lexer.token lexbuf;;

let rec e_concat e1 e2 tag =
(* concatène e1 sous la forme Let(......) avec e2
selon le tag en paramètre *)
  match e1 with
  | Let(truc,e1_0, Var smg) -> if smg = tag then Let(truc, e1_0, e2)
                               else failwith "Cannot merge"
  | LetRec(truc,e1_0, Var smg) -> if smg = tag then LetRec(truc, e1_0, e2)
                                  else failwith "Cannot merge"
  | Let(truc,e1_0, e1)      -> Let(truc,e1_0, (e_concat e1 e2 tag))
  | LetRec(truc,e1_0, e1)      -> LetRec(truc,e1_0, (e_concat e1 e2 tag))
  | _ -> failwith "Cannot merge"


(* la fonction que l'on lance ci-dessous *)
let calc exec_mod =
  let expr = parse () in
  match exec_mod with
  | Normal -> let _ = type_checker expr in let _ = eval expr [] id [] in ()
  | Complete -> let _ = eval expr [] id [] in ()
  | Continuation -> if !outcode_option
    then let cexpr = ccont expr in pretty_print_expr cexpr
    else let _ = eval (ctransform expr) [] id [] in ()
  | References ->
        let expr_mem = Parser.main Lexer.token lexbuf2 in
        let expr_finale = App(e_concat expr_mem (transforme_ref expr) "this_is_a_tag", Unit) in
        if !outcode_option then
            pretty_print_expr expr_finale
        else let _ = eval expr_finale [] id [] in ()
  | CR ->
    begin
      let expr_mem = Parser.main Lexer.token lexbuf2 in
      let expr_finale = App(e_concat expr_mem (transforme_ref (ctransform expr)) "this_is_a_tag", Unit) in
      if !outcode_option then pretty_print_expr expr_finale
      else let _ = eval expr_finale [] id [] in ()
    end
  | RC -> let expr_mem = Parser.main Lexer.token lexbuf2 in
      let expr_inter = App(e_concat expr_mem (transforme_ref expr) "this_is_a_tag", Unit) in
      let expr_finale = ctransform expr_inter in
      if !outcode_option then pretty_print_expr expr_finale
      else let _ = eval expr_finale [] id [] in ()
  | Parsing ->
      print_string "Raw parsing : \n";
      print_expr expr;
      print_string "\n\nBeautiful parsing : \n";
      pretty_print_expr expr;
      print_newline ()
  | Machine ->
      let code = langage_SECD expr in
      if !stackcode_option then
      begin
        print_SECD code;
        print_newline()
      end
      else let _ = secd code in ()
  | Stackcode -> print_SECD (langage_SECD expr)
  | Debug ->
    begin
      print_string "Raw parsing : \n";
      print_expr expr;
      print_string "\n\nBeautiful parsing : \n";
      pretty_print_expr expr;

      print_string "\nTransformation impérative : \n";
      let expr_mem = Parser.main Lexer.token lexbuf2 in
      let expr_trans = transforme_ref expr in
      let expr_finale = App(e_concat expr_mem (expr_trans) "this_is_a_tag", Unit) in
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
  | Type -> 
    begin
      print_expr expr;
      pretty_print_expr expr;
      try print_type (type_checker expr); print_newline ()
      with Failure x -> print_string ("Type checker : " ^ x); print_newline ()
    end
  ;
  flush stdout
;;

let exec_mod = if !debug_option then Debug else if !parsing_only_option then Parsing
else if !complete_option then Complete else if !type_option then Type 
else if !c_option then Continuation else if !r_option then References
else if !cr_option then CR else if !rc_option then RC
else if !machine_option then Machine else if !stackcode_option then Stackcode
else Normal in
calc exec_mod;;
