{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations et les retours à la ligne *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | eof             { EOF }

	(* PONCTUATION *)
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '='             { EQUAL }
  | ';'             { SEMICOL }
  | ','             { COMA }
  | ";;"            { EOI } (*end of instruction*)
  | ':'             { COLON }
  | ":="            { AFFECTATION }
  | '.'             { DOT }
  | '!'             { BANG }
	
	(* OPÉRATEURS ARITHMÉTIQUES *)
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '/'             { DIV }
  | '*'             { TIMES }
  | "mod"           { MOD }

  | '_'             { ANON }

	(* OPÉRATEURS DE COMPARAISON *)
  | '>'             { GREATER }
  | '<'             { LOWER }
  | ">="            { GE }
  | "<="            { LE }
  | "<>"            { NE }

	(* OPÉRATEURS BOOLÉENS *)
  | "&&"            { AND }
  | "||"            { OR }
  | "not"           { NOT }

	(* MOTS CLÉS *)
  | "begin" 		{ BEGIN }
  | "end" 			{ END }

  | "let" 			{ LET }
  | "let rec" 	{ REC }
  | "in"			  { IN }

  | "fun" 			{ FUN }

	(* Structures de contrôle *)
  | "if" 				{ IF }
  | "then" 			{ THEN }
  | "else" 			{ ELSE }

  | "true" 			{ TRUE }
  | "false" 		{ FALSE }

  | "prInt" 		{ PRINT }
  | "->" 				{ FLECHE }

  | "ref" 			{ REF }

  | 'E' 				{ E }
  | "raise" 		{ RAISE }
  | "try" 			{ TRY }
  | "with" 			{ WITH }

	(* CONSTANTES *)
  | ('-'|'+')?['0'-'9']+'.'['0'-'9']* as s { NBR (float_of_string s) } (* FIXME : inutile ? *)
  | ['0'-'9']+ as s { INT (int_of_string s) }

	(* VARIABLES : On n'autorise pas aux variables de commencer par une majuscule *)
  | (['a'-'z']|'_')(['A'-'Z']|['a'-'z']|'_'|['0'-'9'])* as s { VAR s }
  (*| (['A'-'Z']|['a'-'z'])*+['0'-'9']* as s { VAR s }*)

	(* CONSTRUCTEURS : comme les variables sauf qu'on a une majuscule au début *)
	| ['A'-'Z'](['A'-'Z']|['a'-'z']|'_'|['0'-'9'])* as s { CONS s }
