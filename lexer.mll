{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
(* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
exception Eof;;
}

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t' '\n']     { token lexbuf }    (* on saute les blancs et les tabulations *)
 	     	   	           (* token: appel récursif *)
                                   (* lexbuf: argument implicite
                                      associé au tampon où sont
                                      lus les caractères *)
  | eof             { EOF }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '='             { EQUAL }
  | ';'             { SEMICOL }
  | ':'             { COLON }
  | '.'             { DOT }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '/'             { DIV }
  | '*'             { TIMES }
  | "mod"           { MOD }
  | '_'             { ANON }
  | '>'             { GREATER }
  | '<'             { LOWER }
  | ">="            { GE }
  | "<="            { LE }
  | "<>"            { NE }
  | "&&"            { AND }
  | "||"            { OR }
  | "not"           { NOT }
  | "begin" { BEGIN }
  | "end" { END }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN}
  | "let rec" { REC }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "prInt" { PRINT }
  | "fun" { FUN }
  | "->" { FLECHE }
  | ('-'|'+')?['0'-'9']+'.'['0'-'9']* as s { NBR (float_of_string s) }
  | ['0'-'9']+ as s { INT (int_of_string s) }
  | (['A'-'Z']|['a'-'z'])(['A'-'Z']|['a'-'z']|'_'|['0'-'9'])* as s { VAR s }
  (*| (['A'-'Z']|['a'-'z'])*+['0'-'9']* as s { VAR s }*)
