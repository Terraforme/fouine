%{
  open Types
(* --- préambule: ici du code Caml --- *)
%}
/* énumération des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <float> NBR       /* le lexème NBR a un attribut float */
%token <string> VAR       /* le lexème VAR a un attribut, de type string */
%token LPAREN RPAREN EQUAL SEMICOL DOT COLON
%token PLUS MINUS DIV TIMES MOD
%token GREATER LOWER GE LE NE AND OR NOT
%token BEGIN END LET IN FUN REC
%token IF THEN ELSE
%token TRUE FALSE
%token EOL EOF

%nonassoc LET IN
%nonassoc IF THEN ELSE

%left MOD
%left PLUS MINUS /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV
%nonassoc UMINUS /* un "faux token", correspondant au "-" unaire */

%left OR
%left AND
%nonassoc NOT

%start main
%type <Types.expr_f> main

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
    expression EOF                { $1 }  /* on veut reconnaître une "expression" */
;


expression:
  | LPAREN expression RPAREN           { $2 }
  /*| expression binary_operator expression { Bin($1,$2,$3) } %prec ???*/
  | expression PLUS expression { Bin($1,Plus,$3) }
  | expression TIMES expression { Bin($1,Times,$3) }
  | expression MINUS expression { Bin($1,Minus,$3) }
  | expression DIV expression { Bin($1,Div,$3) }
  | expression MOD expression { Bin($1,Mod,$3) }

  | INT   { Cst $1 }
  | VAR { Var $1 }
  | LET VAR EQUAL expression IN expression { Let($2, $4, $6) }
  /*| IF bool_expr THEN expression { If($2,$4) } */
  | IF bool_expr THEN expression ELSE expression { IfElse($2,$4,$6) }
  | MINUS expression %prec UMINUS       { Bin(Cst 0, Minus, $2) } /*un peu spécial: c'est le seul opérateur "unaire" pour le parseur */
;


bool_expr:
  | LPAREN bool_expr RPAREN           { $2 }
  | TRUE { True }
  | FALSE { False }
  /*| expression comparative_operator expression { Cmp($1,$2,$3) }*/
  | expression EQUAL expression { Cmp($1,Eq,$3) }
  | expression NE expression { Cmp($1,Neq,$3) }
  | expression LE expression { Cmp($1,Leq,$3) }
  | expression LOWER expression { Cmp($1,Lt,$3) }
  | expression GE expression { Cmp($1,Geq,$3) }
  | expression GREATER expression { Cmp($1,Gt,$3) }

  /*| bool_expr boolean_operator bool_expr { Bin_op($1,$2,$3) }*/
  | bool_expr OR bool_expr { Bin_op($1,Or,$3) }
  | bool_expr AND bool_expr { Bin_op($1,And,$3) }
  | NOT bool_expr { Not($2) }
;

/*
binary_operator:
  | PLUS         { Plus }
  | TIMES        { Times }
  | MINUS      { Minus }
  | DIV         { Div }
  | MOD           { Mod }
;
*/

/*
comparative_operator:
  | EQUAL { Eq }
  | NE { Neq }
  | LE { Leq }
  | LOWER { Lt }
  | GE { Geq }
  | GREATER { Gt }
;
*/

/*
boolean_operator:
  | OR { Or }
  | AND { And }
;
*/
