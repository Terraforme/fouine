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
%token EOL EOF

%left PLUS MINUS  /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES
%nonassoc UMINUS /* un "faux token", correspondant au "-" unaire */
%left DIV


%start main
%type <Types.expr_f list> main

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:                       /* <- le point d'entrée (cf. + haut, "start") */
    l_expression EOF                { $1 }  /* on veut reconnaître une "expression" */
;

l_expression:
  | expression l_expression { $1::$2 }
  | expression              { [$1] }
;

expression:			    /* règles de grammaire pour les expressions */
  | INT                                { Cst $1 }
  | LPAREN expression RPAREN           { $2 } /* on récupère le deuxième élément */
  | expression PLUS expression          { Plus($1,$3) }
  | expression TIMES expression         { Times($1,$3) }
  | expression MINUS expression         { Minus($1,$3) }
  | MINUS expression %prec UMINUS       { Minus(Cst 0, $2) }
  | expression DIV expression           { Div($1,$3) }
  |expression MOD expression            { Mod($1,$3) }
;
