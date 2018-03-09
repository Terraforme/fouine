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
%token EOL EOF EOI
%token PRINT
%token FUN FLECHE
%token ANON

%nonassoc FUN
%nonassoc ANON
%nonassoc LET IN
%nonassoc IF THEN
%nonassoc ELSE

%left MOD
%left PLUS MINUS /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV
%nonassoc UMINUS /* un "faux token", correspondant au "-" unaire */

%left OR
%left AND
%nonassoc NOT

%nonassoc PRINT
%left APPLICATION /* un faux token pour lui dire que expr1 expr2 est une application ssi ça ne peut rien être d'autre */

%start main
%type <Types.expr_f> main

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:
	| toplevel EOF { $1 }

toplevel:
	| expression { $1 }
	| expression EOI { $1 }
	| expression EOI toplevel { Let("_", $1, $3) }
	| tplvl_let { $1 }

tplvl_let:
	| LET VAR EQUAL expression { Let($2, $4, Cst 0) }
	| LET VAR EQUAL expression EOI { Let($2, $4, Cst 0) }
	| LET VAR EQUAL expression tplvl_let { Let($2, $4, $5) }
	| LET VAR EQUAL expression EOI toplevel { Let($2, $4, $6) }


expression:
  | INT   { Cst $1 }
  | VAR { Var $1 }

  | LPAREN expression RPAREN           { $2 }

  /*opérations arithmétiques*/
  /*| expression binary_operator expression { Bin($1,$2,$3) } %prec ???*/
  | expression PLUS expression { Bin($1,Plus,$3) }
  | expression TIMES expression { Bin($1,Times,$3) }
  | expression MINUS expression { Bin($1,Minus,$3) }
  | expression DIV expression { Bin($1,Div,$3) }
  | expression MOD expression { Bin($1,Mod,$3) }
  | MINUS expression %prec UMINUS       { Bin(Cst 0, Minus, $2) } /*un peu spécial: c'est le seul opérateur "unaire" pour le parseur */

  /*let ... in ...*/
  | LET VAR EQUAL expression IN expression { Let($2, $4, $6) }
  | LET ANON EQUAL expression { $4 } %prec ANON
  | LET ANON EQUAL expression IN expression { Let("_", $4, $6) }
  | REC VAR EQUAL expression IN expression { LetRec($2, $4, $6) }

  /*déclarations de fonction*/
  | LET VAR func IN expression { Let($2,$3,$5) }
  | REC VAR func IN expression { LetRec($2,$3,$5) }
  | FUN VAR FLECHE expression { Fun($2,$4) } %prec FUN

  /*Application (cas possibles: VAR VAR, VAR INT, (expr) INT, (expr) VAR, VAR (expr), (expr) (expr))*/
  | applicator applicated { App($1,$2) } %prec APPLICATION

  | IF bool_expr THEN expression { If($2,$4) }
  | IF bool_expr THEN expression ELSE expression { IfElse($2,$4,$6) }

  | PRINT expression { PrInt($2) }
;

applicator:
  | VAR { Var $1 }
  | LPAREN expression RPAREN { $2 }
  | applicator applicated { App($1,$2) } %prec APPLICATION
;

applicated: /* il faut garder la distinction avec expr_appl1 car on ne peut pas appliquer une constante à quelque chose */
  | VAR { Var $1 }
  | INT { Cst $1 }
  | LPAREN expression RPAREN { $2 }
;

func:
  | VAR EQUAL expression { Fun($1,$3) }
  | VAR func { Fun($1,$2) }
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
