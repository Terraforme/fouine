%{
  open Types
(* --- préambule: ici du code Caml --- *)
%}
/* énumération des lexèmes, ceux-ci sont décrits (par vous) dans lexer.mll */

%token <int> INT       /* le lexème INT a un attribut entier */
%token <float> NBR       /* le lexème NBR a un attribut float */
%token <string> VAR       /* le lexème VAR a un attribut, de type string */
%token LPAREN RPAREN EQUAL SEMICOL COMA DOT COLON AFFECTATION
%token PLUS MINUS DIV TIMES MOD
%token GREATER LOWER GE LE NE AND OR NOT
%token BEGIN END LET IN FUN REC
%token IF THEN ELSE
%token BEGIN END
%token TRUE FALSE
%token EOL EOF EOI
%token PRINT
%token FUN FLECHE
%token REF BANG
%token ANON

%right FUN
%nonassoc ANON
%nonassoc LET IN
%right SEMICOL
%nonassoc IF THEN
%nonassoc ELSE
%right AFFECTATION

%nonassoc COMA

%left MOD
%left PLUS MINUS /* associativité gauche: a+b+c, c'est (a+b)+c */
%left TIMES DIV
%nonassoc UMINUS /* un "faux token", correspondant au "-" unaire */

%left OR
%left AND
%nonassoc NOT

%nonassoc PRINT
%left APPLICATION /* un faux token pour lui dire que expr1 expr2 est une application ssi ça ne peut rien être d'autre */
/*%nonassoc EOI*/
%nonassoc REF
%nonassoc BANG


%start main
%type <Types.expr_f> main

%%
    /* --- début des règles de grammaire --- */
                            /* à droite, les valeurs associées */


main:
    | expression_chain EOF{ $1 }
;

expression_chain:
  | expression { $1 }
  | expression EOI { $1 }
  | expression EOI expression_chain { Let(Var_Pat "_", $1, $3) }
  | let_chain { $1 }
;

let_chain:
  | LET VAR EQUAL expression let_chain { Let(Var_Pat $2, $4, $5) }
  | REC VAR EQUAL expression let_chain { LetRec($2, $4, $5) }
  | LET VAR EQUAL expression EOI expression_chain { Let(Var_Pat $2, $4, $6) }
  | REC VAR EQUAL expression EOI expression_chain { LetRec($2, $4, $6) }
  | LET VAR EQUAL expression { Let(Var_Pat $2, $4, Unit) }
  | REC VAR EQUAL expression { LetRec($2, $4, Unit) }
  | LET VAR EQUAL expression EOI { Let(Var_Pat $2, $4, Unit) }
  | REC VAR EQUAL expression EOI { LetRec($2, $4, Unit) }
  | LET VAR func let_chain{ Let(Var_Pat $2,$3,$4) }
  | REC VAR func let_chain { LetRec($2,$3,$4) }
  | LET VAR func EOI let_chain{ Let(Var_Pat $2,$3,$5) }
  | REC VAR func EOI let_chain { LetRec($2,$3,$5) }
  | LET VAR func { Let(Var_Pat $2,$3, Unit) }
  | REC VAR func { LetRec($2,$3, Unit) }
  | LET VAR func EOI { Let(Var_Pat $2,$3, Unit) }
  | REC VAR func EOI { LetRec($2,$3, Unit) }

  | LET LPAREN var_pattern RPAREN EQUAL expression let_chain { Let($3, $6, $7) }
  | LET LPAREN var_pattern RPAREN EQUAL expression EOI expression_chain { Let($3, $6, $8) }
  | LET LPAREN var_pattern RPAREN EQUAL expression { Let($3, $6, Unit) }
  | LET LPAREN var_pattern RPAREN EQUAL expression EOI { Let($3, $6, Unit) }
  | LET LPAREN var_pattern RPAREN func let_chain{ Let($3,$5,$6) }
  | LET LPAREN var_pattern RPAREN func EOI let_chain{ Let($3,$5,$7) }
  | LET LPAREN var_pattern RPAREN func { Let($3,$5, Unit) }
  | LET LPAREN var_pattern RPAREN func EOI { Let($3,$5, Unit) }
;

var_pattern:
  | VAR { Var_Pat $1 }
  | VAR COMA var_pattern { Pair_Pat($1,$3) }
;

expression:
  | INT   { Cst $1 }
  | VAR { Var $1 }

  | LPAREN expression RPAREN { $2 }
  | LPAREN RPAREN { Unit }

  | expression COMA expression { Pair($1,$3) }

  | BEGIN expression END { $2 }
  | expression SEMICOL expression { Let(Var_Pat "_", $1, $3) }

  /*opérations arithmétiques*/
  | expression PLUS expression { Bin($1,Plus,$3) }
  | expression TIMES expression { Bin($1,Times,$3) }
  | expression MINUS expression { Bin($1,Minus,$3) }
  | expression DIV expression { Bin($1,Div,$3) }
  | expression MOD expression { Bin($1,Mod,$3) }
  | MINUS expression %prec UMINUS       { Bin(Cst 0, Minus, $2) } /*un peu spécial: c'est le seul opérateur "unaire" pour le parseur */

  /*let ... in ...*/
  | LET VAR EQUAL expression IN expression { Let(Var_Pat $2, $4, $6) }
  | LET LPAREN var_pattern RPAREN EQUAL expression IN expression { Let($3, $6, $8) } /*paire, triplet, etc.*/
  | REC VAR EQUAL expression IN expression { LetRec($2, $4, $6) }
  | LET ANON EQUAL expression { $4 } %prec ANON
  | REC ANON EQUAL expression { $4 } %prec ANON
  | LET ANON EQUAL expression IN expression { Let(Var_Pat "_", $4, $6) }
  | REC ANON EQUAL expression IN expression { Let(Var_Pat "_", $4, $6) }

  /*reference*/
  | REF expression { Alloc($2) }
  | BANG expression { Bang($2) }
  | VAR AFFECTATION expression { Aff($1, $3) }

  /*déclarations de fonction*/
  | LET VAR func IN expression { Let(Var_Pat $2,$3,$5) }
  | REC VAR func IN expression { LetRec($2,$3,$5) }
  | FUN VAR FLECHE expression { Fun(Var_Pat $2,$4) } %prec FUN

  /*Application (cas possibles: VAR VAR, VAR INT, (expr) INT, (expr) VAR, VAR (expr), (expr) (expr))*/
  | applicator applicated { App($1,$2) } %prec APPLICATION

  /* if ... then ... else ...*/
  | IF bool_expr THEN expression { If($2,$4) }
  | IF bool_expr THEN expression ELSE expression { IfElse($2,$4,$6) }

  | PRINT expression { PrInt($2) }
;

applicator:
  | VAR { Var $1 }
  | LPAREN expression RPAREN { $2 }
  | applicator applicated { App($1,$2) } %prec APPLICATION
;

applicated: /* il faut garder la distinction avec applicator car on ne peut pas appliquer une constante à quelque chose */
  | VAR { Var $1 }
  | INT { Cst $1 }
  | LPAREN expression RPAREN { $2 }
  | LPAREN RPAREN { Unit }
;

func:
  | VAR EQUAL expression { Fun(Var_Pat $1,$3) }
  | LPAREN var_pattern RPAREN EQUAL expression { Fun($2, $5) }
  | LPAREN RPAREN EQUAL expression { Fun(Var_Pat "_", $4) }
  | VAR func { Fun(Var_Pat $1,$2) }
  | LPAREN var_pattern RPAREN func  { Fun($2, $4) }
  | LPAREN RPAREN func { Fun(Var_Pat "_", $3) }
;


bool_expr:
  | LPAREN bool_expr RPAREN           { $2 }
  | TRUE { True }
  | FALSE { False }

  | expression EQUAL expression { Cmp($1,Eq,$3) }
  | expression NE expression { Cmp($1,Neq,$3) }
  | expression LE expression { Cmp($1,Leq,$3) }
  | expression LOWER expression { Cmp($1,Lt,$3) }
  | expression GE expression { Cmp($1,Geq,$3) }
  | expression GREATER expression { Cmp($1,Gt,$3) }

  | bool_expr OR bool_expr { Bin_op($1,Or,$3) }
  | bool_expr AND bool_expr { Bin_op($1,And,$3) }
  | NOT bool_expr { Not($2) }
;
