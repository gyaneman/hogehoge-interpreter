%{
open Syntax
%}

%token <string> IDENTIFIER
%token <int> INT
%token CAMMA EXCL
%token TRUE FALSE
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token LET IN PROC LETREC
%token IF THEN ELSE
%token REF SET
%token EQ
%token EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS
%start main
%type <Syntax.exp> main

%%

main:
  | exp EOF { $1 }
;

exp:
  // integer literal
  | INT { IntLit $1 }

  // ( exp )
  | LPAREN exp RPAREN { $2 }

  // exp + exp
  | exp PLUS exp
    { Plus ($1, $3) }
  // exp - exp
  | exp MINUS exp
    { Minus ($1, $3) }
  // exp * exp
  | exp TIMES exp
    { Times ($1, $3) }
  // exp / exp
  | exp DIV exp
    { Div ($1, $3) }

  // true
  | TRUE
    { BoolLit (true) }
  // false
  | FALSE
    { BoolLit (false) }

  // - exp
  | MINUS exp %prec UMINUS
    { Minus (IntLit 0, $2) }

  // let identifier in exp
  | LET IDENTIFIER EQ exp IN exp
    { Let ($2, $4, $6) }

  // proc ( ident_list ) exp
  | PROC LPAREN ident_list RPAREN exp
    { Proc ($3, $5) }

  // letrec id = exp in exp
  | LETREC IDENTIFIER LPAREN ident_list RPAREN EQ exp IN exp
    { Letrec ($2, $4, $7, $9) }

  // exp (exp_list)
  | exp LPAREN exp_list RPAREN
    { Call ($1, $3) }

  // if exp then exp else exp
  | IF exp THEN exp ELSE exp
    { If ($2, $4, $6) }

  // ref exp
  | REF exp
    { Ref ($2) }

  // ! exp
  | EXCL exp
    { Deref ($2) }

  // set exp = exp
  | SET exp EQ exp
    { Set ($2, $4) }

  // variable
  | IDENTIFIER
    { Var ($1) }
;

ident_list:
    { [] }
  | ident_list_rec
    { $1 }
;
ident_list_rec:
  | IDENTIFIER
    { [$1] }
  | ident_list CAMMA IDENTIFIER
    { $1 @ [$3] }
;

exp_list:
    { [] }
  | exp_list_rec
    { $1 }
;
exp_list_rec:
  | exp
    { [$1] }
  | exp_list CAMMA exp
    { $1 @ [$3] }
;
