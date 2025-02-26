%{
  open Ast
%}

// il lexer si occupa di associare questi nomi
// al loro effettivo significato

%token ZERO
%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token SUCC
%token PRED
%token ISZERO
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token EOF

%left OR
%left AND
%left NOT
%nonassoc ISZERO
%nonassoc SUCC
%nonassoc PRED


%start <expr> prog
%%

prog:
  | e = expr; EOF { e }
;

expr:
  | LPAREN; e = expr; RPAREN { e }
  | ZERO { Zero }
  | TRUE { True }
  | FALSE { False }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr; { Or(e1, e2) }
  | IF; e0 = expr; THEN; e1 = expr; ELSE; e2 = expr; { If(e0, e1, e2) }
  | SUCC; e = expr { Succ(e) }
  | PRED; e = expr { Pred(e) }
  | ISZERO; e = expr { IsZero(e) }
;