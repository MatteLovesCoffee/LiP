%{
  open Ast
%}

%token <string> CONST
%token <string> VAR
%token TRUE
%token FALSE
%token LBRACE
%token RBRACE
%token RETURN
%token INT
%token FUN
%token LPAREN
%token RPAREN
%token SEQ
%token ASSIGN
%token WHILE
%token DO
%token IF
%token THEN
%token ELSE
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token NOT
%token AND
%token OR
%token SKIP
%token EOF

%left SEQ
%nonassoc ELSE DO
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%start <prog> prog
%%

prog:
  | d = list(decl); c = cmd; EOF { Prog(d, c) }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; ADD; e2 = expr; { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1, e2) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | f = VAR; LPAREN; e = expr; RPAREN { Call(f, e) }
  | x = VAR; { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | SKIP { Skip }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | s = VAR; ASSIGN; e = expr { Assign(s, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1, c2) }
  | LPAREN; c = cmd; RPAREN { c }
;

decl:
  | INT; x = VAR; SEQ { IntVar x }
  | INT; x = VAR { IntVar x }
  | FUN; f = VAR; LPAREN; x = VAR; RPAREN; LBRACE; c = cmd; SEQ; RETURN; e = expr; RBRACE; SEQ { Fun(f, x, c, e)}
  | FUN; f = VAR; LPAREN; x = VAR; RPAREN; LBRACE; c = cmd; SEQ; RETURN; e = expr; RBRACE { Fun(f, x, c, e)}