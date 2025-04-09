%{
  open Ast
%}

%token <string> INTCONST
%token <string> FLOATCONST
%token <string> ID
%token <string> STRING

%token TRUE
%token FALSE
%token RETURN
%token BORROW
%token OFTYPE
%token INTTYPE
%token INTREF
(* scrapped:
%token FLOATTYPE
%token CHARTYPE
%token BOOLTYPE
*)
%token STRINGTYPE
%token STRREF
%token MUT
%token STR_DECL
%token FUN
%token RETURNS

%token LET
%token SEQ
%token COMMA
%token ASSIGN
%token LOOP
%token BREAK
%token IF
%token ELSE

%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token NOT
%token AND
%token OR
%token <string> PRINT
%token LACCESS
%token RACCESS

%token LPAREN
%token RPAREN
// %token LSQUARE
// %token RSQUARE
%token LBRACE
%token RBRACE

%token EOF

%left SEQ
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left ADD SUB
%left MUL

%start <cmd> parse_string
%%

params:
  | x = ID; OFTYPE; INTTYPE; COMMA { IntVar(x, Uninit) }
  | x = ID; OFTYPE; STRINGTYPE; COMMA { GenericVar(x, Uninit) }
  | x = ID; OFTYPE; INTREF; COMMA { BIntVar(x, Uninit) }
  | x = ID; OFTYPE; STRREF; COMMA { BGenericVar(x, Uninit) }
  | x = ID; OFTYPE; INTTYPE { IntVar(x, Uninit) }
  | x = ID; OFTYPE; STRINGTYPE { GenericVar(x, Uninit) }
  | x = ID; OFTYPE; INTREF { BIntVar(x, Uninit) }
  | x = ID; OFTYPE; STRREF { BGenericVar(x, Uninit) }
;

types_b:
  | BORROW; INTTYPE { IntType }
  | BORROW; STRINGTYPE { StringType }
  | BORROW; MUT; INTTYPE { IntType }
  | BORROW; MUT; STRINGTYPE { StringType }
  (* scrapped:
  | BORROW; FLOATTYPE { FloatType }
  | BORROW; CHARTYPE { CharType }
  | BORROW; BOOLTYPE { BoolType }
  | BORROW; MUT; FLOATTYPE { FloatType }
  | BORROW; MUT; CHARTYPE { CharType }
  | BORROW; MUT; BOOLTYPE { BoolType }
  *)
;

types_o:
  | INTTYPE { IntType }
  | STRINGTYPE { StringType }
  (* scrapped:
  | FLOATTYPE { FloatType }
  | CHARTYPE { CharType }
  | BOOLTYPE { BoolType }
  *)
;

types:
  | t = types_b { t }
  | t = types_o { t }
;

parse_string:
  | c = cmd; EOF { c }
;

expr:
  | n = INTCONST { IntConst(int_of_string n) }
  | n = FLOATCONST { FloatConst(float_of_string n) }
  | STR_DECL; LPAREN; s = STRING; RPAREN { StringConst(s)} 
  | TRUE { BoolConst(true) }
  | FALSE { BoolConst(false) }
  | NOT; e = expr; { Not(e) }
  | e1 = expr; ADD; e2 = expr; { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr; { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr; { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr; { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr; { Leq(e1, e2) }
  | e1 = expr; AND; e2 = expr; { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | f = PRINT; LPAREN; LACCESS; x = ID; RACCESS; RPAREN { Call(f, [Var(x)])}
  | f = ID; LPAREN; e = list(expr); RPAREN { Call(f, e) }
  | x = ID; { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

cmd:
  | IF; e = expr; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE { If(e, c1, c2) }
  | LOOP; c = cmd { Loop(c) }
  | x = ID; ASSIGN; e = expr { Assign(x, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1, c2) }
  | d = decl; c = cmd { Decl(d, c) }
  | LBRACE; c = cmd; RBRACE { Block(c) }
  | LPAREN; c = cmd; RPAREN { c }
  | BREAK { Break }
  | { Skip }
;

mut_var_decl_b: // mutable borrowing
  | LET; MUT; x = ID; OFTYPE; INTREF; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMIntVar(x, Var(y)); }
  | LET; MUT; x = ID; OFTYPE; STRREF; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMGenericVar(x, Var(y)) }
  | LET; MUT; x = ID; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMGenericVar(x, Var(y)) }
  | LET; x = ID; OFTYPE; INTREF; ASSIGN; BORROW; MUT; y = ID; SEQ { BMIntVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; STRREF; ASSIGN; BORROW; MUT; y = ID; SEQ { BMGenericVar(x, Var(y)) }
  | LET; x = ID; ASSIGN; BORROW; MUT; y = ID; SEQ { BMGenericVar(x, Var(y)) }
  (* scrapped:
  | LET; MUT; x = ID; OFTYPE; FLOATTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMFloatVar(x, Var(y)); }
  | LET; MUT; x = ID; OFTYPE; CHARTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMCharVar(x, Var(y)); }
  | LET; MUT; x = ID; OFTYPE; BOOLTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { MBMBoolVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; FLOATTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { BMFloatVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; CHARTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { BMCharVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; BOOLTYPE; ASSIGN; BORROW; MUT; y = ID; SEQ { BMBoolVar(x, Var(y)); }
  *)
;

mut_var_decl_o: // mutable ownership
  | LET; MUT; x = ID; OFTYPE; INTTYPE; ASSIGN; e = expr; SEQ { MIntVar(x, e); }
  | LET; MUT; x = ID; ASSIGN; e = expr; SEQ { MGenericVar(x, e) }
  | LET; MUT; x = ID; OFTYPE; INTTYPE; SEQ { MIntVar(x, Uninit); }
  | LET; MUT; x = ID; SEQ { MGenericVar(x, Uninit) }
  (*scrapped:
  | LET; MUT; x = ID; OFTYPE; FLOATTYPE; ASSIGN; e = expr; SEQ { MFloatVar(x, e); }
  | LET; MUT; x = ID; OFTYPE; CHARTYPE; ASSIGN; e = expr; SEQ { MCharVar(x, e); }
  | LET; MUT; x = ID; OFTYPE; BOOLTYPE; ASSIGN; e = expr; SEQ { MBoolVar(x, e); }
  | LET; MUT; x = ID; OFTYPE; FLOATTYPE; SEQ { MFloatVar(x, Uninit); }
  | LET; MUT; x = ID; OFTYPE; CHARTYPE; SEQ { MCharVar(x, Uninit); }
  | LET; MUT; x = ID; OFTYPE; BOOLTYPE; SEQ { MBoolVar(x, Uninit); }
  *)
;

imm_var_decl_b:
  | LET; x = ID; OFTYPE; INTREF; ASSIGN; BORROW; y = ID; SEQ { BIntVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; STRREF; ASSIGN; BORROW; y = ID; SEQ { BGenericVar(x, Var(y)) }
  | LET; x = ID; ASSIGN; BORROW; y = ID; SEQ { BGenericVar(x, Var(y)) }
  (* scrapped:
  | LET; x = ID; OFTYPE; FLOATTYPE; ASSIGN; BORROW; y = ID; SEQ { BFloatVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; CHARTYPE; ASSIGN; BORROW; y = ID; SEQ { BCharVar(x, Var(y)); }
  | LET; x = ID; OFTYPE; BOOLTYPE; ASSIGN; BORROW; y = ID; SEQ { BBoolVar(x, Var(y)); }
  *)
;

imm_var_decl_o:
  | LET; x = ID; OFTYPE; INTTYPE; ASSIGN; e = expr; SEQ { IntVar(x, e); }
  | LET; x = ID; OFTYPE; INTTYPE; SEQ { IntVar(x, Uninit); }
  | LET; x = ID; SEQ { GenericVar(x, Uninit) }
  (* scrapped:
  | LET; x = ID; OFTYPE; FLOATTYPE; ASSIGN; e = expr; SEQ { FloatVar(x, e); }
  | LET; x = ID; OFTYPE; CHARTYPE; ASSIGN; e = expr; SEQ { CharVar(x, e); }
  | LET; x = ID; OFTYPE; BOOLTYPE; ASSIGN; e = expr; SEQ { BoolVar(x, e); }
  | LET; x = ID; OFTYPE; FLOATTYPE; SEQ { FloatVar(x, Uninit); }
  | LET; x = ID; OFTYPE; CHARTYPE; SEQ { CharVar(x, Uninit); }
  | LET; x = ID; OFTYPE; BOOLTYPE; SEQ { BoolVar(x, Uninit); }
  *)
;

mut_var_decl:
  | d = mut_var_decl_b { d } // mutable borrowing
  | d = mut_var_decl_o { d } // mutable ownership
;

imm_var_decl:
  | d = imm_var_decl_o { d } // immutable ownership
  | d = imm_var_decl_b { d } // immutable borrowing
;

var_decl:
  | d = mut_var_decl { d } // mutable
  | d= imm_var_decl { d } // immutable
;

decl:
  | d = var_decl { d } // variabili
  | FUN; f = ID; LPAREN; x = list(params); RPAREN; RETURNS; t = types; LBRACE; c = cmd; SEQ; RETURN; e = expr; RBRACE { Fun(f, x, t, c, e)}
  | FUN; f = ID; LPAREN; x = list(params); RPAREN; LBRACE; c = cmd; SEQ; RBRACE { Proc(f, x, c)}
;
