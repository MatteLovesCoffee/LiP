{
open Parser
}

let quote = '"'
let integertype = ['i' 'u'] '8' | "16" | "32" | "64" | "128" | "size"
let intref = '&'integertype
let floattype = 'f' "32" | "64"
let string = ['a'-'z' 'A'-'Z' '0'-'9']*
let id = ['a'-'z'] string
let str_type = "str"
let str_ref = '&'str_type
let str_decl = "String::from"
let nat = ['0'-'9']+
let float = nat?'.'nat
let white = [' ' '\t' '\n']+

rule read =
    parse
    | white { read lexbuf }
    | "println!" { PRINT "println!" }
    | "mut" { MUT }
    | "fn" { FUN }
    | "->" { RETURNS }
    | "return" { RETURN }
    | "true" { TRUE }
    | "false" { FALSE }
    | '&' { BORROW }
    | ',' { COMMA }
    | ';' { SEQ }
    | "=" { ASSIGN }
    | ':' { OFTYPE }
    | "let" { LET }
    | "if" { IF }
    | "else" { ELSE }
    | "loop" { LOOP }
    | "break" { BREAK }
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | "==" { EQ }
    | integertype { INTTYPE }
    | intref { INTREF }
    | str_type { STRINGTYPE}
    | str_ref { STRREF }
    | quote '{' { LACCESS }
    | '}' quote { RACCESS }
    | quote string quote { STRING (Lexing.lexeme lexbuf) }
    | str_decl { STR_DECL }
    | "<=" { LEQ }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '{' { LBRACE }
    | '}' { RBRACE }
    (* scrapped:
    | floattype { FLOATTYPE}
    | "char" { CHARTYPE }
    | "bool" { BOOLTYPE }
    *)
    | id { ID (Lexing.lexeme lexbuf) }
    | nat { INTCONST (Lexing.lexeme lexbuf) }
    | float { FLOATCONST (Lexing.lexeme lexbuf) }
    | eof { EOF }