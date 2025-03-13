{
open Parser
}

let string = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let nat = ['0'-'9']+
let white = [' ' '\t' '\n']+

rule read =
    parse
    | white { read lexbuf }
    | "int" { INT }
    | "bool" { BOOL }
    | "skip" { SKIP }
    | "true" { TRUE }
    | "false" { FALSE }
    | ';' { SEQ }
    | ":=" { ASSIGN }
    | "while" { WHILE }
    | "do" { DO }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | '+' { ADD }
    | '-' { SUB }
    | '*' { MUL }
    | '=' { EQ }
    | "<=" { LEQ }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | string { VAR (Lexing.lexeme lexbuf) }
    | nat { CONST (Lexing.lexeme lexbuf) }
    | eof { EOF }