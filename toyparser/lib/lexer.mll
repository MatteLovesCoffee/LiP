{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let hex = ("0x"|"0X")['a'-'f' 'A'-'F' '0'-'9']+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MUL }
  | "/" { DIV }
  | (('-'?num)|hex) { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
