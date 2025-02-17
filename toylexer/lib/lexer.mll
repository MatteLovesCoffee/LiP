{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let atok = ['A'-'Z'] chr+
let btok = (['a'-'z']#[^ 'a' 'e' 'i' 'o' 'u'])+
let ctok = ['a'-'z' 'A'-'Z']#['a' 'e' 'i' 'o' 'u']* ['a' 'e' 'i' 'o' 'u'] ['a'-'z' 'A'-'Z']#['a' 'e' 'i' 'o' 'u']
let dtok = '-'? ['0'-'9']+ '.'? ['0'-'9']+
let etok = ("0x"|"0X") ['0'-'9' 'A'-'F' 'a'-'f']+
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ } 
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }    
  | eof { EOF }
