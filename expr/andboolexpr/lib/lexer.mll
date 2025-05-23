{
open Parser
}

let white = [' ' '\t']+

rule read =
    parse
    | white { read lexbuf }
    | "true" { TRUE }
    | "false" { FALSE }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { EOF }
    | _ { failwith "Unexpected character" }