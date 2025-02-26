{
open Parser
}

let white = [' ' '\t']+

rule read =
    parse
    | white { read lexbuf }
    | '0' { ZERO }
    | "true" { TRUE }
    | "false" { FALSE }
    | "succ" { SUCC }
    | "pred" { PRED }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "iszero" { ISZERO }
    | "not" { NOT }
    | "and" { AND }
    | "or" { OR }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { EOF }
    | _ { failwith "Unexpected character" }