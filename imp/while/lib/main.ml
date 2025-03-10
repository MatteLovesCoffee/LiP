open Ast
open Types


let parse (s : ide) =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let bind st ide ev = fun ide' -> if ide = ide' then ev else st ide';;

let rec eval_expr st = function
  | True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const c -> Nat c
  | Not e -> (match (eval_expr st e) with Bool e' -> Bool (not e') | _ -> raise (TypeError "asdasdad"))
  | And(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Bool v1, Bool v2 -> Bool(v1 && v2) | _, _ -> raise (TypeError "asdasdad"))
  | Or(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Bool v1, Bool v2 -> Bool(v1 || v2) | _, _ -> raise (TypeError "asdasdad"))
  | Add(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Nat v1, Nat v2 -> Nat(v1 + v2) | _, _ -> raise (TypeError "asdasdad"))
  | Sub(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Nat v1, Nat v2 -> Nat(v1 - v2) | _, _ -> raise (TypeError "asdasdad"))
  | Mul(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Nat v1, Nat v2 -> Nat(v1 * v2) | _, _ -> raise (TypeError "asdasdad"))
  | Eq(e1, e2) -> let v1, v2 = eval_expr st e1, eval_expr st e2 in Bool (v1 = v2)
  | Leq(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Nat v1, Nat v2 -> Bool(v1 <= v2) | _, _ -> raise (TypeError "asdasdad"))


(* raises UnboundVar and NoRulesApplies*)
let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign(x, e), st) -> St (bind st x (eval_expr st e))
  | Cmd (Seq(c1, c2), st) -> (match (trace1 (Cmd (c1, st))) with St st' -> Cmd (c2, st') | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
  | Cmd(If(e, c1, c2), st) -> if eval_expr st e = Bool(true) then Cmd(c1, st) else if eval_expr st e = Bool(false) then Cmd(c2, st) else raise (TypeError "If")
  | Cmd(While(e, c), st) -> if eval_expr st e = Bool(true) then Cmd(Seq(c, While(e, c)), st) else if eval_expr st e = Bool(false) then St st else raise (TypeError "While")
;;

let bottom : state = fun y -> raise (UnboundVar y);;

let rec trace_rec n t =
  if n <= 0 then [t]
  else
    try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

let trace n c =
  trace_rec n (Cmd(c, bottom))
