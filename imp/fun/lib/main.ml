open Ast
open Types

let parse (s : ide) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

type exprvalue = Bool of bool | Int of int;;

let rec trace1_expr (st : state) = function
  | Var(x) -> (Const(apply st x), st)
  | Not True -> (False, st)
  | Not False -> (True, st)
  | Not e -> let (e', st') = trace1_expr st e in (Not(e'), st')
  | And(True, e2) -> (e2, st)
  | And(False, _) -> (False, st)
  | And(e1, e2) -> let (e1',st') = trace1_expr st e1 in (And(e1', e2), st')
  | Or(True, _) -> (True, st)
  | Or(False, e2) -> (e2, st)
  | Or(e1, e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1', e2), st')
  | Add(Const(n1), Const(n2)) -> (Const(n1+n2), st)
  | Add(Const(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Add(Const(n1), e2'), st')
  | Add(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Add(e1', e2), st')
  | Sub(Const(n1), Const(n2)) -> (Const(n1-n2), st)
  | Sub(Const(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Sub(Const(n1), e2'), st')
  | Sub(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Sub(e1', e2), st')
  | Mul(Const(n1), Const(n2)) -> (Const(n1*n2), st)
  | Mul(Const(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Mul(Const(n1), e2'), st')
  | Mul(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Mul(e1', e2), st')
  | Eq(Const(n1), Const(n2)) -> if n1=n2 then (True, st) else (False, st)
  | Eq(Const(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Eq(Const(n1), e2'), st')
  | Eq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Eq(e1', e2), st')
  | Leq(Const(n1), Const(n2)) -> if n1<=n2 then (True, st) else (False, st)
  | Leq(Const(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Leq(Const(n1), e2'), st')
  | Leq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Leq(e1', e2), st')
  | Call(f, Const(n)) ->
    (match apply_fun st f with
      (x, cmd, ret) ->
        let loc = getloc st in
        let env' = bind_env (topenv st) x (IVar loc) in
        let mem' = bind_mem (getmem st) loc n in 
        let st' = make_state (pushenv st env') mem' (loc+1) in
        (CallExec(cmd, ret), st'))
  | Call(f, e) -> let (e', st') = trace1_expr st e in (Call(f, e'), st')
  | CallExec(cmd, ret) ->
    (match trace1_cmd (Cmd(cmd, st)) with
      St st' -> (CallRet(ret), st')
      | Cmd(cmd', st') -> (CallExec(cmd', ret), st'))
  | CallRet(Const(n)) -> let st' = setenv st (popenv st) in (Const(n), st')
  | CallRet(e) -> let (e', st') = trace1_expr st e in (CallRet(e'), st')
  | _ -> raise NoRuleApplies
(* raises UnboundVar and NoRulesApplies*)
and trace1_cmd = function
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign(x, Const(n)), st) -> St (bind_ivar st x n)
  | Cmd (Assign(x, e), st) -> let e', st' = trace1_expr st e in Cmd (Assign(x, e'), st')
  | Cmd (Seq(c1, c2), st) -> (match (trace1_cmd (Cmd (c1, st))) with St st' -> Cmd (c2, st') | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
  | Cmd(If(True, c, _), st) -> Cmd(c, st)
  | Cmd(If(False, _, c), st) -> Cmd(c, st)
  | Cmd(If(e, c1, c2), st) -> let e', st' = trace1_expr st e in Cmd(If(e', c1, c2), st')
  | Cmd(While(e, c), st) -> Cmd(If(e, Seq(c, While(e, c)), Skip), st)

let rec trace_rec n t =
  if n <= 0 then [t]
  else
    try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


let rec eval_dec st = function
    | [] -> st
    | IntVar x :: t -> eval_dec (setloc (setenv st [bind_env (topenv st) x (IVar (getloc st))]) ((getloc st) + 1)) t 
    | Fun (f, x, body, ret) :: t-> eval_dec (setenv st [bind_env (topenv st) f (IFun (x, body, ret))]) t
let trace n (Prog(decl, c)) =
  let st = eval_dec state0 decl in
  trace_rec n (Cmd(c, st))
