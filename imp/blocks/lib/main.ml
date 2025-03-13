open Ast
open Types

let parse (s : ide) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let rec eval_expr (st : state) = function
  | True -> Bool true
  | False -> Bool false
  | Const i -> Int i
  | Var(x) ->
    (match (topenv st) x with
      BVar loc -> getmem st loc
      | IVar loc -> getmem st loc)
  | Not e -> (match eval_expr st e with Bool b -> Bool (not b) | _ -> raise (TypeError "Expected Bool, found Int"))
  | And(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Bool b1, Bool b2 -> Bool (b1 && b2) | _ -> raise (TypeError "Expected Bool, found Int"))
  | Or(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Bool b1, Bool b2 -> Bool (b1 || b2) | _ -> raise (TypeError "Expected Bool, found Int"))
  | Add(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Int i1, Int i2 -> Int(i1 + i2) | _ -> raise (TypeError "Expected Int, found Bool"))
  | Sub(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Int i1, Int i2 -> Int(i1 - i2) | _ -> raise (TypeError "Expected Int, found Bool"))
  | Mul(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Int i1, Int i2 -> Int(i1 * i2) | _ -> raise (TypeError "Expected Int, found Bool"))
  | Eq(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Int i1, Int i2 -> Bool(i1 = i2) | _ -> raise (TypeError "Expected Int, found Bool"))
  | Leq(e1, e2) -> (match eval_expr st e1, eval_expr st e2 with Int i1, Int i2 -> Bool(i1 <= i2) | _ -> raise (TypeError "Expected Int, found Bool"))

type scope = env * loc;;
let rec eval_decl (sc : scope) = function
    | [] -> sc
    | IntVar id :: t -> eval_decl (bind_env (fst sc) id (IVar (snd sc)), (snd sc + 1)) t
    | BoolVar id :: t -> eval_decl (bind_env (fst sc) id (BVar (snd sc)), (snd sc + 1)) t

let rec trace1_cmd = function
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd(Decl (dlist, cmd), st)-> let scope = eval_decl (topenv st, getloc st) dlist in Cmd(Block(cmd), make_state ((fst scope) :: (getenv st)) (getmem st) (snd scope))
  | Cmd (Assign(x, e), st) -> 
    (match topenv st x, eval_expr st e with
      BVar loc, Bool b -> St (setmem st (bind_mem (getmem st) loc (Bool b)))
      | IVar loc, Int i -> St (setmem st (bind_mem (getmem st) loc (Int i)))
      | BVar _, _ -> raise (TypeError "Expected Bool, found Int")
      | IVar _, _ -> raise (TypeError "Expected Int, found Bool"))
  | Cmd (Seq(c1, c2), st) -> (match (trace1_cmd (Cmd (c1, st))) with St st' -> Cmd (c2, st') | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
  | Cmd(If(e, c1, c2), st) ->
    (match eval_expr st e with
      Bool b -> if b then Cmd(c1, st) else Cmd(c2, st)
      | _ -> raise (TypeError "Expected Bool, found Int"))
  | Cmd(While(e, c), st) ->
    (match eval_expr st e with
      Bool b -> if b then Cmd(Seq(c, While(e, c)), st) else St st
      | _ -> raise (TypeError "Expected Bool, found Int"))
    (* Cmd(If(e, Seq(c, While(e, c)), Skip), st) *)
  | Cmd(Block c, st) ->
    (match trace1_cmd (Cmd(c, st)) with
      St st' -> St(make_state (popenv st') (getmem st') (getloc st'))
      | Cmd(c', st') -> Cmd(Block(c'), st'))
    (* trace1_cmd (Cmd (cmd, setenv st ((topenv st) :: getenv st)))*)

let rec trace_rec n t =
  if n <= 0 then [t]
  else
    try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

let trace n c =
  trace_rec n (Cmd(c, state0))
