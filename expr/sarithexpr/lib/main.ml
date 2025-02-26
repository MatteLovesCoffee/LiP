open Ast

type exprtype = BoolT | NatT
type exprval = Bool of bool | Nat of int

let string_of_type = function
  BoolT -> "Bool"
  | NatT -> "Nat"

let string_of_val = function
  | Nat x -> string_of_int x
  | Bool x -> string_of_bool x

let rec string_of_expr : (expr -> string) = function
  | Zero -> "Zero"
  | True -> "True"
  | False -> "False"
  | Not e -> "Not(" ^ string_of_expr e ^ ")"
  | And(e1, e2) -> "And(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 
  | Or(e1, e2) -> "Or(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Succ(e) -> "Succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "Pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "IsZero(" ^ string_of_expr e ^ ")"
;;

exception TypeError of string;;

let error_msg e type1 type2 =
  string_of_expr e ^ " has type " ^ string_of_type type1 ^ ", but type " ^ string_of_type type2 ^ " was expected."

let raise_type_error e etype exp_type = raise (TypeError (error_msg e etype exp_type));;

let rec typecheck = function
  | Zero -> NatT
  | True | False -> BoolT
  | Not e -> if typecheck e = BoolT then BoolT else raise_type_error e (typecheck e) BoolT
  | And(e1, e2) | Or(e1, e2) -> if typecheck e1 != BoolT || typecheck e2 != BoolT then if typecheck e1 = BoolT then raise_type_error e2 (typecheck e2) BoolT else raise_type_error e1 (typecheck e1) BoolT else BoolT
  | Pred Zero -> raise (TypeError "Pred Zero has no type")
  | Succ e | Pred e -> if typecheck e != NatT then raise_type_error e (typecheck e) NatT else NatT
  | IsZero e -> if typecheck e != NatT then raise_type_error e (typecheck e) NatT else BoolT
  | If(e0, e1, e2) -> if typecheck e0 != BoolT then raise_type_error e0 (typecheck e0) BoolT else if typecheck e1 != typecheck e2 then raise_type_error e2 (typecheck e2) (typecheck e1) else typecheck e1  

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let rec is_nv = function
  Zero -> true
  | Succ x -> is_nv x
  | Pred x -> if is_nv x then false else true
  | _ -> false

exception NoRuleApplies
let rec trace1 = function
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  | And(True, e) -> e
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e) -> e
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If(trace1 e0, e1, e2)
  | Pred(Succ(e)) -> e
  | Succ(e) -> Succ(trace1 e)
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) ->IsZero(trace1 e)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

let eval_bool = function
  Bool(value) -> value
  | Nat(_) -> failwith "Type error: found int instead of bool"

let eval_int = function
  Nat(value) -> value
  | Bool(_) -> failwith "Type error: found bool instead of int"

let rec eval expr = match expr with
  | Zero -> Nat(0)
  | True -> Bool(true)
  | False -> Bool(false)
  | Not(e) -> Bool(not (eval_bool (eval e)))
  | And(e1, e2) -> Bool (eval_bool (eval e1) && eval_bool (eval e2))
  | Or(e1, e2) -> Bool (eval_bool (eval e1) || eval_bool (eval e2))
  | If(e0, e1, e2) -> Bool(if eval_bool (eval e0) then eval_bool (eval e1) else eval_bool(eval e2))
  | Succ(e) -> Nat(eval_int (eval e) + 1)
  | Pred(e) -> let e1 = (eval_int (eval e)) in if e1 = 0 then failwith "Value error: negative number" else Nat(e1 - 1)
  | IsZero(e) -> if(eval_int(eval e) = 0) then Bool(true) else Bool(false)
