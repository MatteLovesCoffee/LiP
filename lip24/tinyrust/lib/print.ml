open Ast
open Types

(*
Libreria per la visualizzazione dei trace a scopo di debugging.
*)

let rec string_of_memval m loc = match m loc with
  | IntRef loc' -> "ref of " ^ string_of_memval m loc'
  | StringRef loc' -> "ref of " ^ string_of_memval m loc'
  | Int int -> string_of_int int
  | String string -> string
  | Bool bool -> string_of_bool bool
;;

let rec string_of_expr_list = function
  | [] -> ""
  | e :: t -> string_of_expr e ^ "," ^ string_of_expr_list t
and string_of_decl_list = function
  | [] -> ""
  | d :: t -> string_of_decl d ^ "," ^ string_of_decl_list t
and string_of_expr = function
  | Uninit -> ""
  | BoolConst true -> "true"
  | BoolConst false -> "false"
  | Var x | Mut x -> x
  | TVar (x, _) | TMut (x, _) -> x
  | IntConst n -> string_of_int n
  | FloatConst f -> string_of_float f
  | CharConst c -> String.make 1 c
  | StringConst s -> s
  | Not e -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  | Call(f,e) -> f ^ "(" ^ string_of_expr_list e ^ ")"
  | CallExec(c,e) -> "exec{" ^ string_of_cmd c ^ "; ret " ^ string_of_expr e ^ "}"
  | CallRet(e) -> "{ret " ^ string_of_expr e ^ "}"

and string_of_cmd = function
    Skip -> "skip"
  | Assign(x,e) -> x ^ ":=" ^ string_of_expr e
  | Seq(c1,c2) -> string_of_cmd c1 ^ "; " ^ string_of_cmd c2
  | If(e,c1,c2) -> "if " ^ string_of_expr e ^ " then " ^ string_of_cmd c1 ^ " else " ^ string_of_cmd c2
  | Loop(c) -> "loop { " ^ string_of_cmd c ^ "}"
  | Break -> "break"
  | Decl (d, c) -> string_of_decl d ^ "; " ^ string_of_cmd c
  | Block c -> "{"^ string_of_cmd c ^"}"
and string_of_decl = function
  | IntVar (x, e) | BIntVar (x, e) | MIntVar (x, e) | BMIntVar (x, e) | MBIntVar (x, e) | MBMIntVar (x, e)  -> "int " ^ x ^ if e = Uninit then "" else " = " ^ string_of_expr e
  | GenericVar (x, e) | BGenericVar (x, e) | MGenericVar (x, e) | BMGenericVar (x, e) | MBGenericVar (x, e) | MBMGenericVar (x, e)  -> "var " ^ x ^ if e = Uninit then "" else " = " ^ string_of_expr e
  | Fun(f, x, t, c, _) -> "fun " ^ f ^ "(" ^ string_of_decl_list x ^ ") -> " ^ string_of_types t ^ " {" ^ string_of_cmd c ^ "}"
  | Proc(f, x, c) -> "fun " ^ f ^ "(" ^ string_of_decl_list x ^ ") {" ^ string_of_cmd c ^ "}"
and string_of_types = function
  | IntType -> "int"
  | StringType -> "str"
  | IntRef -> "&int"
  | StringRef -> "&str"

let string_of_env1 s x = match topenv s x with
  | Mut (l, _, _) | Imm (l, _, _) -> string_of_int l ^ "/" ^ x
  | Fun(_, c, t, _) -> "fun {" ^ string_of_cmd c ^ "} -> "  ^ string_of_types t ^"/" ^ x
  | Proc(_, c) -> "proc {" ^ string_of_cmd c ^ "}/" ^ x

let rec string_of_env s = function
    [] -> ""
  | [x] -> (try string_of_env1 s x with _ -> "")
  | x::dom' -> (try string_of_env1 s x ^ "," ^ string_of_env s dom'
                with _ -> string_of_env s dom')

let string_of_mem1 (m,l) i =
  assert (i<l);
  string_of_memval m i ^ "/" ^ string_of_int i

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_mem (m,l) =
  List.fold_left (fun str i -> str ^ (try string_of_mem1 (m,l) i ^ "," with _ -> "")) "" (range 0 (l - 1))

let rec getlocs e = function
    [] -> []
  | x::dom -> try (match e x with
  | Mut (l, _, _) | Imm (l, _, _) -> l::(getlocs e dom)
  | _ -> (getlocs e dom))
    with _ -> getlocs e dom

let string_of_state st dom =
  "[" ^ string_of_env st dom ^ "], " ^
  "[" ^ string_of_mem (getmem st, getloc st) ^ "]" ^ ", " ^
  string_of_int (getloc st)

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

let rec vars_of_expr = function
  | Uninit
  | IntConst _
  | FloatConst _
  | CharConst _
  | StringConst _
  | BoolConst _ -> []
  | Var x | Mut x | TVar (x, _) | TMut (x, _) -> [x]
  | Not e -> vars_of_expr e
  | And(e1,e2)
  | Or(e1,e2)
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)
  | Eq(e1,e2)
  | Leq(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Call(f,e) -> union [f] (List.flatten (List.map vars_of_expr e))
  | CallExec(c,e) -> union (vars_of_cmd c) (vars_of_expr e)
  | CallRet(e) -> vars_of_expr e

and vars_of_cmd = function
    Skip
  | Break -> []
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Loop(c) -> union ["loop: "] (vars_of_cmd c)
  | Decl(d, c) -> union (vars_of_decl d) (vars_of_cmd c)
  | Block c -> union ["{"] (union (vars_of_cmd c) ["}"] )

and vars_of_decl = function
  | IntVar (x, e) (* immutable var *)
  | GenericVar (x, e) (* immutable var *)
  | BIntVar (x, e) (* immutable borrowing *)
  | BGenericVar (x, e) (* immutable borrowing *)
  | MBIntVar (x, e) (* mutable var immutable borrowing *)
  | MBGenericVar (x, e) (* mutable var immutable borrowing *)
  | MIntVar (x, e) (* mutable var *)
  | MGenericVar (x, e) (* mutable var *)
  | BMIntVar (x, e) (* mutable borrowing *)
  | BMGenericVar (x, e) (* mutable borrowing *)
  | MBMIntVar (x, e) (* mutable var mutable borrowing *)
  | MBMGenericVar (x, e) -> union [x] (vars_of_expr e)
  | Fun(f,dl,_,c,e) -> union [f] (union (List.flatten (List.map (vars_of_decl) dl)) (union (vars_of_cmd c) (vars_of_expr e)))
  | Proc(f,dl,c) -> union [f] (union (List.flatten (List.map (vars_of_decl) dl)) (vars_of_cmd c))

let string_of_conf vars = function
    St st -> string_of_state st vars
  | Cmd(c,st) -> "<" ^ string_of_cmd c ^ ", " ^ string_of_state st vars ^ ">"

let rec string_of_trace vars = function
    [] -> ""
  | [x] -> (string_of_conf vars x)
  | x::l -> (string_of_conf vars x) ^ "\n -> " ^ string_of_trace vars l
