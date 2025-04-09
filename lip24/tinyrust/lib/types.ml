open Ast

type loc = int

type param_stack = ide Stack.t

type ownership_status =
 | Owner | Moved | Borrowed of envval list | MutableBorrowed of envval
and envval =
  | Mut of loc * types * ownership_status (* flag per indicare se è borrowed, mutable borrowed o nessuno dei due *)
  | Imm of loc * types * ownership_status
  | Fun of decl list * cmd * types * expr
  | Proc of decl list * cmd

type memval =
  | IntRef of loc
  | StringRef of loc
  | Int of int
  | String of string
  | Bool of bool
  (* scrappred:
  | FloatRef of loc
  | CharRef of loc
  | BoolRef of loc
  | Float of float
  | Char of char
  *)

type env = ide -> envval
type mem = loc -> memval

exception ParserError of string
exception FunError of string
exception VarError of string
exception TypeError of string
exception UnboundVarException of ide
exception UnboundLocException of loc
exception OwnershipError of ide
exception MutBorrowOfNonMut of ide
exception AssignmentError of ide
exception MultipleBorrow of (ide * envval * envval)
exception NoRuleApplies

type state = { envstack : env list; memory : mem; firstloc : loc }

let getenv st = st.envstack
let getmem st = st.memory
let getloc st = st.firstloc

let setenv st envstack =
  { envstack; memory = st.memory; firstloc = st.firstloc }

let setmem st memory =
  { envstack = st.envstack; memory; firstloc = st.firstloc }

let setloc st firstloc =
  { envstack = st.envstack; memory = st.memory; firstloc }

let topenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | e :: _ -> e

let pushenv st env = env :: st.envstack
let popenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | _ :: el' -> el'

let make_state envstack memory firstloc = { envstack; memory; firstloc }
let bind_env (f : env) x (v : envval) y = if String.equal x y then v else f y
let bind_mem (f : mem) x (v : memval) y = if Int.equal x y then v else f y
let bottom_env : env = fun x -> raise (UnboundVarException x)
let bottom_mem : mem = fun l -> raise (UnboundLocException l)
let bind_mut_var (st : state) (x : ide) (v : memval) : state =
  let var = ((topenv st) x) in
  match var, v with
  | Imm (_, _, _), _ -> raise (AssignmentError x) (* assignment to immutable *)
  (* assignment of integer *)
  | Mut (l, IntType, b), Int _ ->
    (match b with
      | Borrowed [] -> raise (ParserError "ParserError")
      | Owner | Moved -> setmem st (bind_mem (getmem st) l v) (* int variable is never really "moved"*)
      | Borrowed (var'::_) | MutableBorrowed var' -> raise (MultipleBorrow (x, var, var')))
  (* assignment of string *)
  | Mut (l, StringType, b), String _ ->
      (match b with
      | Owner -> setmem st (bind_mem (getmem st) l v)
      | Moved -> raise (OwnershipError x)
      | Borrowed _ -> raise (MutBorrowOfNonMut x)
      | MutableBorrowed var' -> raise (MultipleBorrow (x, var, var')))
  | Mut _, _ -> raise (TypeError "Type mismatch")
  | Fun _, _  | Proc _, _ -> raise (TypeError "Tried assigning to function-like")
  (*
  | Mut (l, FloatType, _) -> setmem st (bind_mem (getmem st) l v)
  | Mut (l, BoolType, _), Bool _ -> setmem st (bind_mem (getmem st) l v)
  | Mut (l, CharType, _), Char _ -> setmem st (bind_mem (getmem st) l v)
  *)
;;

let rec build_param_stack paramS = function
  | ide :: t -> let () = Stack.push ide paramS in build_param_stack paramS t
  | [] -> paramS

let bind_fun (st : state) (fn : ide) (paraml : decl list) (body: cmd) (t : types) (e: expr) =
  setenv st (pushenv st (bind_env (topenv st) fn (Fun(paraml, body, t, e))))
;;

let bind_proc (st : state) (fn : ide) (paraml : decl list) body =
  setenv st (pushenv st (bind_env (topenv st) fn (Proc(paraml, body))))
;;

let state0 = make_state [bottom_env] bottom_mem 0

let apply (st : state) (x : ide) : memval =
  match (topenv st) x with
  | Imm (l, _, _) | Mut (l, _, _) -> (getmem st) l
  | Fun _ | Proc _ -> raise (TypeError "Variable expected, found function-like")

let apply_fun (st : state) (fn : ide) =
  match (topenv st) fn with
  | Fun (paraml, cmd, _, ret) -> paraml, cmd, ret
  | _ -> raise (TypeError "Function expected")

type conf = St of state | Cmd of cmd * state
type item = conf * string
type dconf = DSt of state | Dec of decl * state