open Ast
open Types

let runtime_log = Buffer.create 100;;
let print = "println!";;

let parse (s : ide) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.parse_string Lexer.read lexbuf in
  ast
;;

let parse_string (str : string) =
  let length = String.length str in
  if Char.equal (String.get str 0) '"' then
    if Char.equal (String.get str length) '"' then
      String.sub (String.sub str length length) 0 length
    else raise (ParserError "Wrong parse_string usage")
  else raise (ParserError "Wrong parse_string usage")
;;

let wrap_const_var (st : state) (x : ide) : expr =
  let rec dereference loc = (match getmem st loc with
  | Int n -> IntConst(n)
  | Bool b -> BoolConst(b)
  | String s -> StringConst s
  | IntRef loc | StringRef loc  -> dereference loc)
in match apply st x with
  | IntRef loc | StringRef loc ->
    dereference loc
  | Int n -> IntConst(n)
  | Bool b -> BoolConst(b)
  | String s -> StringConst s
;;

let rec transform_paraml dlist elist =
  let transform = function
    | IntVar (x, _), e -> IntVar(x, e)
    | BIntVar (x, _), e -> BIntVar(x, e)
    | GenericVar (x, _), e -> GenericVar(x, e)
    | BGenericVar (x, _), e -> BGenericVar(x, e)
    | _ -> raise (ParserError "ParserError")
  in
  match dlist, elist with
    | d::decl, e::exprl -> transform (d, e) :: (transform_paraml decl exprl)
    | [], [] -> []
    | _, _ -> raise (ParserError "ParserError")

let rec string_of_memval st = function
| IntRef loc | StringRef loc -> string_of_memval st (getmem st loc)
| Int int -> string_of_int int
| String string -> string
| Bool bool -> string_of_bool bool

let rec build_block (decl : decl list) (cmd : cmd) =
  List.fold_left(fun c d -> (Decl(d, c))) cmd (List.rev decl)
;;

let rec trace1_expr (st : state) = function
  | Var(x) -> (wrap_const_var st x, st)
  | Not (BoolConst b) -> (BoolConst(not b), st)
  | Not e -> let (e', st') = trace1_expr st e in (Not(e'), st')
  | And(BoolConst(true), e2) -> (e2, st)
  | And(BoolConst(false), _) -> (BoolConst(false), st)
  | And(e1, e2) -> let (e1',st') = trace1_expr st e1 in (And(e1', e2), st')
  | Or(BoolConst(true), _) -> (BoolConst(true), st)
  | Or(BoolConst(false), e2) -> (e2, st)
  | Or(e1, e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1', e2), st')
  (* integer operations *)
  | Add(IntConst(n1), IntConst(n2)) -> (IntConst(n1+n2), st)
  | Add(IntConst(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Add(IntConst(n1), e2'), st')
  | Add(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Add(e1', e2), st')
  | Sub(IntConst(n1), IntConst(n2)) -> (IntConst(n1-n2), st)
  | Sub(IntConst(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Sub(IntConst(n1), e2'), st')
  | Sub(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Sub(e1', e2), st')
  | Mul(IntConst(n1), IntConst(n2)) -> (IntConst(n1*n2), st)
  | Mul(IntConst(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Mul(IntConst(n1), e2'), st')
  | Mul(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Mul(e1', e2), st')
  | Eq(IntConst(n1), IntConst(n2)) -> if n1=n2 then (BoolConst(true), st) else (BoolConst(false), st)
  | Eq(IntConst(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Eq(IntConst(n1), e2'), st')
  | Eq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Eq(e1', e2), st')
  | Leq(IntConst(n1), IntConst(n2)) -> if n1<=n2 then (BoolConst(true), st) else (BoolConst(false), st)
  | Leq(IntConst(n1), e2) -> let (e2', st') = trace1_expr st e2 in (Leq(IntConst(n1), e2'), st')
  | Leq(e1, e2) -> let (e1', st') = trace1_expr st e1 in (Leq(e1', e2), st')
  (* functions/procedures *)
  | Call(fn, [Var(x)]) when fn = print ->
    let () = Buffer.add_string runtime_log (string_of_memval st (apply st x)) in
    let () = Buffer.add_string runtime_log "\n" in
    raise NoRuleApplies
  | Call(fn, exprl) ->
    let decl, cmd, ret = (apply_fun st fn) in
    let decl = transform_paraml decl exprl in
    let fn_block = build_block decl cmd in
    CallExec(fn_block, ret), st
  | CallExec(cmd, ret) ->
    (match trace1_cmd (Cmd(cmd, st)) with
      St st' -> (CallRet(ret), st')
      | Cmd(cmd', st') -> (CallExec(cmd', ret), st'))
  | CallRet(IntConst n) -> let st' = setenv st (popenv st) in (IntConst n, st')
  | CallRet(StringConst s) -> let st' = setenv st (popenv st) in (StringConst s, st')
  | CallRet(e) -> let (e', st') = trace1_expr st e in (CallRet(e'), st')
  | _ -> raise NoRuleApplies
  (*scrapped:
  | Call(fn, FloatConst f :: exprl) ->
    (try
      match (topenv st) (pop_param_id st fn) with
        | Imm (loc, FloatType, _) -> (Call(fn, exprl), (setmem st (bind_mem (getmem st) loc (Float f))))
        | _ -> raise (TypeError "Type mismatch on function call")
      with Stack.Empty -> raise (FunError "Function has no parameters, one or more paramteres were passed"))
  | Call(fn, CharConst c :: exprl) ->
    (try
      match (topenv st) (pop_param_id st fn) with
        | Imm (loc, CharType, _) -> (Call(fn, exprl), (setmem st (bind_mem (getmem st) loc (Char c))))
        | _ -> raise (TypeError "Type mismatch on function call")
      with Stack.Empty -> raise (FunError "Function has no parameters, one or more paramteres were passed"))
  | Call(fn, BoolConst b :: exprl) ->
    (try
      match (topenv st) (pop_param_id st fn) with
        | Imm (loc, BoolType, _) -> (Call(fn, exprl), (setmem st (bind_mem (getmem st) loc (Bool b))))
        | _ -> raise (TypeError "Type mismatch on function call")
      with Stack.Empty -> raise (FunError "Function has no parameters, one or more paramteres were passed"))
  | CallRet(FloatConst f) -> let st' = setenv st (popenv st) in (FloatConst(f), st')
  | CallRet(CharConst c) -> let st' = setenv st (popenv st) in (CharConst(c), st')
  | CallRet(BoolConst b) -> let st' = setenv st (popenv st) in (BoolConst(b), st')
  *)
(* raises UnboundVarException and NoRulesApplies*)
and trace1_decl (st : state) = function
  (* gestione ownership *)
  (* immutable *)
  | GenericVar (id, IntConst n) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, IntType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (Int n) in
    DSt (make_state (pushenv st env') mem' loc')
  | GenericVar (id, StringConst s) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, StringType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (String s) in
    DSt (make_state (pushenv st env') mem' loc')
  | GenericVar (id, Var(x)) ->
    (match (topenv st) x with
      | Fun (_, _, _, _) | Proc (_, _)-> raise (VarError "Borrowing from function")
      | Mut (_, IntType, _) | Imm (_, IntType, _) ->  let e', st' = trace1_expr st (Var(x)) in Dec (GenericVar(id, e'), st')
      | var ->
        let loc' = getloc st in
        (* aggiorno la ownership di var *)
        let loc, var_type, var = (match var with
          | Mut(loc, t, _) -> loc, t, Mut(loc, t, Moved)
          | Imm(loc, t, _) -> loc, t, Imm(loc, t, Moved)
          | _ -> raise (VarError "Borrowing from function"))
        in
        let env = bind_env (topenv st) x var in
          (match var_type with
            | StringType ->
              let env' = bind_env env id (Imm(loc', StringType, Owner)) in
              let mem' = bind_mem (getmem st) loc' (StringRef loc) in
              DSt (setloc (setmem (setenv st (env'::(popenv st))) mem') (loc' + 1))
            | _ -> raise (ParserError "Parser error")))
  (* mutable *)
  | MGenericVar (id, IntConst n) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, IntType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (Int n) in
    DSt (make_state (pushenv st env') mem' loc')
  | MGenericVar (id, StringConst s) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, StringType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (String s) in
    DSt (make_state (pushenv st env') mem' loc')
  | MGenericVar (id, Var(x)) ->
    (match (topenv st) x with
      | Fun (_, _, _, _, _ ) | Proc (_, _, _)-> raise (VarError "Borrowing from function")
      | Mut (_, IntType, _) | Imm (_, IntType, _) ->  let e', st' = trace1_expr st (Var(x)) in Dec (MGenericVar(id, e'), st')
      | var ->
        let loc' = getloc st in
        (* aggiorno la ownership di var *)
        let loc, var_type, var = (match var with
          | Mut(loc, t, _) -> loc, t, Mut(loc, t, Moved)
          | Imm(loc, t, _) -> loc, t, Imm(loc, t, Moved)
          | _ -> raise (VarError "Borrowing from function"))
        in
        let env = bind_env (topenv st) x var in
          (match var_type with
            | StringType ->
              let env' = bind_env env id (Mut(loc', StringType, Owner)) in
              let mem' = bind_mem (getmem st) loc' (StringRef loc) in
              DSt (setloc (setmem (setenv st (env'::(popenv st))) mem') (loc' + 1))
            | _ -> raise (ParserError "Parser error")))
  (* gestione borrowing *)
  | BGenericVar (id, Var(x)) -> (* immutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let (t' : types), memvar = (match t with IntType -> IntRef, IntRef loc | StringType -> StringRef, StringRef loc | _ -> raise (ParserError "ParserError")) 
    in
    let var' = Imm(loc', t', Owner)
    in
    let var_list =
      (match b with
        | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Borrowed (var_list) -> var_list @ [var']
        | Owner -> [var'])
      in
      let var = (match var with (* riscrivo var *)
        | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
        | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
        | _ -> raise (ParserError "ParserError"))
      in (* re-binding di var nell'env *)
      let env = bind_env (topenv st) x var in
      (* bind della reference a var *)
      let env' = bind_env env id var' in
      (* aggiornamento dello stato *)
      let loc' = (loc' + 1) in
      let mem' = bind_mem (getmem st) loc' memvar in
      DSt (make_state (pushenv st env') mem' loc')
  | BMGenericVar (id, Var(x)) -> (* mutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let (t' : types), memvar = (match t with IntType -> IntRef, IntRef loc | StringType -> StringRef, StringRef loc | _ -> raise (ParserError "ParserError")) 
    in
    let var' = Imm(loc', t', Owner)
    in
    let var_list =
      (match b with
        | Borrowed [] -> raise (ParserError "ParserError")
        | Borrowed (var''::_) | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Owner -> [var'])
      in
      let var = (match var with (* riscrivo var *)
        | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
        | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
        | _ -> raise (ParserError "ParserError"))
      in (* re-binding di var nell'env *)
      let env = bind_env (topenv st) x var in
      (* bind della reference a var *)
      let env' = bind_env env id var' in
      (* aggiornamento dello stato *)
      let loc' = (loc' + 1) in
      let mem' = bind_mem (getmem st) loc' memvar in
      DSt (make_state (pushenv st env') mem' loc')
  | MBGenericVar (id, Var(x)) -> (* mutable var immutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let (t' : types), memvar = (match t with IntType -> IntRef, IntRef loc | StringType -> StringRef, StringRef loc | _ -> raise (ParserError "ParserError")) 
    in
    let var' = Mut(loc', t', Owner)
    in
    let var_list =
      (match b with
        | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Borrowed (var_list) -> var_list @ [var']
        | Owner -> [var'])
      in
      let var = (match var with (* riscrivo var *)
        | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
        | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
        | _ -> raise (ParserError "ParserError"))
      in (* re-binding di var nell'env *)
      let env = bind_env (topenv st) x var in
      (* bind della reference a var *)
      let env' = bind_env env id var' in
      (* aggiornamento dello stato *)
      let loc' = (loc' + 1) in
      let mem' = bind_mem (getmem st) loc' memvar in
      DSt (make_state (pushenv st env') mem' loc')
  | MBMGenericVar (id, Var(x)) -> (* mutable var mutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let (t' : types), memvar = (match t with IntType -> IntRef, IntRef loc | StringType -> StringRef, StringRef loc | _ -> raise (ParserError "ParserError")) 
    in
    let var' = Mut(loc', t', Owner)
    in
    let var_list =
      (match b with
        | Borrowed [] -> raise (ParserError "ParserError")
        | Borrowed (var''::_) | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Owner -> [var'])
      in
      let var = (match var with (* riscrivo var *)
        | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
        | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
        | _ -> raise (ParserError "ParserError"))
      in (* re-binding di var nell'env *)
      let env = bind_env (topenv st) x var in
      (* bind della reference a var *)
      let env' = bind_env env id var' in
      (* aggiornamento dello stato *)
      let loc' = (loc' + 1) in
      let mem' = bind_mem (getmem st) loc' memvar in
      DSt (make_state (pushenv st env') mem' loc')
  (* gestione ownership *)
  | IntVar (id, IntConst n) -> (* immutable *)
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, IntType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (Int n) in
    DSt (make_state (pushenv st env') mem' loc')
  | MIntVar (id, IntConst n) -> (* mutable *)
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, IntType, Owner)) in
    let mem' = bind_mem (getmem st) loc' (Int n) in
    DSt (make_state (pushenv st env') mem' loc')
  (* gestione borrowing *)
  | BIntVar (id, Var(x)) -> (* immutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let var', memval = Imm(loc', IntRef, Owner), (IntRef loc)
    in
    let var_list =
      (match b with
        | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Borrowed (var_list) -> var_list @ [var']
        | Owner -> [var'])
    in
    let var = (match var with (* riscrivo var *)
      | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
      | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
      | _ -> raise (ParserError "ParserError"))
    in (* re-binding di var nell'env *)
    let env = bind_env (topenv st) x var in
    (match t with
    | IntType ->
      let loc' = loc' + 1 in
      let env' = bind_env env id var' in
      let mem' = bind_mem (getmem st) loc' memval in
      DSt (make_state (pushenv st env') mem' loc')
    | _ -> raise (TypeError "Reference type error"))
  | BMIntVar (id, Var(x)) -> (* mutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let var', memval = Imm(loc', IntRef, Owner), (IntRef loc)
    in
    let var_list =
      (match b with
        | Borrowed [] -> raise (ParserError "ParserError")
        | Borrowed (var''::_) | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Owner -> [var'])
    in
    let var = (match var with (* riscrivo var *)
      | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
      | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
      | _ -> raise (ParserError "ParserError"))
    in (* re-binding di var nell'env *)
    let env = bind_env (topenv st) x var in
    (match t with
    | IntType ->
      let loc' = loc' + 1 in
      let env' = bind_env env id var' in
      let mem' = bind_mem (getmem st) loc' memval in
      DSt (make_state (pushenv st env') mem' loc')
    | _ -> raise (TypeError "Reference type error"))
  | MBIntVar (id, Var(x)) -> (* mutable var immutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let var', memval = Mut(loc', IntRef, Owner), (IntRef loc)
    in
    let var_list =
      (match b with
        | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Borrowed (var_list) -> var_list @ [var']
        | Owner -> [var'])
    in
    let var = (match var with (* riscrivo var *)
      | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
      | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
      | _ -> raise (ParserError "ParserError"))
    in (* re-binding di var nell'env *)
    let env = bind_env (topenv st) x var in
    (match t with
    | IntType ->
      let loc' = loc' + 1 in
      let env' = bind_env env id var' in
      let mem' = bind_mem (getmem st) loc' memval in
      DSt (make_state (pushenv st env') mem' loc')
    | _ -> raise (TypeError "Reference type error"))
  | MBMIntVar (id, Var(x)) -> (* mutable var mutable borrow *)
    let loc' = getloc st in
    let var = (topenv st) x in 
    let (loc, t, b) = (match var with
      | Mut(loc, t, b) | Imm(loc, t, b) -> loc, t, b
      | _ -> raise (VarError "Borrowing from function"))
    in
    let var', memval = Mut(loc', IntRef, Owner), (IntRef loc)
    in
    let var_list =
      (match b with
        | Borrowed [] -> raise (ParserError "ParserError")
        | Borrowed (var''::_) | MutableBorrowed var'' -> raise(MultipleBorrow (x, var', var''))
        | Moved -> raise (OwnershipError x)
        (* aggiungo var' alla lista di variabili che hanno fatto il borrow di var *)
        | Owner -> [var'])
    in
    let var = (match var with (* riscrivo var *)
      | Mut(loc, t, _) -> Mut(loc, t, Borrowed (var_list))
      | Imm(loc, t, _) -> Imm(loc, t, Borrowed (var_list))
      | _ -> raise (ParserError "ParserError"))
    in (* re-binding di var nell'env *)
    let env = bind_env (topenv st) x var in
    (match t with
    | IntType ->
      let loc' = loc' + 1 in
      let env' = bind_env env id var' in
      let mem' = bind_mem (getmem st) loc' memval in
      DSt (make_state (pushenv st env') mem' loc')
    | _ -> raise (TypeError "Reference type error"))
  (* progressione trace *)
  | GenericVar (id, e) -> let e', st' = trace1_expr st e in Dec (GenericVar(id, e'), st')
  | MGenericVar (id, e) -> let e', st' = trace1_expr st e in Dec (MGenericVar(id, e'), st')
  | BGenericVar (id, e) -> let e', st' = trace1_expr st e in Dec (BGenericVar(id, e'), st')
  | BMGenericVar (id, e) -> let e', st' = trace1_expr st e in Dec (BMGenericVar(id, e'), st')
  | MBMGenericVar (id, e) -> let e', st' = trace1_expr st e in Dec (MBMGenericVar(id, e'), st')
  | IntVar (id, e) -> let e', st' = trace1_expr st e in Dec (IntVar(id, e'), st')
  | MIntVar (id, e) -> let e', st' = trace1_expr st e in Dec (MIntVar(id, e'), st')
  | BIntVar (id, e) -> let e', st' = trace1_expr st e in Dec (BIntVar(id, e'), st')
  | BMIntVar (id, e) -> let e', st' = trace1_expr st e in Dec (BMIntVar(id, e'), st')
  | MBMIntVar (id, e) -> let e', st' = trace1_expr st e in Dec (MBMIntVar(id, e'), st')
  (* functions and procedures *)
  | Fun(fn, paraml, t, c, ret) -> DSt (bind_fun st fn paraml c t ret)
  | Proc(fn, paraml, c) -> DSt (bind_proc st fn paraml c)
  | _ -> raise (ParserError "Not implemented.")
  (* 
  *** scrapped idea ***
  | FloatVar (id, FloatConst f) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, FloatType)) in
    let mem' = bind_mem (getmem st) loc' (Float f) in
    DSt (make_state (pushenv st env') mem' loc')
  | FloatVar (id, e) -> let e', st' = trace1_expr st e in Dec (FloatVar(id, e'), st')
  | MFloatVar (id, FloatConst f) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, FloatType)) in
    let mem' = bind_mem (getmem st) loc' (Float f) in
    DSt (make_state (pushenv st env') mem' loc')
  | MFloatVar (id, e) -> let e', st' = trace1_expr st e in Dec (MFloatVar(id, e'), st')
  (* bool variables *)
  | BoolVar (id, BoolConst b) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, BoolType)) in
    let mem' = bind_mem (getmem st) loc' (Bool b) in
    DSt (make_state (pushenv st env') mem' loc')
  | BoolVar (id, e) -> let e', st' = trace1_expr st e in Dec (BoolVar(id, e'), st')
  | MBoolVar (id, BoolConst b) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, BoolType)) in
    let mem' = bind_mem (getmem st) loc' (Bool b) in
    DSt (make_state (pushenv st env') mem' loc')
  | MBoolVar (id, e) -> let e', st' = trace1_expr st e in Dec (MBoolVar(id, e'), st')
  (* char variables *)
  | CharVar (id, CharConst c) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Imm (loc, CharType)) in
    let mem' = bind_mem (getmem st) loc' (Char c) in
    DSt (make_state (pushenv st env') mem' loc')
  | CharVar (id, e) -> let e', st' = trace1_expr st e in Dec (CharVar(id, e'), st')
  | MCharVar (id, CharConst c) ->
    let loc = getloc st in let loc' = loc + 1 in
    let env' = bind_env (topenv st) id (Mut (loc, CharType)) in
    let mem' = bind_mem (getmem st) loc' (Char c) in
    DSt (make_state (pushenv st env') mem' loc')
  | MCharVar (id, e) -> let e', st' = trace1_expr st e in Dec (MCharVar(id, e'), st')
  *)

and trace1_cmd = function
  | St _ -> raise NoRuleApplies
  | Cmd (Skip, st) -> St st
  | Cmd (Assign(x, IntConst(n)), st) -> St (bind_mut_var st x (Int n))
  | Cmd (Assign(x, StringConst(s)), st) -> St (bind_mut_var st x (String s))
  | Cmd (Assign(x, BoolConst(b)), st) -> St (bind_mut_var st x (Bool b))
  | Cmd (Assign(x, e), st) -> let e', st' = trace1_expr st e in Cmd (Assign(x, e'), st')
  | Cmd (Seq(c1, c2), st) ->
    (match (trace1_cmd (Cmd (c1, st))) with
    | Cmd(Break, st') -> St st' (* drop del lato destro della sequenza all'interno di un determinato blocco *)
    | St st' -> Cmd (c2, st')
    | Cmd(c1', st') -> Cmd(Seq(c1', c2), st'))
  | Cmd(If(BoolConst(true), c, _), st) -> Cmd(c, st)
  | Cmd(If(BoolConst(false), _, c), st) -> Cmd(c, st)
  | Cmd(If(e, c1, c2), st) -> let e', st' = trace1_expr st e in Cmd(If(e', c1, c2), st')
  | Cmd (Break, st) -> St(make_state (popenv st) (getmem st) (getloc st))
  | Cmd(Loop(c), st) -> Cmd(Seq(c, Loop(c)), st) (* riscrive i loop come "esecuzione codice -> loop di codice" *)
  | Cmd(Block c, st) ->
    (match trace1_cmd (Cmd(c, st)) with
      St st' -> St(make_state (popenv st') (getmem st') (getloc st'))
      | Cmd(c', st') -> Cmd(Block(c'), st'))
  | Cmd(Decl(d, c), st) ->
    (match trace1_decl st d with
        | DSt st' -> Cmd(Block(c), st')
        | Dec(d', st') -> Cmd(Block(Decl(d', c)), st')
        )
;;

let rec trace_rec n t =
  if n <= 0 then [t]
  else
    try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with 
    | NoRuleApplies -> [t]

let trace_prog n c =
  trace_rec n (Cmd(c, state0)), (Buffer.contents runtime_log)
