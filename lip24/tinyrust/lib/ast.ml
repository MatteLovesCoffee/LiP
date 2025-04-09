
type ide = string

type types =
  | IntType
  | StringType
  | IntRef
  | StringRef
  (* scrapped:
  | FloatType
  | BoolType
  | CharType
  | FloatRef
  | BoolRef
  | CharRef
  *)

type param = ide * types;;

type expr =
  | Uninit
  | Var of ide
  | Mut of ide
  | TVar of ide * types
  | TMut of ide * types
  | IntConst of int
  | FloatConst of float
  | BoolConst of bool
  | CharConst of char
  | StringConst of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Call of ide * expr list
  | CallExec of cmd * expr
  | CallRet of expr

and cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | Loop of cmd
  | Break
  | Decl of decl * cmd
  | Block of cmd

and decl =
  | IntVar of ide * expr (* immutable var *)
  | GenericVar of ide * expr (* immutable var *)
  | BIntVar of ide * expr (* immutable borrowing *)
  | BGenericVar of ide * expr (* immutable borrowing *)
  | MBIntVar of ide * expr (* mutable var immutable borrowing *)
  | MBGenericVar of ide * expr (* mutable var immutable borrowing *)
  | MIntVar of ide * expr (* mutable var *)
  | MGenericVar of ide * expr (* mutable var *)
  | BMIntVar of ide * expr (* mutable borrowing *)
  | BMGenericVar of ide * expr (* mutable borrowing *)
  | MBMIntVar of ide * expr (* mutable var mutable borrowing *)
  | MBMGenericVar of ide * expr (* mutable var mutable borrowing *)
  | Fun of ide * decl list * types * cmd * expr  (** name, parameter(s), return type, body command, return expr *)
  | Proc of ide * decl list * cmd  (** name, parameter(s), body command *)
  (* scrapped idea
  | BoolVar of ide * expr (* immutable var *)
  | FloatVar of ide * expr (* immutable var *)
  | CharVar of ide * expr (* immutable var *)
  | BBoolVar of ide * expr (* immutable var borrowing *)
  | BFloatVar of ide * expr (* immutable var borrowing *)
  | BCharVar of ide * expr (* immutable var borrowing *)
  | MBoolVar of ide * expr (* mutable var *)
  | MFloatVar of ide * expr (* mutable var *)
  | MCharVar of ide * expr (* mutable var *)
  | BMBoolVar of ide * expr (* mutable var borrowing *)
  | BMFloatVar of ide * expr (* mutable var borrowing *)
  | BMCharVar of ide * expr (* mutable var borrowing *)
  *)