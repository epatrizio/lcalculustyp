type constant =
  | Cbool of bool
  | Cnat of int

type unop =
  | Unot (* not e *)

type binop =
  | Add
  | Sub

(* Lambda terms *)
type lterm =
  | Cst of constant
  | Var of string
  | App of lterm * lterm
  | Abs of string * lterm
  | Unop of unop * lterm
  | Binop of binop * lterm * lterm

(* Types *) 
type typ =
  | Bool
  | Nat
  | Var of string
  | Arr of typ * typ
