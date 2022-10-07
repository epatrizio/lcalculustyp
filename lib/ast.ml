type constant =
  | Cbool of bool
  | Cnat of int

(* Lambda terms *)
type lterm =
  | Cst of constant
  | Var of string
  | App of lterm * lterm
  | Abs of string * lterm
  | Add of lterm * lterm

(* Types *) 
type typ =
  | Bool
  | Nat
  | Var of string
  | Arr of typ * typ
