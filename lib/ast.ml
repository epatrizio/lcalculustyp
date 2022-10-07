type constant =
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
  | Var of string
  | Arr of typ * typ
  | Nat 
