type constant =
  | Cbool of bool
  | Cnat of int
  | Cop of string

(* Lambda terms *)
type lterm =
  | Cst of constant
  | Var of string
  | App of lterm * lterm
  | Abs of string * lterm

(* Types *) 
type typ =
  | Bool
  | Nat
  | Var of string
  | Arr of typ * typ
  | List of typ

(* Operation types *)
module OperationTypeMap = Map.Make(String)
let op_type_map = OperationTypeMap.empty
let op_type_map = OperationTypeMap.add "+" (Arr (Nat, Arr (Nat, Nat))) op_type_map
let op_type_map = OperationTypeMap.add "-" (Arr (Nat, Arr (Nat, Nat))) op_type_map
let op_type_map = OperationTypeMap.add "not" (Arr (Bool, Bool)) op_type_map
let op_type_map = OperationTypeMap.add "[]" (List (Var "a")) op_type_map
let op_type_map = OperationTypeMap.add "::" (Arr (Var "a", Arr (List (Var "a"), List (Var "a")))) op_type_map
let op_type_map = OperationTypeMap.add "hd" (Arr (List (Var "a"), Var "a")) op_type_map
let op_type_map = OperationTypeMap.add "tl" (Arr (List (Var "a"), List (Var "a"))) op_type_map
