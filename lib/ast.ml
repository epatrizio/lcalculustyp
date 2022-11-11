type constant =
  | Cbool of bool
  | Cnat of int
  | Cunit
  | Cop of string

(* Lambda terms *)
type lterm =
  | Cst of constant
  | Var of string
  | App of lterm * lterm
  | Abs of string * lterm
  | Ifz of lterm * lterm * lterm
  | Ife of lterm * lterm * lterm
  | Let of string * lterm * lterm

(* Types *)
type typ =
  | Bool
  | Nat
  | Unit
  | Var of string
  | Arr of typ * typ
  | List of typ
  | Ref of typ

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
let op_type_map = OperationTypeMap.add "fix" (Arr (Arr (Var "a", Var "a"), Var "a")) op_type_map
let op_type_map = OperationTypeMap.add "ref" (Arr (Var "a", Ref (Var "a"))) op_type_map
let op_type_map = OperationTypeMap.add "!" (Arr (Ref (Var "a"), Var "a")) op_type_map
let op_type_map = OperationTypeMap.add ":=" (Arr (Ref (Var "a"), Arr (Var "a", Unit))) op_type_map
