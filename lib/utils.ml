open Ast

exception NotImplemented of string

(* New var generator *)
let new_var (prefix : string) =
  let counter_var = ref 0 in
  fun () ->
    let ccv = !counter_var in
    incr counter_var;
    prefix ^ (string_of_int ccv)

let new_type_var = new_var "T"

let new_eval_var = new_var "V"

(* Lambda term pretty printer *)
let rec print_term (t : lterm) : string =
  match t with
  | Cst (Cnat n) -> string_of_int n
  | Cst (Cbool b) -> string_of_bool b
  | Cst (Cop sop) -> sop
  | Var x -> x
  | App (t1, t2) -> "(" ^ print_term t1 ^ " " ^ print_term t2 ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ print_term t ^ ")"
  | Ifz (tcond, t1, t2) -> "ife " ^ print_term tcond ^ " then " ^ print_term t1 ^ " else " ^ print_term t2
  | Ife (tcond, t1, t2) -> "ifz " ^ print_term tcond ^ " then " ^ print_term t1 ^ " else " ^ print_term t2