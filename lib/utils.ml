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
  let rec print_list (lt : lterm) (sep : bool) =
    match lt with
    | Cst (Cop "[]") -> ""
    | App (App (Cst (Cop "::"), t1), t2) -> (if sep then "; " else "") ^ print_term t1 ^ print_list t2 true
    | t -> "; -error-" ^ print_term t
  in
  match t with
  | Cst (Cnat n) -> string_of_int n
  | Cst (Cbool b) -> string_of_bool b
  | Cst (Cunit) -> "()"
  | Cst (Cop sop) -> sop
  | Var x -> x
  | App (App (Cst (Cop "::"), t1), t2) -> "[" ^ print_list (App (App (Cst (Cop "::"), t1), t2)) false ^ "]"
  | App (t1, t2) -> "(" ^ print_term t1 ^ " " ^ print_term t2 ^ ")"
  | Abs (x, t) -> "(fun " ^ x ^ " -> " ^ print_term t ^ ")"
  | Ifz (tcond, t1, t2) -> "ifz " ^ print_term tcond ^ " then " ^ print_term t1 ^ " else " ^ print_term t2
  | Ife (tcond, t1, t2) -> "ife " ^ print_term tcond ^ " then " ^ print_term t1 ^ " else " ^ print_term t2
  | Let (x, t1, t2) -> "let " ^ x ^ "=" ^ print_term t1 ^ " in " ^ print_term t2

(* Lambda term pretty printer - specific for evaluation *)
let print_term_eval (t : lterm) : string =
  match t with
  | App (Cst (Cop "ref"), t) -> "{contents = " ^ print_term t ^ "}"
  | App (Cst (Cop "!"), App (Cst (Cop "ref"), t)) -> print_term t
  | App (App (Cst (Cop ":="), _), _) -> "()"
  | t -> print_term t
