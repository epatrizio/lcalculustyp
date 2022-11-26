open Ast
open Utils

(* Typing environment *)
type env = (string * typ) list

(* Equation list *)
type equa = (typ * typ) list

(* exceptions *)
exception VarNotFound
exception UnifyError of string
exception OperationNotFound of string

(* Type pretty printer *)
let rec print_type (t : typ) : string =
  match t with
  | Bool -> "Boolean"
  | Nat -> "Nat"
  | Unit -> "Unit"
  | Var x -> x
  | Arr (t1, t2) -> "(" ^ print_type t1 ^ " -> " ^ print_type t2 ^ ")"
  | List t -> print_type t ^ " list"
  | Ref t -> print_type t ^ " ref"
  | Product (t1, t2) -> "(" ^ print_type t1 ^ " * " ^ print_type t2 ^ ")"

(* Search variable type in an environment *)
let rec search_type (v : string) (e : env) : typ =
  match e with
  | [] -> raise VarNotFound
  | (v1, t1)::_ when v1 = v -> t1
  | (_, _):: q -> (search_type v q) 

(* Variable type occurrence checker *)
let rec var_type_check (v : string) (t : typ) : bool =
  match t with
  | Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (var_type_check v t1) || (var_type_check v t2)
  | List t -> var_type_check v t
  | Ref t -> var_type_check v t
  | Product (t1, t2) -> (var_type_check v t1) || (var_type_check v t2)
  | _ -> false

(* Substitue a variable with a type in a type *)
let rec substitue_type (t : typ) (v : string) (t0 : typ) : typ =
  match t with
  | Bool -> Bool
  | Nat -> Nat
  | Unit -> Unit
  | Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0)
  | List t1 -> List (substitue_type t1 v t0)
  | Ref t1 -> Ref (substitue_type t1 v t0)
  | Product (t1, t2) -> Product (substitue_type t1 v t0, substitue_type t2 v t0)

(* Substitue a variable by a type in a list of equations *)
let substitue_type_eq (e : equa) (v : string) (t0 : typ) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(* Generate typing equations from a lambda term *)
let rec generate_equa (lt : lterm) (ty : typ) (e : env) : equa =
  match lt with 
  | Var v -> let tv : typ = search_type v e in [(ty, tv)]
  | App (t1, t2) ->
      let nv : string = new_type_var () in
      let eq1 : equa = generate_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = generate_equa t2 (Var nv) e in
        eq1 @ eq2
  | Abs (x, t) ->
      let nv1 : string = new_type_var () and nv2 : string = new_type_var () in
        (ty, Arr (Var nv1, Var nv2))::(generate_equa t (Var nv2) ((x, Var nv1)::e))
  | Cst (Cnat _) -> [(ty, Nat)]
  | Cst (Cbool _) -> [(ty, Bool)]
  | Cst (Cunit) -> [(ty, Unit)]
  | Cst (Cop sop) ->
      (try
        let typ = OperationTypeMap.find sop op_type_map in [(ty, typ)]
      with Not_found -> raise (OperationNotFound sop))
  | Ifz (tcond, t1, t2) ->
      let nvc : string = new_type_var () and nv : string = new_type_var () in
      let eqc : equa = generate_equa tcond (Var nvc) e in
      let eq1 : equa = generate_equa t1 (Var nv) e in
      let eq2 : equa = generate_equa t2 (Var nv) e in
        (Nat, Var nvc)::(ty, Var nv)::eqc @ eq1 @ eq2
  | Ife (tcond, t1, t2) ->
      let nvc : string = new_type_var () and nvl : string = new_type_var () and nv : string = new_type_var () in
      let eqc : equa = generate_equa tcond (Var nvc) e in
      let eq1 : equa = generate_equa t1 (Var nv) e in
      let eq2 : equa = generate_equa t2 (Var nv) e in
        (List (Var nvl), Var nvc)::(ty, Var nv)::eqc @ eq1 @ eq2
  | Let (x, t1, t2) ->
      let nv1 : string = new_type_var () and nv2 : string = new_type_var () in
      let eq1 : equa = generate_equa t1 (Var nv1) e in
      let eq2 : equa = generate_equa t2 (Var nv2) ((x, Var nv1)::e) in
        [(ty, Var nv2)] @ eq1 @ eq2
  | Pair (t1, t2) ->
      let nv1 : string = new_type_var () and nv2 : string = new_type_var () in
      let eq1 : equa = generate_equa t1 (Var nv1) e in
      let eq2 : equa = generate_equa t2 (Var nv2) e in
        [(ty, Product (Var nv1, Var nv2))] @ eq1 @ eq2

(* Equation list zipper *)
type equa_zip = equa * equa

(* Rewind zipper *)
let rewind_zip (e : equa_zip) =
  match e with
  | ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* Substitue a variable by a type in an equation list zipper *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : typ) : equa_zip =
  match e with
  | (e1, e2) -> (substitue_type_eq e1 v t0, substitue_type_eq e2 v t0)

(* Find var type in an equation list zipper *)
let rec find_goal (e : equa_zip) (goal : string) = 
  match e with
  | (_, []) -> raise VarNotFound
  | (_, (Var v, t)::_) when v = goal -> t
  | (_, (t, Var v)::_) when v = goal -> t 
  | (e1, c::e2) -> find_goal (c::e1, e2) goal

(* Unification: solves an equations system (equation list zipper) *)
let rec unify (e : equa_zip) (goal : string) : typ = 
  match e with 
    (* all equations are passed: success! *)
  | (_, []) -> (try find_goal (rewind_zip e) goal with VarNotFound -> raise (UnifyError "goal not find"))
    (* equation with goal: continue *)
  | (e1, (Var v1, t2)::e2) when v1 = goal -> unify ((Var v1, t2)::e1, e2) goal
    (* 2 vars: replace one by the other *)
  | (e1, (Var v1, Var v2)::e2) -> unify (substitue_type_zip (rewind_zip (e1,e2)) v2 (Var v1)) goal
    (* var on left : check then substitute *)
  | (e1, (Var v1, t2)::e2) ->
      if var_type_check v1 t2 then raise (UnifyError (v1 ^ " occurence in " ^ print_type t2))
      else unify (substitue_type_zip (rewind_zip (e1,e2)) v1 t2) goal
    (* var on right : check then substitute *)
  | (e1, (t1, Var v2)::e2) -> 
      if var_type_check v2 t1 then raise (UnifyError (v2 ^ " occurence in " ^ print_type t1))
      else unify (substitue_type_zip (rewind_zip (e1,e2)) v2 t1) goal
    (* types arrow on both sides : we decomposition *)
  | (e1, (Arr (t1, t2), Arr (t3, t4))::e2) -> unify (e1, (t1, t3)::(t2, t4)::e2) goal
    (* types arrow on left not on right : error *)
  | (_, (Arr (_,_), t)::_) -> raise (UnifyError ("Arrow type not unifiable with " ^ print_type t))
    (* types arrow on right not on left : error *)
  | (_, (t, Arr (_,_))::_) -> raise (UnifyError ("Arrow type not unifiable with " ^ print_type t))
    (* List type *)
  | (e1, (List t1, List t2)::e2) -> unify (e1, (t1, t2)::e2) goal
  | (_, (t, List _)::_) -> raise (UnifyError ("List type not unifiable with " ^ print_type t))
  | (_, (List _, t)::_) -> raise (UnifyError ("List type not unifiable with " ^ print_type t))
    (* Constant types *)
  | (e1, (Nat, Nat)::e2) -> unify (e1, e2) goal
  | (e1, (Bool, Bool)::e2) -> unify (e1, e2) goal
  | (e1, (Unit, Unit)::e2) -> unify (e1, e2) goal
    (* Constant types not equal on both sides : error *)
  | (_, (Nat, t)::_) -> raise (UnifyError ("Nat type not unifiable with " ^ print_type t))
  | (_, (t, Nat)::_) -> raise (UnifyError ("Nat type not unifiable with " ^ print_type t))
  | (_, (Bool, t)::_) -> raise (UnifyError ("Boolean type not unifiable with " ^ print_type t))
  | (_, (t, Bool)::_) -> raise (UnifyError ("Boolean type not unifiable with " ^ print_type t))
    (* Ref type *)
  | (e1, (Ref t1, Ref t2)::e2) -> unify (e1, (t1, t2)::e2) goal
  | (_, (Ref _, t)::_) -> raise (UnifyError ("Ref type not unifiable with " ^ print_type t))
  | (_, (t, Ref _)::_) -> raise (UnifyError ("Ref type not unifiable with " ^ print_type t))
    (* Product type *)
  | (e1, (Product (t1,t2), Product (t3, t4))::e2) -> unify (e1, (t1, t3)::(t2, t4)::e2) goal
  | (_, (Product (_,_), t)::_) -> raise (UnifyError ("Product type not unifiable with " ^ print_type t))
  | (_, (t, Product (_,_))::_) -> raise (UnifyError ("Product type not unifiable with " ^ print_type t))
