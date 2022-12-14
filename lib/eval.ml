open Ast
open Utils

(* Free and non bound variables in a lambda term *)
let rec get_free_nb_vars (lt : lterm) : string list =
  match lt with
  | Cst _ -> []
  | Var x -> [x]
  | App (t1, t2) -> get_free_nb_vars t1 @ get_free_nb_vars t2
  | Abs (x, t) -> List.filter (fun y -> x <> y) (get_free_nb_vars t)
  | Ifz (tcond, t1, t2) -> get_free_nb_vars tcond @ get_free_nb_vars t1 @ get_free_nb_vars t2
  | Ife (tcond, t1, t2) -> get_free_nb_vars tcond @ get_free_nb_vars t1 @ get_free_nb_vars t2
  | Let (x, t1, t2) -> get_free_nb_vars t1 @ (List.filter (fun y -> x <> y) (get_free_nb_vars t2))
  | Pair (t1, t2) -> get_free_nb_vars t1 @ get_free_nb_vars t2

(* Substitute v occurences by m in lt *)
let rec substitute (lt : lterm) (v : string) (m : lterm) : lterm =
  match lt with
  | Cst _ -> lt
  | Var x -> if x = v then m else lt
  | App (t1,t2) -> App (substitute t1 v m, substitute t2 v m)
  | Abs (x,t) ->
      if x = v then Abs (x,lt)
      else if not (List.mem x (get_free_nb_vars m)) then Abs (x, substitute t v m)
      else
	      let nv = new_eval_var () in
	      let t' = substitute t x (Var nv) in
	        Abs (nv, substitute t' v m)
  | Ifz (tcond, t1, t2) -> Ifz (substitute tcond v m, substitute t1 v m, substitute t2 v m)
  | Ife (tcond, t1, t2) -> Ife (substitute tcond v m, substitute t1 v m, substitute t2 v m)
  | Let (x, t1, t2) ->
      if x = v then Let (x, substitute t1 v m, t2)
      else Let (x, substitute t1 v m, t2)
  | Pair (t1,t2) -> Pair (substitute t1 v m, substitute t2 v m)

(* Lambda term reduction *)
let rec reduce (lt : lterm) : lterm =
  match lt with
  | Cst _ -> lt
  | Var _ -> lt
  | App (App (Cst (Cop "+"), Cst (Cnat n1)), Cst (Cnat n2)) -> Cst (Cnat (n1+n2))
  | App (App (Cst (Cop "-"), Cst (Cnat n1)), Cst (Cnat n2)) -> Cst (Cnat (n1-n2))
  | App (Cst (Cop "not"), Cst (Cbool b)) -> Cst (Cbool (not b))
  | App (Cst (Cop "hd"), App( App( Cst (Cop "::"), Cst c), _)) -> Cst c
  | App (Cst (Cop "tl"), App( App( Cst (Cop "::"), _), t)) -> t
  | App (Abs (x,t1), t2) -> substitute t1 x t2
  | App (t1,t2) -> let t1' = reduce t1 in App (t1',reduce t2)
  | Abs (x,t) -> Abs (x, reduce t)
  | Ifz (tcond, t1, t2) ->
      let tc = reduce tcond in
        if tc = (Cst (Cnat 0)) then reduce t2
        else reduce t1
  | Ife (tcond, t1, t2) ->
      let tc = reduce tcond in
        if tc = (Cst (Cop "[]")) then reduce t1
        else reduce t2
  | Let (x, t1, t2) -> substitute t2 x t1
  | Pair (t1,t2) -> let t1' = reduce t1 in Pair (t1',reduce t2)

(* Lambda term evaluation *)
let eval (lt : lterm) : lterm =
  let rec eval_loop (lt : lterm) (timeout : int) : lterm =
    if (timeout < 100) then
      eval_loop (reduce lt) (timeout+1)
    else
      lt
  in
    eval_loop lt 1
