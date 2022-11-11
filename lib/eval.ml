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
  | Ife (_, _, _) -> raise (NotImplemented "Ife")
  | Let (x, t1, t2) -> get_free_nb_vars t1 @ (List.filter (fun y -> x <> y) (get_free_nb_vars t2))

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
  | Ife (_, _, _) -> raise (NotImplemented "Ife")
  | Let (x, t1, t2) ->
      if x = v then Let (x, substitute t1 v m, t2)
      else Let (x, substitute t1 v m, t2)
      
let rec reduce (lt : lterm) : lterm =
  match lt with
  | Cst _ -> lt
  | Var _ -> lt
  | App (Abs (x,t1), t2) -> substitute t1 x t2
  | App (t1,t2) ->
      let t1' = reduce t1 in
        if t1' != t1 then App (t1',t2)
        else App (t1,reduce t2)
  | Abs (x,t) -> Abs (x, reduce t)
  | Ifz (tcond, t1, t2) ->
      let tc = reduce tcond in
        if tc = (Cst (Cnat 0)) then reduce t2
        else reduce t1
  | Ife (_, _, _) -> raise (NotImplemented "Ife")
  | Let (x, t1, t2) -> substitute t2 x t1
