open Ast

(* Typing environment *)
type env = (string * typ) list

(* Equation list *)
type equa = (typ * typ) list

(* Term pretty printer *)
let rec print_term (t : lterm) : string =
  match t with
  | Cst (Cnat n) -> string_of_int n
  | Cst (Cbool b) -> string_of_bool b
  | Var x -> x
  | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
  | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
  | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"

(* Type pretty printer *)
let rec print_type (t : typ) : string =
  match t with
  | Bool -> "Boolean"
  | Nat -> "Nat"
  | Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^ " -> " ^ (print_type t2) ^ ")"

(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0

let nouvelle_var () : string =
  compteur_var := !compteur_var + 1;
  "T" ^ (string_of_int !compteur_var)

exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)
let rec cherche_type (v : string) (e : env) : typ =
  match e with
  | [] -> raise VarPasTrouve
  | (v1, t1)::_ when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q) 

(* vérificateur d'occurence de variables *)  
let rec appartient_type (v : string) (t : typ) : bool =
  match t with
  | Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2) 
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : typ) (v : string) (t0 : typ) : typ =
  match t with
  | Bool -> Bool
  | Nat -> Nat
  | Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0)

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : typ) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

(* genere des equations de typage à partir d'un terme *)  
let rec genere_equa (te : lterm) (ty : typ) (e : env) : equa =
  match te with 
  | Var v -> let tv : typ = cherche_type v e in [(ty, tv)]
  | App (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2))::(genere_equa t (Var nv2) ((x, Var nv1)::e))  
  | Cst (Cnat _) -> [(ty, Nat)]
  | Cst (Cbool _) -> [(ty, Bool)]
  | Add (t1, t2) ->
      let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in (ty, Nat)::(eq1 @ eq2)

exception Echec_unif of string      

(* zipper d'une liste d'équations *)
type equa_zip = equa * equa
  
(* rembobine le zipper *)
let rembobine (e : equa_zip) =
  match e with
    ([], _) -> e
  | (c::e1, e2) -> (e1, c::e2)

(* remplace unee variable par un type dans un zipper d'équations *)
let substitue_type_zip (e : equa_zip) (v : string) (t0 : typ) : equa_zip =
  match e with
    (e1, e2) -> (substitue_type_partout e1 v t0, substitue_type_partout e2 v t0)

(* trouve un type associé à une variable dans un zipper d'équation *)
let rec trouve_but (e : equa_zip) (but : string) = 
  match e with
    (_, []) -> raise VarPasTrouve
  | (_, (Var v, t)::_) when v = but -> t
  | (_, (t, Var v)::_) when v = but -> t 
  | (e1, c::e2) -> trouve_but (c::e1, e2) but 
                     
(* résout un système d'équations *) 
let rec unification (e : equa_zip) (but : string) : typ = 
  match e with 
    (* on a passé toutes les équations : succes *)
    (_, []) -> (try trouve_but (rembobine e) but with VarPasTrouve -> raise (Echec_unif "but pas trouvé"))
    (* equation avec but : on passe *)
  | (e1, (Var v1, t2)::e2) when v1 = but ->  unification ((Var v1, t2)::e1, e2) but
    (* deux variables : remplacer l'une par l'autre *)
  | (e1, (Var v1, Var v2)::e2) ->  unification (substitue_type_zip (rembobine (e1,e2)) v2 (Var v1)) but
    (* une variable à gauche : vérification d'occurence puis remplacement *)
  | (e1, (Var v1, t2)::e2) ->  if appartient_type v1 t2 then raise (Echec_unif ("occurence de "^ v1 ^" dans "^(print_type t2))) else  unification (substitue_type_zip (rembobine (e1,e2)) v1 t2) but
    (* une variable à droite : vérification d'occurence puis remplacement *)
  | (e1, (t1, Var v2)::e2) ->  if appartient_type v2 t1 then raise (Echec_unif ("occurence de "^ v2 ^" dans " ^(print_type t1))) else  unification (substitue_type_zip (rembobine (e1,e2)) v2 t1) but 
    (* types fleche des deux cotes : on decompose  *)
  | (e1, (Arr (t1,t2), Arr (t3, t4))::e2) -> unification (e1, (t1, t3)::(t2, t4)::e2) but 
    (* types fleche à gauche pas à droite : echec  *)
  | (_, (Arr (_,_), t3)::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types fleche à droite pas à gauche : echec  *)
  | (_, (t3, Arr (_,_))::_) -> raise (Echec_unif ("type fleche non-unifiable avec "^(print_type t3)))     
    (* types nat des deux cotes : on passe *)
  | (e1, (Nat, Nat)::e2) -> unification (e1, e2) but
  | (e1, (Bool, Bool)::e2) -> unification (e1, e2) but
    (* types nat à gauche pas à droite : échec *)
  | (_, (Nat, t3)::_) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))
    (* types à droite pas à gauche : échec *)
  | (_, (t3, Nat)::_) -> raise (Echec_unif ("type entier non-unifiable avec "^(print_type t3)))
                                       
(* enchaine generation d'equation et unification *)                                   
let inference (t : lterm) : string =
  let e : equa_zip = ([], genere_equa t (Var "but") []) in
  try (let res = unification e "but" in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type res))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla
