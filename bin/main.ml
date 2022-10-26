open Lib.Typer
open Lib.Ast

(* examples *)

let ex_nat : lterm = Cst (Cnat 42)
let inf_ex_nat : string = inference ex_nat
let ex_bool : lterm = Cst (Cbool true)
let inf_ex_bool : string = inference ex_bool

(* not *)
let ex_not : lterm = Cst (Cop "not")
let inf_ex_not : string = inference ex_not
let ex_not_1 : lterm = App (Cst (Cop "not"), Cst (Cbool true))
let inf_ex_not_1 : string = inference ex_not_1
let ex_not_ko : lterm = App (Cst (Cop "not"), Cst (Cnat 42))
let inf_ex_not_ko : string = inference ex_not_ko

(* + *)
let ex_add : lterm = Cst (Cop "+")
let inf_ex_add : string = inference ex_add
(* + 42 *)
let ex_add_1 : lterm = App (Cst (Cop "+"), Cst (Cnat 42))
let inf_ex_add_1 : string = inference ex_add_1
(* + 40 2 *)
let ex_add_2 : lterm = App( App (Cst (Cop "+"), Cst (Cnat 40)), Cst (Cnat 2))
let inf_ex_add_2 : string = inference ex_add_2
(* + ko *)
let ex_add_ko_1 : lterm = App (Cst (Cop "+"), Cst (Cbool false))
let inf_ex_add_ko_1 : string = inference ex_add_ko_1
let ex_add_ko_2 : lterm = App( App (Cst (Cop "+"), Cst (Cnat 42)), Cst (Cbool false))
let inf_ex_add_ko_2 : string = inference ex_add_ko_2
let ex_add_ko_3 : lterm = App( App (Cst (Cop "+"), Cst (Cbool false)), Cst (Cnat 42))
let inf_ex_add_ko_3 : string = inference ex_add_ko_3

(* - *)
let ex_sub : lterm = Cst (Cop "-")
let inf_ex_sub : string = inference ex_sub
(* - 44 2 *)
let ex_sub_1 : lterm = App( App (Cst (Cop "-"), Cst (Cnat 44)), Cst (Cnat 2))
let inf_ex_sub_1 : string = inference ex_sub_1

let ex_id : lterm = Abs ("x", Var "x")
let inf_ex_id : string = inference ex_id
let ex_k : lterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k : string = inference ex_k
let ex_s : lterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s
let ex_nat1 : lterm = App (Abs ("x", App( App (Cst (Cop "+"), Var "x"), Cst (Cnat 1))), Cst (Cnat 3))
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : lterm = Abs ("x", App( App (Cst (Cop "+"), Var "x"), Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : lterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : lterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3

(* lists *)
let ex_list_empty : lterm = Cst (Cop "[]")
let inf_ex_list_empty : string = inference ex_list_empty
let ex_list_cons : lterm = Cst (Cop "::")
let inf_ex_list_cons : string = inference ex_list_cons
let ex_list_1 : lterm = App( App (Cst (Cop "::"), Cst (Cbool true)), Cst (Cop "[]"))
let inf_ex_list_1 : string = inference ex_list_1
let ex_list_2 : lterm = App( App (Cst (Cop "::"), Cst (Cnat 42)), Cst (Cop "[]"))
let inf_ex_list_2 : string = inference ex_list_2
let ex_list_3 : lterm =   (* 2::40::[] *)
  App(
    App (
      Cst (Cop "::"), Cst (Cnat 2)
    ),
    App(
      App (
        Cst (Cop "::"), Cst (Cnat 40)
      ),
      Cst (Cop "[]")
    )    
  )
let inf_ex_list_3 : string = inference ex_list_3
let ex_list_4 : lterm =   (* 42::false::[] > KO *)
  App(
    App (
      Cst (Cop "::"), Cst (Cnat 42)
    ),
    App(
      App (
        Cst (Cop "::"), Cst (Cbool false)
      ),
      Cst (Cop "[]")
    )    
  )
let inf_ex_list_4 : string = inference ex_list_4

let ex_list_hd_1 : lterm = Cst (Cop "hd")
let inf_ex_list_hd_1 : string = inference ex_list_hd_1
let ex_list_hd_2 : lterm = App (Cst (Cop "hd"), Cst (Cop "[]"))
let inf_ex_list_hd_2 : string = inference ex_list_hd_2
let ex_list_hd_3 : lterm = App (Cst (Cop "hd"), Cst (Cnat 42))
let inf_ex_list_hd_3 : string = inference ex_list_hd_3
let ex_list_hd_4 : lterm = App (Cst (Cop "hd"), ex_list_3)
let inf_ex_list_hd_4 : string = inference ex_list_hd_4
let ex_list_hd_5 : lterm = App (Cst (Cop "hd"), ex_list_4)
let inf_ex_list_hd_5 : string = inference ex_list_hd_5

let main () =
  print_newline ();
  print_endline inf_ex_nat;
  print_endline "======================";
  print_endline inf_ex_bool;
  print_endline "======================";

  print_endline inf_ex_not;
  print_endline "======================";
  print_endline inf_ex_not_1;
  print_endline "======================";
  print_endline inf_ex_not_ko;
  print_endline "======================";

  print_endline inf_ex_add;
  print_endline "======================";
  print_endline inf_ex_add_1;
  print_endline "======================";
  print_endline inf_ex_add_2;
  print_endline "======================";
  print_endline inf_ex_add_ko_1;
  print_endline "======================";
  print_endline inf_ex_add_ko_2;
  print_endline "======================";
  print_endline inf_ex_add_ko_3;
  print_endline "======================";

  print_endline inf_ex_sub;
  print_endline "======================";
  print_endline inf_ex_sub_1;
  print_endline "======================";

  print_endline inf_ex_id;
  print_endline "======================";
  print_endline inf_ex_k;
  print_endline "======================";
  print_endline inf_ex_s;
  print_endline "======================";
  print_endline inf_ex_omega;
  print_endline "======================";
  print_endline inf_ex_nat1;
  print_endline "======================";
  print_endline inf_ex_nat2;
  print_endline "======================";
  print_endline inf_ex_nat3;
  print_endline "======================";
  print_endline inf_ex_list_empty;
  print_endline "======================";
  print_endline inf_ex_list_cons;
  print_endline "======================";
  print_endline inf_ex_list_1;
  print_endline "======================";
  print_endline inf_ex_list_2;
  print_endline "======================";
  print_endline inf_ex_list_3;
  print_endline "======================";
  print_endline inf_ex_list_4;
  print_endline "======================";
  print_endline inf_ex_list_hd_1;
  print_endline "======================";
  print_endline inf_ex_list_hd_2;
  print_endline "======================";
  print_endline inf_ex_list_hd_3;
  print_endline "======================";
  print_endline inf_ex_list_hd_4;
  print_endline "======================";
  print_endline inf_ex_list_hd_5

let _ = main ()
