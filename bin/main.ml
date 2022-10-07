open Lib.Typer
open Lib.Ast

(* ***EXEMPLES*** *)  
let ex_nat : lterm = Cst (Cnat 42)
let inf_ex_nat : string = inference ex_nat
let ex_id : lterm = Abs ("x", Var "x")
let inf_ex_id : string = inference ex_id
let ex_k : lterm = Abs ("x", Abs ("y", Var "x"))
let inf_ex_k : string = inference ex_k
let ex_s : lterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s
let ex_nat1 : lterm = App (Abs ("x", Add(Var "x", Cst (Cnat 1))), Cst (Cnat 3))
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : lterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : lterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : lterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3

let main () =
  print_newline ();
  print_endline inf_ex_nat;
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
  print_endline inf_ex_nat3

let _ = main ()
