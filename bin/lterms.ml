open Lib.Ast

(* Lambda terms list construction for tests *)

let lterms = (Cst (Cnat 42))::[]
let lterms = (Cst (Cbool true))::lterms

let lterms = (Cst (Cop "not"))::lterms
let lterms = (App (Cst (Cop "not"), Cst (Cbool true)))::lterms
let lterms = (App (Cst (Cop "not"), App (Cst (Cop "not"), Cst (Cbool true))))::lterms
let lterms = (App (Cst (Cop "not"), Cst (Cnat 42)))::lterms

let lterms = (Cst (Cop "unknown_op"))::lterms

let ex_add : lterm =
  App (App (Cst (Cop "+"), Cst (Cnat 40)), Cst (Cnat 2))

let ex_min : lterm =
  App (App (Cst (Cop "-"), Cst (Cnat 42)), Cst (Cnat 42))

let lterms = (Cst (Cop "+"))::lterms
let lterms = (App (Cst (Cop "+"), Cst (Cnat 42)))::lterms
let lterms = (ex_add)::lterms
let lterms = (ex_min)::lterms
let lterms = (App (App (Cst (Cop "+"), Cst (Cnat 42)), ex_min))::lterms
let lterms = (App (App (Cst (Cop "+"), ex_min), Cst (Cnat 42)))::lterms

let lterms = (App (Cst (Cop "+"), Cst (Cbool false)))::lterms
let lterms = (App (App (Cst (Cop "+"), Cst (Cnat 42)), Cst (Cbool false)))::lterms
let lterms = (App (App (Cst (Cop "+"), Cst (Cbool false)), Cst (Cnat 42)))::lterms

let lterms = (Cst (Cop "-"))::lterms
let lterms = (App( App (Cst (Cop "-"), Cst (Cnat 44)), Cst (Cnat 2)))::lterms

let ex_nat : lterm = Abs ("x", App( App (Cst (Cop "+"), Var "x"), Var "x"))
let ex_id : lterm = Abs ("x", Var "x")

let lterms = (ex_id)::lterms
let lterms = (App (ex_id, Cst (Cnat 1)))::lterms
let lterms = (App (ex_id, Cst (Cbool true)))::lterms
let lterms = (Abs ("x", Var "y"))::lterms
let lterms = (Abs ("x", Abs ("y", Var "x")))::lterms
let lterms = (Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z"))))))::lterms
let lterms = (App (Abs ("x", App( App (Cst (Cop "+"), Var "x"), Cst (Cnat 1))), Cst (Cnat 3)))::lterms
let lterms = (ex_nat)::lterms
let lterms = (App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y"))))::lterms
let lterms = (App (ex_nat, ex_id))::lterms
let lterms = (App (ex_nat, ex_add))::lterms
let lterms = (App (Abs ("y", Var "y"), ex_nat))::lterms

let ex_list_1 : lterm =   (* 0::1::2::[] *)
  App(
    App (
      Cst (Cop "::"), Cst (Cnat 0)
    ),
    App(
      App (
        Cst (Cop "::"), Cst (Cnat 1)
      ),
      App(
        App (
          Cst (Cop "::"), Cst (Cnat 2)
        ),
        Cst (Cop "[]")
      )
    )
  )

let ex_list_2 : lterm =   (* 42::false::[] > KO *)
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

let ex_list_3 : lterm =   (* (40+2)::(42-42)::[] *)
  App(
    App (
      Cst (Cop "::"), ex_add
    ),
    App(
      App (
        Cst (Cop "::"), ex_min
      ),
      Cst (Cop "[]")
    )    
  )

let ex_list_4 : lterm =   (* list of lists ! *)
  App(
    App (
      Cst (Cop "::"), ex_list_1
    ),
    App(
      App (
        Cst (Cop "::"), ex_list_3
      ),
      Cst (Cop "[]")
    )    
  )

let lterms = (Cst (Cop "[]"))::lterms
let lterms = (Cst (Cop "::"))::lterms
let lterms = (App (App (Cst (Cop "::"), Cst (Cbool true)), Cst (Cop "[]")))::lterms
let lterms = (App (App (Cst (Cop "::"), Cst (Cnat 42)), Cst (Cop "[]")))::lterms
let lterms = (App (Cst (Cop "::"), Cst (Cnat 42)))::lterms
let lterms = (App (App (Cst (Cop "::"), Cst (Cnat 42)), Cst (Cnat 0)))::lterms
let lterms = (ex_list_1)::lterms
let lterms = (ex_list_2)::lterms
let lterms = (ex_list_3)::lterms
let lterms = (App (ex_id, ex_list_1))::lterms
let lterms = (App (ex_id, ex_list_3))::lterms
let lterms = (App (App (Cst (Cop "::"), ex_list_1), Cst (Cop "[]")))::lterms
let lterms = (ex_list_4)::lterms

let lterms = (Cst (Cop "hd"))::lterms
let lterms = (App (Cst (Cop "hd"), Cst (Cop "[]")))::lterms
let lterms = (App (Cst (Cop "hd"), Cst (Cnat 42)))::lterms
let lterms = (App (Cst (Cop "hd"), ex_list_1))::lterms
let lterms = (App (Cst (Cop "hd"), ex_list_2))::lterms
let lterms = (App (Cst (Cop "hd"), ex_list_3))::lterms
let lterms = (App (ex_nat, App (Cst (Cop "hd"), ex_list_3)))::lterms
let lterms = (Cst (Cop "tl"))::lterms
let lterms = (App (Cst (Cop "tl"), Cst (Cop "[]")))::lterms
let lterms = (App (Cst (Cop "tl"), Cst (Cnat 42)))::lterms
let lterms = (App (Cst (Cop "tl"), ex_list_1))::lterms
let lterms = (App (Cst (Cop "tl"), ex_list_2))::lterms
let lterms = (App (Cst (Cop "tl"), ex_list_3))::lterms

let lterms = (Ifz (Cst (Cnat 1), Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ifz (Cst (Cnat 0), Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ifz (Cst (Cnat 1), Cst (Cbool true), Cst (Cnat 0)))::lterms
let lterms = (Ifz (Abs ("x", Var "x"), Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ifz (ex_add, Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ifz (ex_min, Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ifz (Cst (Cnat 1), Abs ("x", Var "x"), Abs ("x", Abs ("y", Var "x"))))::lterms
let lterms = (Ifz (Cst (Cnat 1), Abs ("x", Var "x"), ex_nat))::lterms
let lterms = (Ifz (Cst (Cnat 1), ex_add, ex_min))::lterms
let lterms = (Ifz (Cst (Cnat 0), ex_add, ex_min))::lterms
let lterms = (Ifz (ex_add, ex_add, ex_min))::lterms
let lterms = (Ifz (ex_min, ex_add, ex_min))::lterms

let lterms = (Ife (Cst (Cop "[]"), Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ife (Cst (Cop "[]"), Cst (Cbool true), Cst (Cnat 0)))::lterms
let lterms = (Ife (Abs ("x", Var "x"), Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ife (ex_list_1, Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ife (Cst (Cop "[]"), Abs ("x", Var "x"), Abs ("x", Abs ("y", Var "x"))))::lterms
let lterms = (Ife (Cst (Cop "[]"), Abs ("x", Var "x"), ex_nat))::lterms
let lterms = (Ife (ex_list_2, Cst (Cbool true), Cst (Cbool false)))::lterms
let lterms = (Ife (ex_list_3, ex_add, ex_min))::lterms

let lterms = (Cst (Cop "fix"))::lterms

let ex_fun : lterm =   (* fun x -> ((+ x) 42) *)
  Let ("y", ex_add, Abs ("x", App( App (Cst (Cop "+"), Var "x"), Var "y")))

let lterms = (Let ("x", Cst (Cbool true), Cst (Cop "not")))::lterms
let lterms = (Let ("x", Cst (Cbool true), App (Cst (Cop "not"), Var "x")))::lterms
let lterms = (Let ("x", App (Cst (Cop "not"), Cst (Cbool true)), App (Cst (Cop "not"), Var "x")))::lterms
let lterms = (Let ("x", Cst (Cbool true), App (Cst (Cop "+"), Var "x")))::lterms
let lterms = (Let ("x", Abs ("x", Var "x"), App (Var "x", Cst (Cnat 42))))::lterms
let lterms = (Let ("x", App (Cst (Cop "+"), Cst (Cnat 40)), App (Var "x", Cst (Cbool false))))::lterms
let lterms = (Let ("x", Cst (Cop "+"), App (Var "x", Cst (Cnat 42))))::lterms
let lterms = (ex_fun)::lterms
let lterms = (App (ex_fun, Cst (Cnat 10)))::lterms
let lterms = (Let ("x", ex_list_3, App (Cst (Cop "hd"), Var "x")))::lterms
let lterms = (Let ("x", ex_list_3, App (Cst (Cop "tl"), Var "x")))::lterms

let lterms = (Cst (Cunit))::lterms

let lterms = (Cst (Cop "ref"))::lterms
let lterms = (App (Cst (Cop "ref"), Cst (Cnat 42)))::lterms
let lterms = (App (Cst (Cop "ref"), Cst (Cbool true)))::lterms
let lterms = (App (Cst (Cop "ref"), ex_add))::lterms
let lterms = (App (Cst (Cop "ref"), ex_list_1))::lterms

let lterms = (Cst (Cop "!"))::lterms
let lterms = (App (Cst (Cop "!"), App (Cst (Cop "ref"), Cst (Cnat 42))))::lterms
let lterms = (App (Cst (Cop "!"), App (Cst (Cop "ref"), ex_min)))::lterms
let lterms = (App (Cst (Cop "!"), Cst (Cnat 42)))::lterms

let lterms = (Cst (Cop ":="))::lterms
let lterms = (App (App (Cst (Cop ":="), App (Cst (Cop "ref"), Cst (Cbool true))), Cst (Cbool false)))::lterms
let lterms = (App (App (Cst (Cop ":="), App (Cst (Cop "ref"), Cst (Cbool true))), Cst (Cnat 42)))::lterms
let lterms = (App (App (Cst (Cop ":="), App (Cst (Cop "ref"), Cst (Cnat 0))), ex_add))::lterms
let lterms = (Let ("x", App (Cst (Cop "ref"), Cst (Cnat 0)), App (App (Cst (Cop ":="), Var "x"), Cst (Cnat 1))))::lterms
let lterms = (Let ("x", App (Cst (Cop "ref"), Cst (Cnat 0)), App (App (Cst (Cop ":="), Var "x"), Cst (Cbool false))))::lterms
let lterms = (Let ("x", App (Cst (Cop "ref"), ex_min), App (App (Cst (Cop ":="), Var "x"), Cst (Cnat 1))))::lterms
let lterms = (Let ("x", App (Cst (Cop "ref"), ex_min), App (App (Cst (Cop ":="), Var "x"), Cst (Cbool false))))::lterms

let lterms = (Pair (Cst (Cnat 0), Cst (Cnat 1)))::lterms
let lterms = (Pair (Cst (Cnat 0), Cst (Cbool true)))::lterms
let lterms = (Pair (App (Cst (Cop "not"), Cst (Cbool true)), ex_add))::lterms
let lterms = (Pair (Cst (Cop "[]"), Cst (Cnat 1)))::lterms
let lterms = (Pair (Cst (Cbool false), ex_list_1))::lterms
let lterms = (Pair (Cst (Cbool true), Pair (Cst (Cnat 0), Cst (Cnat 1))))::lterms
let lterms = (Pair (Pair (Cst (Cnat 0), Cst (Cnat 1)), Cst (Cbool true)))::lterms
let lterms = (
  Let ("x",App (Cst (Cop "ref"), Pair (Cst (Cnat 0), Cst (Cbool true))),
  App (App (Cst (Cop ":="), Var "x"), Pair (Cst (Cnat 1), Cst (Cbool false)))))::lterms
let lterms = (
  Let ("x",App (Cst (Cop "ref"), Pair (Cst (Cnat 0), Cst (Cbool true))),
  App (App (Cst (Cop ":="), Var "x"), Pair (Cst (Cbool false), Cst (Cnat 1)))))::lterms
let lterms = (
  Let ("x",App (Cst (Cop "ref"), Pair (Cst (Cnat 0), Cst (Cbool true))),
  App (App (Cst (Cop ":="), Var "x"), Cst (Cnat 1))))::lterms
let lterms = (
  Let ("x",App (Cst (Cop "ref"), Pair (Cst (Cbool true), Pair (Cst (Cnat 0), Cst (Cnat 1)))),
  App (App (Cst (Cop ":="), Var "x"), Pair (Cst (Cbool false), Pair (Cst (Cnat 1), Cst (Cnat 0))))))::lterms
let lterms = (
  Let ("x",App (Cst (Cop "ref"), Pair (Cst (Cbool true), Pair (Cst (Cnat 0), Cst (Cnat 1)))),
  App (App (Cst (Cop ":="), Var "x"), Pair (Pair (Cst (Cbool false), Cst (Cnat 0)), Cst (Cnat 1)))))::lterms
  