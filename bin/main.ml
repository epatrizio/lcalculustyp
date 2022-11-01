open Format

open Lterms

open Lib.Typer
open Lib.Ast

let inference (lt : lterm) : unit =
  try
    let eq : equa_zip = ([], genere_equa lt (Var "goal") []) in
    let res = unification eq "goal" in
      printf "[Typing result] %s :: %s@." (print_term lt) (print_type res)
  with
  | OperationNotFound err -> eprintf "[Typing ERROR -- Operation not found] %s@." err
  | UnifyError err -> eprintf "[Typing ERROR -- impossible to unify] %s : %s@." (print_term lt) err
  | VarNotFound -> eprintf "[Typing ERROR -- Unbound variable] %s@." (print_term lt)

let rec lterms_process lterms_test =
  match lterms_test with
  | [] -> ()
  | lt::r ->
      inference lt;
      lterms_process r

let _ = lterms_process (List.rev lterms)
