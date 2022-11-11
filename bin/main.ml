open Format

open Lterms

open Lib.Ast
open Lib.Eval
open Lib.Typer
open Lib.Utils

let lterms_process (lt : lterm) : unit =
  try
    let eq : equa_zip = ([], genere_equa lt (Var "goal") []) in
    let res = unification eq "goal" in (* inference *)
      printf "[Typing result] %s :: %s@." (print_term lt) (print_type res);
      printf "[Eval result] %s@." (print_term (reduce lt)) (* eval only if typing ok - beta *)
  with
  | OperationNotFound err -> eprintf "[Typing ERROR -- Operation not found] %s@." err
  | UnifyError err -> eprintf "[Typing ERROR -- impossible to unify] %s : %s@." (print_term lt) err
  | VarNotFound -> eprintf "[Typing ERROR -- Unbound variable] %s@." (print_term lt)
  | NotImplemented err -> eprintf "[Eval WARNING -- Not implemented] %s@." err

let _ = List.map lterms_process (List.rev lterms)