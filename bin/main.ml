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
      printf "[Typing result] - %s : %s@." (print_term lt) (print_type res);
      printf "[Eval result] %s@." (print_term_eval (eval lt)) (* eval only if typing ok *)
  with
  | OperationNotFound err -> eprintf "\x1b[31m[Typing ERROR -- Operation not found]\x1b[0m %s@." err
  | UnifyError err -> eprintf "\x1b[31m[Typing ERROR -- impossible to unify]\x1b[0m %s : %s@." (print_term lt) err
  | VarNotFound -> eprintf "\x1b[31m[Typing ERROR -- Unbound variable]\x1b[0m %s@." (print_term lt)
  | NotImplemented err -> eprintf "\x1b[33m[Eval WARNING -- Not implemented]\x1b[0m %s@." err

let _ = List.map lterms_process (List.rev lterms)