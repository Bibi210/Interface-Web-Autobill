open Preprocess_ast
open SortInfer
open Sort_export
open Misc

let polarity_inference (prog : Preprocess_ast.program) =
  try
    let env = initial_sort_check_env prog.prelude in
    let env = unify_prog env prog in
    export_ast env prog
  with
  | Sort_mismatch (so1, so2, loc1, loc2) ->
    let mess = Printf.sprintf
        "the sort %s of the thing at position %s doesn't match with\nthe sort %s at position %s"
        so2 (string_of_position loc2) so1 (string_of_position loc1) in
    Misc.fatal_error "Polarity inference" ~loc:loc2 mess
  | Ambiguous_polarity loc ->
    Misc.fatal_error "Polarity inference" ~loc "Could not guess the polarity here. Please annotate."
