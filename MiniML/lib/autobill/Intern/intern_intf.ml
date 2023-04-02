open Preprocess_ast
open Intern_common
open Intern_prelude
open Intern_prog

let internalize_prelude prog =
  let env = initial_env () in
  let env = internalize_all_sortvar env prog in
  let env = internalize_all_typcons env prog in
  let env = List.fold_left
      sort_check_one_item
      env
      prog in
  let is_not_prelude = function
    | Cst.Term_definition _
    | Cst.Cmd_execution _
    | Cst.Term_declaration _
    | Cst.Goal_selection _
      -> true
    | _ -> false in
  let prog = List.filter is_not_prelude prog in
    (prog, env)

let string_of_intern_ast prog =
  pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let internalize prog =
  try
    let prog = PrimitivePrelude.with_primitives prog in
    let prog, env = internalize_prelude prog in
    let prog = intern_prog env prog in
    prog
  with
  | Double_definition (info, loc) ->
    Misc.fatal_error "Internalization" ~loc info
  | Bad_sort (info, loc) ->
    Misc.fatal_error "Syntactic analysis" ~loc info
  | Undefined_identifier (info, loc) ->
    Misc.fatal_error "Internaliaztion" ~loc ("the " ^ info ^ "is undefined")
  | Invalid_Goal string ->
    Misc.fatal_error "Internalization" ("Invalid goal: " ^ string)
