open Intern_common
open Intern_prelude
open Intern_prog

let internalize_prelude prog =
  let env = initial_sortcheck () in
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
      -> true
    | _ -> false in
  let prog = List.filter is_not_prelude prog in
    (prog, env)

let string_of_intern_ast prog =
  Intern_prettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let internalize prog =
  let prog = PrimitivePrelude.with_primitives prog in
  let prog, env = internalize_prelude prog in
  let prog, env = intern_prog env prog in
  prog, env
