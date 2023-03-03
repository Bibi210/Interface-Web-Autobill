open Misc
open Intern_common

let polarity_inference ?trace:(trace=false) env prog =

  let debug = if trace then Some Format.std_formatter else None in

  let env =
    List.fold_left (SortInfer.unify_def ?debug) env prog in
  let rec aux env acc = function
    | [] -> env.prelude, List.rev acc
    | h::t ->
      let def,env = Sort_export.export_ast env h in
      aux env (def::acc) t in
  aux env [] prog

let intern_error_wrapper f =
  let wrap ?loc str = begin
    let loc = match loc with
      | None -> ""
      | Some loc ->"(" ^ string_of_position loc ^ ")" in
    print_endline ("FATAL" ^ loc ^ ": " ^ str);
    exit 1 end in
  try f () with

  | Intern_common.Ambiguous_polarity loc ->
    wrap ("ambiguous polarity at " ^ (string_of_position loc))

  | Intern_common.Double_definition name ->
    wrap ("the name " ^ name ^ " is defined twice")

  | Intern_common.Bad_sort {loc; actual; expected} ->
    wrap  ("conflicting sorts, expected "
               ^  expected
               ^ ", got "
               ^  actual
               ^ " at " ^ loc)

  | Intern_common.Undefined_type {name; loc} ->
    wrap ~loc ("The type " ^ name ^ "is undefined")

  | Intern_common.Bad_type_cons_arity {cons; loc} ->
    wrap ~loc ("This type application has the wrong arity for" ^ cons)

  | Intern_common.Bad_constructor_name {loc} ->
    wrap ~loc ("This constructor/destructor name is reserved")

  | Intern_common.Higher_order_type_argument {loc; name} ->
    wrap ~loc ("Unsupported: the type argument "
               ^ name
               ^ " has a higher-order sort")

  | Intern_common.Undefined_var (name, loc) ->
    wrap ~loc ("Undefined variable " ^ name)

  | Intern_common.Undefined_constructor (name, loc) ->
    wrap ~loc ("Undefined constructor " ^ name)

  | Intern_common.Undefined_destructor (name, loc) ->
    wrap ~loc ("Undefined destructor " ^ name)

  | Intern_common.Sort_mismatch (pol1, pol2, loc1, loc2) ->
    wrap ("The polarities of expressions "
         ^ pol1 ^ " at " ^ string_of_position loc1
         ^ " and "
         ^ pol2 ^ " at " ^ string_of_position loc2
         ^ "disagree")
