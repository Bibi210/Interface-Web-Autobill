open Helpers

type value =
  | Integer of int
  | Boolean of bool

type expr =
  | Value of
      { value : value
      ; loc : position
      }
  | Tuple of
      { value : expr list
      ; loc : position
      }

type prog = expr list

let fmt_value = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
;;

let rec fmt_expr acc e =
  (match e with
  | Value v ->
    Printf.sprintf "Value(%s,Pos = %s)" (fmt_value v.value) (string_of_position v.loc)
  | Tuple t ->
    Printf.sprintf
      "Tuple((%s),Pos = %s)"
      (print_expr_list t.value)
      (string_of_position t.loc))
  ^ acc

and print_expr_list expr_ls =
  List.fold_right (fun ls acc -> " , " ^ fmt_expr acc ls) expr_ls ""
;;

let fmt_program prog = List.fold_right (fun ls acc -> "\n" ^ fmt_expr acc ls) prog ""
