open Ast
open Interpreter

let list_sprintf list fmt_func separator =
  String.concat separator (List.map fmt_func list)
;;

let array_sprintf array fmt_func separator =
  list_sprintf (List.of_seq (Array.to_seq array)) fmt_func separator
;;

let fmt_const = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
;;

let rec fmt_types = function
  | Int_t -> "Int"
  | Bool_t -> "Bool"
  | Tuple_t types -> Printf.sprintf "Tuple of (%s)" (array_sprintf types fmt_types " ")
  | Weak_t -> "WeakType"
  | List_t elem_type -> Printf.sprintf "List of (%s)" (fmt_types elem_type)
  | Lambda_t (args_type, return_type) ->
    Printf.sprintf
      "Function [%s] -> %s"
      (list_sprintf args_type fmt_types " ")
      (fmt_types return_type)
;;

let rec fmt_expr = function
  | VerifiedTree.Const const -> Printf.sprintf "%s" (fmt_const const)
  | VerifiedTree.Tuple expr_ls ->
    Printf.sprintf "Tuple(%s)" (array_sprintf expr_ls fmt_expr ",")
  | Seq expr_ls -> Printf.sprintf "Seq(%s)" (array_sprintf expr_ls fmt_expr ";")
  | VerifiedTree.Cons hd_tail ->
    Printf.sprintf
      "Cons(hd = %s, tail = %s)"
      (fmt_expr hd_tail.hd)
      (fmt_expr hd_tail.tail)
  | VerifiedTree.Nil -> Printf.sprintf "Nil"
  | VerifiedTree.Binding { var_name; init; content } ->
    Printf.sprintf "Binding(%s = %s in %s)" var_name (fmt_expr init) (fmt_expr content)
  | VerifiedTree.Var { var_name } -> Printf.sprintf "Var(%s)" var_name
  | VerifiedTree.Lambda lambda ->
    Printf.sprintf
      "Lambda %s -> (%s)"
      (list_sprintf lambda.args (fun a -> a) " ")
      (fmt_expr lambda.body)
  | VerifiedTree.Call funcall ->
    Printf.sprintf
      "Apply %s with (%s)"
      (fmt_expr funcall.func)
      (list_sprintf funcall.args fmt_expr " ")
;;

let rec fmt_value = function
  | Const c -> fmt_const c
  | List expr_ls -> "[" ^ list_sprintf expr_ls fmt_value " " ^ "]"
  | Tuple expr_array -> "(" ^ array_sprintf expr_array fmt_value "," ^ ")"
  | Lambda _func -> "Lambda()"
;;
