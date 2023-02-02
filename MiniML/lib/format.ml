open Ast
open Interpreter

let array_sprintf array fmt_func separator =
  let acc = ref "" in
  for i = 0 to Array.length array - 2 do
    acc := !acc ^ fmt_func (Array.get array i) ^ separator
  done;
  !acc ^ fmt_func (Helpers.array_getlast array)
;;

let list_sprintf array fmt_func separator =
  let rec fold_left accu l =
    match l with
    | [] -> accu
    | [ elem ] -> accu ^ fmt_func elem
    | elem :: l -> fold_left (accu ^ fmt_func elem ^ separator) l
  in
  fold_left "" array
;;

let fmt_const = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
;;

let rec fmt_types = function
  | Int_t -> "Int"
  | Bool_t -> "Bool"
  | Tuple types -> Printf.sprintf "Tuple of (%s)" (array_sprintf types fmt_types " ")
  | WeakType -> "WeakType"
  | List elem_type -> Printf.sprintf "List of (%s)" (fmt_types elem_type)
  | Lambda (args_type, return_type) ->
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
;;

(*   | VerifiedTree.Call func_call ->
    Printf.sprintf
      "Call(%s on [%s])"
      (fmt_expr func_call.func)
      (array_sprintf func_call.args fmt_expr ",") *)

let rec fmt_value = function
  | Const c -> fmt_const c
  | List expr_ls -> "[" ^ list_sprintf expr_ls fmt_value " " ^ "]"
  | Tuple expr_array -> "(" ^ array_sprintf expr_array fmt_value "," ^ ")"
  | Lambda _func -> "Lambda()"
;;
