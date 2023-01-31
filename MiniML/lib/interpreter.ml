open Ast
module Env = Map.Make (String)

type value =
  | Const of const
  | Tuple of value array
  | List of value list

let rec fmt_value = function
  | Const c -> Ast.fmt_const c
  | List expr_ls ->
    "[ " ^ List.fold_left (fun acc expr -> acc ^ fmt_value expr ^ " ") "" expr_ls ^ "]"
  | Tuple expr_array ->
    "[ "
    ^ Array.fold_left (fun acc expr -> acc ^ fmt_value expr ^ " ") "" expr_array
    ^ "]"
;;

let rec eval_expr env a =
  match a with
  | VerifiedTree.Const c -> Const c
  | VerifiedTree.Tuple t -> Tuple (Array.map (eval_expr env) t)
  | VerifiedTree.Seq b -> Helpers.array_getlast (Array.map (eval_expr env) b)
  | VerifiedTree.Nil -> List []
  | VerifiedTree.Cons { hd; tail } ->
    let tail =
      match eval_expr env tail with
      | List x -> x
      | _ -> raise (Invalid_argument "Cons without a list")
    in
    List (eval_expr env hd :: tail)
  | VerifiedTree.Var v -> Env.find v.var_name env
  | VerifiedTree.Binding new_var ->
    let init_value = eval_expr env new_var.init in
    let body_env = Env.add new_var.var_name init_value env in
    eval_expr body_env new_var.content
;;

let eval_prog = eval_expr Env.empty
