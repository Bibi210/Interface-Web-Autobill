open Ast
open Helpers
module Env = Map.Make (String)

type value =
  | Const of const
  | Tuple of value array
  | List of value list
  | Lambda of (value list -> value)

let rec eval_expr env a =
  match a with
  | VerifiedTree.Const c -> Const c
  | VerifiedTree.Tuple t -> Tuple (Array.map (eval_expr env) t)
  | VerifiedTree.Seq b -> array_getlast (Array.map (eval_expr env) b)
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
  | VerifiedTree.Lambda lambda ->
    Lambda
      (fun value_ls ->
        eval_expr
          (List.fold_left2
             (fun acc arg_name arg_value -> Env.add arg_name arg_value acc)
             env (* * Fixed Closure *)
             lambda.args
             value_ls)
          lambda.body)
  | VerifiedTree.Call funcall ->
    let lambda = eval_expr env funcall.func in
    let args = List.map (eval_expr env) funcall.args in
    (match lambda with
    | Lambda func -> func args
    | _ -> raise (Invalid_argument "Call with no lambda"))
;;

let eval_prog = eval_expr Env.empty
