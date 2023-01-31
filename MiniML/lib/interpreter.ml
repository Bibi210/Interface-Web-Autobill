open Ast

(* FMT Valeur *)

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

let rec eval_expr a =
  match a with
  | VerifiedTree.Const c -> Const c
  | VerifiedTree.Tuple t -> Tuple (Array.map eval_expr t)
  | VerifiedTree.Seq b -> Helpers.array_getlast (Array.map eval_expr b)
  | VerifiedTree.Nil -> List []
  | VerifiedTree.Cons { hd; tail } ->
    let tail =
      match eval_expr tail with
      | List x -> x
      | _ -> raise (Invalid_argument "Cons without a list")
    in
    List (eval_expr hd :: tail)
;;

let eval_prog = eval_expr
