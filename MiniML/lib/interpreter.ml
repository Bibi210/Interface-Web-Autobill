open Ast

let eval_const a = a

let rec eval_expr a =
  match a with
  | VerifiedTree.Const c -> VerifiedTree.Const c
  | VerifiedTree.Tuple t -> VerifiedTree.Tuple (List.map eval_expr t)
  | VerifiedTree.Block b -> Helpers.getlast (List.map eval_expr b)
  | VerifiedTree.Nil -> VerifiedTree.Nil
  | VerifiedTree.Cons { hd; tail } ->
    VerifiedTree.Cons { hd = eval_expr hd; tail = eval_expr tail }
;;

let eval_prog = eval_expr
