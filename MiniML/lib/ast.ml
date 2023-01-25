type const =
  | Integer of int
  | Boolean of bool

module VerifiedTree = struct
  type expr =
    | Const of const
    | Tuple of expr list
    | Block of expr list
    | Cons of
        { hd : expr
        ; tail : expr
        }
    | Nil

  type prog = expr
end

module Syntax = struct
  type expr =
    | Const of
        { const : const
        ; loc : Helpers.position
        }
    | Tuple of
        { content : expr list
        ; loc : Helpers.position
        }
    (*     | Binding of
        { varname : string
        ; content : expr
        ; loc : Helpers.position
        } *)
    | Block of
        { content : expr list
        ; loc : Helpers.position
        }
    | Cons of
        { hd : expr
        ; tail : expr
        ; loc : Helpers.position
        }
    | Nil of { loc : Helpers.position }

  type prog = expr
end

let fmt_const = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
;;

let rec fmt_expr = function
  | VerifiedTree.Const const -> Printf.sprintf "%s" (fmt_const const)
  | VerifiedTree.Tuple expr_ls ->
    Printf.sprintf
      "Tuple(%s)"
      (List.fold_left (fun acc expr -> acc ^ fmt_expr expr ^ ",") "" expr_ls)
  | VerifiedTree.Block expr_ls ->
    Printf.sprintf
      "Block(%s)"
      (List.fold_left (fun acc expr -> acc ^ fmt_expr expr ^ ";") "" expr_ls)
  | VerifiedTree.Cons hd_tail ->
    Printf.sprintf
      "Cons(hd = %s, tail = %s)"
      (fmt_expr hd_tail.hd)
      (fmt_expr hd_tail.tail)
  | VerifiedTree.Nil -> Printf.sprintf "Nil"
;;

let fmt_prog prog = fmt_expr prog
