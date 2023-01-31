type const =
  | Integer of int
  | Boolean of bool

(* Flow de Control *)
(* Binding *)
(* Operateurs de Base *)

module VerifiedTree = struct
  type expr =
    | Const of const
    | Tuple of expr array
    | Seq of expr array
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
        { content : expr array
        ; loc : Helpers.position
        }
(*     | Binding of
        { varname : string
        ; content : expr
        ; loc : Helpers.position
        }  *)
    | Seq of
        { content : expr array
        ; loc : Helpers.position
        }
    | Cons of
        { hd : expr (* Use of OCAML List ?? *)
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
      (Array.fold_left (fun acc expr -> acc ^ fmt_expr expr ^ ",") "" expr_ls)
  | Seq expr_ls ->
    Printf.sprintf
      "Block(%s)"
      (Array.fold_left (fun acc expr -> acc ^ fmt_expr expr ^ ";") "" expr_ls)
  | VerifiedTree.Cons hd_tail ->
    Printf.sprintf
      "Cons(hd = %s, tail = %s)"
      (fmt_expr hd_tail.hd)
      (fmt_expr hd_tail.tail)
  | VerifiedTree.Nil -> Printf.sprintf "Nil"
;;
