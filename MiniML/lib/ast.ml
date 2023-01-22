open Helpers

type const =
  | Integer of int
  | Boolean of bool

type expr =
  | UseVar of
      { name : string
      ; loc : position
      }
  | DeclVar of
      { name : string
      ; expr : expr
      ; loc : position
      }
  | Const of
      { value : const
      ; loc : position
      }
  | Tuple of
      { value : expr list
      ; loc : position
      }
  | Func_Call of
      { func : string
      ; args : expr list
      ; loc : position
      }

type prog = expr list

let fmt_value = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
;;

let rec fmt_expr acc e =
  (match e with
  | UseVar v -> Printf.sprintf "UseVar(%s,Pos = %s)" v.name (string_of_position v.loc)
  | DeclVar v ->
    Printf.sprintf
      "DeclVar(%s,Pos = %s,Assign = %s)"
      v.name
      (string_of_position v.loc)
      (fmt_expr acc v.expr)
  | Const v ->
    Printf.sprintf "Value(%s,Pos = %s)" (fmt_value v.value) (string_of_position v.loc)
  | Tuple t ->
    Printf.sprintf
      "Tuple((%s),Pos = %s)"
      (print_expr_list t.value)
      (string_of_position t.loc)
  | Func_Call f ->
    Printf.sprintf
      "Func_Call(funcName = %s,NbArgs %d, Pos = %s)"
      f.func
      (List.length f.args)
      (string_of_position f.loc))
  ^ acc

and print_expr_list expr_ls =
  List.fold_right (fun ls acc -> " , " ^ fmt_expr acc ls) expr_ls ""
;;

let fmt_program prog = List.fold_right (fun ls acc -> "\n" ^ fmt_expr acc ls) prog ""
