open Ast
open Helpers

type types =
  | Int_t
  | Bool_t
  | Tuple of types list
  | Generic
  | List of types

let rec fmt_types = function
  | Int_t -> "Int"
  | Bool_t -> "Bool"
  | Tuple types ->
    Printf.sprintf
      "Tuple of (%s)"
      (List.fold_left (fun acc expr -> acc ^ fmt_types expr ^ " ") "" types)
  | Generic -> "Generic"
  | List elem_type -> Printf.sprintf "List of (%s)" (fmt_types elem_type)
;;

type info =
  { expr : VerifiedTree.expr
  ; etype : types
  }

let info_constructor expr etype = { expr; etype }

let rec info_list_split = function
  | [] -> [], []
  | { expr; etype; _ } :: l ->
    let r_expr, r_etype = info_list_split l in
    expr :: r_expr, etype :: r_etype
;;

let analyse_const a =
  match a with
  | Integer _ -> Int_t
  | Boolean _ -> Bool_t
;;

let rec analyse_expr a =
  match a with
  | Syntax.Const a ->
    info_constructor (VerifiedTree.Const a.const) (analyse_const a.const)
  | Syntax.Tuple t ->
    let content, etype = info_list_split (List.map analyse_expr t.content) in
    info_constructor (VerifiedTree.Tuple content) (Tuple etype)
  | Syntax.Block b ->
    let content, etype = info_list_split (List.map analyse_expr b.content) in
    info_constructor (VerifiedTree.Block content) (Helpers.getlast etype)
  | Syntax.Nil _ -> info_constructor VerifiedTree.Nil (List Generic)
  | Syntax.Cons e ->
    let hd_info = analyse_expr e.hd in
    let tail_info = analyse_expr e.tail in
    let output = VerifiedTree.Cons { hd = hd_info.expr; tail = tail_info.expr } in
    (match tail_info.etype with
    | List Generic -> info_constructor output (List hd_info.etype)
    | List x ->
      if hd_info.etype = x
      then info_constructor output x
      else
        err
          (Printf.sprintf
             "Invalid Type Expected:%s Given:%s"
             (fmt_types x)
             (fmt_types hd_info.etype))
          e.loc.start_pos
    | _ -> err "Invalid This Is Not A List" e.loc.start_pos)
;;

let analyse_prog = analyse_expr
