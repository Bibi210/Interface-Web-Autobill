open Ast
open Helpers

type info =
  { expr : VerifiedTree.expr
  ; etype : types
  }

module Env = Map.Make (String)

let info_constructor expr etype = { expr; etype }

let rec info_list_split = function
  | [] -> [], []
  | { expr; etype; _ } :: l ->
    let r_expr, r_etype = info_list_split l in
    expr :: r_expr, etype :: r_etype
;;

let info_array_split x =
  if x = [||]
  then [||], [||]
  else (
    let info = Array.get x 0 in
    let n = Array.length x in
    let a = Array.make n info.expr in
    let b = Array.make n info.etype in
    for i = 1 to n - 1 do
      let infoi = Array.get x i in
      Array.set a i infoi.expr;
      Array.set b i infoi.etype
    done;
    a, b)
;;

let match_identifier_type (ident : Ast.identifier) toCompare errLoc =
  match ident.etype with
  | None -> toCompare
  | Some vtype ->
    if vtype = toCompare
    then vtype
    else
      err
        (Printf.sprintf
           " Var(%s) With Wrong Type Expected:%s Given:%s"
           ident.var_name
           (Format.fmt_types vtype)
           (Format.fmt_types toCompare))
        errLoc.start_pos
;;

let verify_identifier_type ident toCompare errLoc =
  let _ = match_identifier_type ident toCompare errLoc in
  ()
;;

let analyse_const a =
  match a with
  | Integer _ -> Int_t
  | Boolean _ -> Bool_t
;;

let rec analyse_expr env a =
  match a with
  | Syntax.Const a ->
    info_constructor (VerifiedTree.Const a.const) (analyse_const a.const)
  | Syntax.Tuple t ->
    let content, etype = info_array_split (Array.map (analyse_expr env) t.content) in
    info_constructor (VerifiedTree.Tuple content) (Tuple etype)
  | Syntax.Seq b ->
    let content, etype = info_array_split (Array.map (analyse_expr env) b.content) in
    info_constructor (VerifiedTree.Seq content) (array_getlast etype)
  | Syntax.Nil _ -> info_constructor VerifiedTree.Nil (List WeakType)
  | Syntax.Cons e ->
    let hd_info = analyse_expr env e.hd in
    let tail_info = analyse_expr env e.tail in
    let output = VerifiedTree.Cons { hd = hd_info.expr; tail = tail_info.expr } in
    (match tail_info.etype with
    | List WeakType -> info_constructor output (List hd_info.etype)
    | List x ->
      if hd_info.etype = x
      then info_constructor output x
      else
        err
          (Printf.sprintf
             "Invalid Type Expected:%s Given:%s"
             (Format.fmt_types x)
             (Format.fmt_types hd_info.etype))
          e.loc.start_pos
    | _ -> err "Invalid This Is Not A List" e.loc.start_pos)
  | Syntax.Var var ->
    (match Env.find_opt var.ident.var_name env with
    | Some envType ->
      info_constructor
        (VerifiedTree.Var { var_name = var.ident.var_name })
        (match_identifier_type var.ident envType var.loc)
    | None -> err "Unbound Variable" var.loc.start_pos)
  | Syntax.Binding new_var ->
    let init_info = analyse_expr env new_var.init in
    verify_identifier_type new_var.ident init_info.etype new_var.loc;
    let body_env = Env.add new_var.ident.var_name init_info.etype env in
    let body_info = analyse_expr body_env new_var.content in
    info_constructor
      (VerifiedTree.Binding
         { var_name = new_var.ident.var_name
         ; init = init_info.expr
         ; content = body_info.expr
         })
      body_info.etype
;;

let analyse_prog = analyse_expr Env.empty
