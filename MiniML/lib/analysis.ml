open Ast
open Helpers
module Env = Map.Make (String)

type info =
  { expr : VerifiedTree.expr
  ; analysed_type : types
  }

let make_info expr analysed_type = { expr; analysed_type }

let rec info_list_split = function
  | [] -> [], []
  | { expr; analysed_type; _ } :: l ->
    let r_expr, r_analysed_type = info_list_split l in
    expr :: r_expr, analysed_type :: r_analysed_type
;;

let info_array_split x =
  if x = [||]
  then [||], [||]
  else (
    let info = Array.get x 0 in
    let n = Array.length x in
    let a = Array.make n info.expr in
    let b = Array.make n info.analysed_type in
    for i = 1 to n - 1 do
      let infoi = Array.get x i in
      Array.set a i infoi.expr;
      Array.set b i infoi.analysed_type
    done;
    a, b)
;;

let match_identifier_type (ident : Ast.identifier) toCompare errLoc =
  match ident.type_t with
  | None -> toCompare
  | Some vtype ->
    if vtype = toCompare
    then vtype
    else
      err
        (Printf.sprintf
           " Var(%s) With Wrong Type (Expected : %s) (Given : %s)"
           ident.name
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

let rec analyse_expr env toanalyse =
  match toanalyse with
  | Syntax.Const a -> make_info (VerifiedTree.Const a.const) (analyse_const a.const)
  | Syntax.Tuple t ->
    let content, analysed_type =
      info_array_split (Array.map (analyse_expr env) t.content)
    in
    make_info (VerifiedTree.Tuple content) (Tuple_t analysed_type)
  | Syntax.Seq b ->
    let content, analysed_type =
      info_array_split (Array.map (analyse_expr env) b.content)
    in
    make_info (VerifiedTree.Seq content) (array_getlast analysed_type)
  | Syntax.Nil _ -> make_info VerifiedTree.Nil (List_t Weak_t)
  | Syntax.Cons e ->
    let hd_info = analyse_expr env e.hd in
    let tail_info = analyse_expr env e.tail in
    let output = VerifiedTree.Cons { hd = hd_info.expr; tail = tail_info.expr } in
    (match tail_info.analysed_type with
    | List_t Weak_t -> make_info output (List_t hd_info.analysed_type)
    | List_t x ->
      if hd_info.analysed_type = x
      then make_info output x
      else
        err
          (Printf.sprintf
             "Invalid Type Expected:%s Given:%s"
             (Format.fmt_types x)
             (Format.fmt_types hd_info.analysed_type))
          e.loc.start_pos
    | _ -> err "Invalid This Is Not A List" e.loc.start_pos)
  | Syntax.Var var ->
    (match Env.find_opt var.ident.name env with
    | Some assigned_type ->
      make_info
        (VerifiedTree.Var { var_name = var.ident.name })
        (match_identifier_type var.ident assigned_type var.loc)
    | None -> err "Unbound Variable" var.loc.start_pos)
  | Syntax.Binding new_var ->
    let init_info = analyse_expr env new_var.init in
    verify_identifier_type new_var.ident init_info.analysed_type new_var.loc;
    let body_env = Env.add new_var.ident.name init_info.analysed_type env in
    let body_info = analyse_expr body_env new_var.content in
    make_info
      (VerifiedTree.Binding
         { var_name = new_var.ident.name
         ; init = init_info.expr
         ; content = body_info.expr
         })
      body_info.analysed_type
  | Syntax.Lambda lambda ->
    let body_env =
      List.fold_left
        (fun prev_env elem ->
          match elem.type_t with
          | Some t -> Env.add elem.name t prev_env (*TODO Verify duplicated parameters *)
          | None ->
            (*! Because of Basic Typing  *)
            err "Lambda with untyped parametters" lambda.loc.start_pos
          (*! Because of Basic Typing  *))
        env
        lambda.args
    in
    let body_info = analyse_expr body_env lambda.body in
    let args_names, args_types =
      List.split (List.map (fun elem -> elem.name, Option.get elem.type_t) lambda.args)
    in
    make_info
      (VerifiedTree.Lambda { args = args_names; body = body_info.expr })
      (Lambda_t (args_types, body_info.analysed_type))
  | Syntax.Call funcall ->
    let func_info = analyse_expr env funcall.func in
    let args_expr, _args_types =
      info_list_split (List.map (analyse_expr env) funcall.args)
    in
    (match func_info.analysed_type with
    | Lambda_t (_args_types, return_type) ->
      (* TODO Type This *)
      (* TODO Manage Partial Applications *)
      make_info
        (VerifiedTree.Call { func = func_info.expr; args = args_expr })
        return_type
    | _ ->
      err
        (Printf.sprintf
           "Call with wrong type Expected:Lambda Given:%s"
           (Format.fmt_types func_info.analysed_type))
        funcall.loc.start_pos)
;;

let analyse_prog = analyse_expr Env.empty
