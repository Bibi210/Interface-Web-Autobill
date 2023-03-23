open Vars
open Constructors
open Types
open Ast
open FullAst

type renaming = {
  vars : Var.t Var.Env.t;
  covars : CoVar.t CoVar.Env.t;
  tyvars : TyVar.t TyVar.Env.t
}

let empty_renaming = {
  vars = Var.Env.empty;
  covars = CoVar.Env.empty;
  tyvars = TyVar.Env.empty
}

let add_var env x = { env with vars = Var.Env.add x (Var.fresh ()) env.vars}

let add_covar env x = { env with covars = CoVar.Env.add x (CoVar.fresh ()) env.covars}

let add_tyvar env x = {env with tyvars = TyVar.Env.add x (TyVar.fresh ()) env.tyvars}

let get_var env x = Option.value (Var.Env.find_opt x env.vars) ~default:x

let get_covar env x = Option.value (CoVar.Env.find_opt x env.covars) ~default:x

let get_tyvar env x = Option.value (TyVar.Env.find_opt x env.tyvars) ~default:x


let rec alpha_typ env t = match t with
  | Types.TCons _ -> t
  | Types.TApp {tfun;args;loc} ->
    TApp {loc; tfun = alpha_typ env tfun; args = List.map (alpha_typ env) args}
  | Types.TVar {node;loc} -> TVar {loc; node = get_tyvar env node}
  | Types.TInternal node -> TInternal (get_tyvar env node)

let alpha_bind env (x,t) =
  let env = add_var env x in
  env, (get_var env x, alpha_typ env t)

let alpha_cobind env (x,t) =
  let env = add_covar env x in
  env, (get_covar env x, alpha_typ env t)

let alpha_typbind env (x,t) =
  let env = add_tyvar env x in
  env, (get_tyvar env x, t)


let rec alpha_val env (MetaVal {node; val_typ; loc}) =
  MetaVal {loc; val_typ = alpha_typ env val_typ; node = alpha_preval env node}

and alpha_stk env (MetaStack {node; cont_typ; loc}) =
  MetaStack {loc; cont_typ = alpha_typ env cont_typ; node = alpha_prestk env node}

and alpha_preval env t = match t with
  | Var v -> Var (get_var env v)
  | CoTop -> CoTop
  | Bindcc {bind; pol; cmd} ->
    let env, bind = alpha_cobind env bind in
    Bindcc {bind; pol; cmd = alpha_cmd env cmd}
  | Box {bind; kind; cmd} ->
    let env, bind = alpha_cobind env bind in
    Box {bind; kind; cmd = alpha_cmd env cmd}
  | Fix {self; cont; cmd} ->
    let env, self = alpha_bind env self in
    let env, cont = alpha_cobind env cont in
    Fix {self; cont; cmd = alpha_cmd env cmd}
  | Cons cons -> Cons (alpha_cons env cons)
  | Destr {for_type; cases; default} ->
    Destr {
      for_type;
      cases = cases |> List.map (fun (patt, cmd) ->
          let env, patt = alpha_patt env patt in
          (patt, alpha_cmd env cmd));
      default = default |> Option.map (fun (bind,cmd) ->
          let _, bind = alpha_cobind env bind in
          (bind, cmd))
    }

and alpha_prestk env s = match s with
  | Ret x -> Ret (get_covar env x)
  | CoZero -> CoZero
  | CoBind {bind;pol;cmd} ->
    let env, bind = alpha_bind env bind in
    CoBind {bind;pol; cmd = alpha_cmd env cmd}
  | CoBox {kind;stk} -> CoBox {kind; stk = alpha_stk env stk}
  | CoFix stk -> CoFix (alpha_stk env stk)
  | CoDestr destr -> CoDestr (alpha_destr env destr)
  | CoCons {for_type; cases; default} ->
    CoCons {
      for_type;
      cases = cases |> List.map (fun (patt, cmd) ->
          let env, patt = alpha_copatt env patt in
          (patt, alpha_cmd env cmd));
      default = default |> Option.map (fun (bind,cmd) ->
          let _, bind = alpha_bind env bind in
          (bind, cmd))
    }

and alpha_cmd env (Command {pol; valu; stk; mid_typ; loc}) =
  Command {
    pol; loc;
    valu = alpha_val env valu;
    stk = alpha_stk env stk;
    mid_typ = alpha_typ env mid_typ
  }

and alpha_cons env (Raw_Cons {tag; idxs; args})=
  Raw_Cons {
    tag;
    idxs = List.map (alpha_typ env) idxs;
    args = List.map (alpha_val env) args
  }

and alpha_destr env (Raw_Destr {tag; idxs; args; cont}) =
  Raw_Destr {
    tag;
    idxs = List.map (alpha_typ env) idxs;
    args = List.map (alpha_val env) args;
    cont = alpha_stk env cont
  }

and alpha_patt env (Raw_Destr {tag; idxs; args; cont}) =
  let env, idxs = List.fold_left_map alpha_typbind env idxs in
  let env, args = List.fold_left_map alpha_bind env args in
  let env, cont = alpha_cobind env cont in
  env, (Raw_Destr {tag; idxs; args; cont})

and alpha_copatt env (Raw_Cons {tag; idxs; args}) =
  let env, idxs = List.fold_left_map alpha_typbind env idxs in
  let env, args = List.fold_left_map alpha_bind env args in
  env, (Raw_Cons {tag; idxs; args})
