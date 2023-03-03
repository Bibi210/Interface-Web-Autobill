open Types
open Vars
open Constructors
open Misc
open FirstOrder.FullFOL

type type_bind = TyVar.t * SortVar.t Types.sort

type tycons_definition = {
  sort : sort;
  loc : position;
  args : type_bind list;
  content : tycons_def_content
}
and tycons_def_content =
  | Declared
  | Defined of typ
  | Data of ((ConsVar.t, type_bind, typ) constructor * eqn list) list
  | Codata of ((DestrVar.t, type_bind, typ, typ) destructor * eqn list) list

and cons_definition = Consdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  equations : eqn list;
  val_args : typ list;
  resulting_type : typ
}

and destr_definition = Destrdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  equations : eqn list;
  val_args : typ list;
  ret_arg : typ;
  resulting_type : typ
}


type _prelude = {
  sort_defs : unit SortVar.Env.t;
  relations : sort list RelVar.Env.t;
  tycons : tycons_definition TyConsVar.Env.t;
  cons : cons_definition ConsVar.Env.t;
  destr : destr_definition DestrVar.Env.t;
  vars : typ Var.Env.t;
  covars : typ CoVar.Env.t;
  sorts : sort TyVar.Env.t;
  var_multiplicities : var_multiplicity Var.Env.t;
  covar_multiplicities : var_multiplicity CoVar.Env.t
}

type prelude = _prelude ref

let empty_prelude () = ref {
    sort_defs = SortVar.Env.empty;
    relations = RelVar.Env.empty;
    tycons = TyConsVar.Env.empty;
    cons = ConsVar.Env.empty;
    destr = DestrVar.Env.empty;
    vars = Var.Env.empty;
    covars = CoVar.Env.empty;
    sorts = TyVar.Env.empty;
    var_multiplicities = Var.Env.empty;
    covar_multiplicities = CoVar.Env.empty
  }



let get_env var env =
  match TyVar.Env.find_opt var !env with
  | Some v -> v
  | None ->
    (let w = TyVar.fresh () in
    env := TyVar.Env.add var w !env;
    w)

let rec refresh_typ env typ = match typ with
| TBox b -> TBox {b with node = refresh_typ env b.node}
| TVar {node; loc} -> TVar {node = get_env node env; loc}
| TPos typ -> TPos (refresh_typ env typ)
| TNeg typ -> TNeg (refresh_typ env typ)
| TInternal var -> TInternal (get_env var env)
| TFix t -> TFix (refresh_typ env t)
| TCons _ -> typ
| TApp {loc; tfun; args} ->
  TApp {loc;
       tfun = refresh_typ env tfun;
       args = List.map (refresh_typ env) args}

and refresh_type_bind env (t,so) = (get_env t env, so)

and refresh_eqns env eqns =
  let refresh_eqn = function
    | Eq (a,b,so) -> Eq (refresh_typ env a, refresh_typ env b, so)
    | Rel (rel, args) -> Rel (rel, List.map (refresh_typ env) args) in
  List.map refresh_eqn eqns

and refresh_cons env = function
  | PosCons (cons, args, ts) ->
    PosCons (cons,
             List.map (refresh_type_bind env) args,
             List.map (refresh_typ env) ts)
  | _ -> raise (Failure "Internalisation invariant")

and refresh_destr env = function
  | NegCons (cons, args, ts, t) ->
    NegCons (cons,
             List.map (refresh_type_bind env) args,
             List.map (refresh_typ env) ts,
             refresh_typ env t)
  | _ -> raise (Failure "Internalisation invariant")


let add_sorts prelude binds =
  prelude := {!prelude with sorts = List.fold_left
                    (fun sorts (a,so) -> TyVar.Env.add a so sorts)
                    !prelude.sorts
                    binds}


let refresh_tycons_def prelude env def =
  let args = List.map (fun (x,so) -> (get_env x env, so)) def.args in
  add_sorts prelude args;
  {def with
   args;
   content = match def.content with
     | Declared -> Declared
     | Defined typ -> Defined (refresh_typ env typ)
     | Data conses ->
       Data (List.map (fun (x,y) -> refresh_cons env x, refresh_eqns env y) conses)
     | Codata destrs ->
       Codata (List.map (fun (x,y) -> refresh_destr env x, refresh_eqns env y) destrs)
  }

and refresh_cons_def prelude env
    (Consdef { typ_args; val_args; private_typs; resulting_type; equations }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  add_sorts prelude typ_args;
  add_sorts prelude private_typs;
  let equations = map_eqns (refresh_typ env) equations in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  Consdef {typ_args; val_args; resulting_type; private_typs; equations}

and refresh_destr_def prelude env
    (Destrdef { typ_args; val_args; ret_arg; resulting_type; private_typs; equations }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  add_sorts prelude typ_args;
  add_sorts prelude private_typs;
  let equations = map_eqns (refresh_typ env) equations in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  let ret_arg = refresh_typ env ret_arg in
  Destrdef {typ_args; val_args; resulting_type; ret_arg; private_typs; equations}

let def_of_cons prelude cons =
  refresh_cons_def prelude (ref TyVar.Env.empty) (ConsVar.Env.find cons !prelude.cons)


let def_of_destr prelude destr =
 refresh_destr_def prelude (ref TyVar.Env.empty) (DestrVar.Env.find destr !prelude.destr)


let def_of_tycons prelude t =
 refresh_tycons_def prelude (ref TyVar.Env.empty) (TyConsVar.Env.find t !prelude.tycons)
