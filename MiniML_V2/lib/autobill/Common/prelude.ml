open Types
open Vars
open Constructors
open Misc
open FirstOrder.FullFOL

type type_bind = TyVar.t * SortVar.t Types.sort

type cons_for_def = (ConsVar.t, type_bind, type_bind, typ) constructor

type destr_for_def = (ConsVar.t, type_bind, type_bind, typ, typ) destructor

type tycons_definition = {
  sort : sort;
  loc : position;
  args : type_bind list;
  content : tycons_def_content
}
and tycons_def_content =
  | Predefined
  | Declared
  | Defined of typ
  | Data of (ConsVar.t * cons_for_def * eqn list) list
  | Codata of (DestrVar.t * destr_for_def * eqn list) list

and cons_definition = Consdef of {
  typ_args : type_bind list;
  constructor : cons_for_def;
  equations : eqn list;
  resulting_type : typ
}

and destr_definition = Destrdef of {
  typ_args : type_bind list;
  destructor : destr_for_def;
  equations : eqn list;
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
  }



let get_env var env =
  match TyVar.Env.find_opt var !env with
  | Some v -> v
  | None ->
    (let w = TyVar.fresh () in
    env := TyVar.Env.add var w !env;
    w)

let rec refresh_typ env typ = match typ with
| TVar {node; loc} -> TVar {node = get_env node env; loc}
| TInternal var -> TInternal (get_env var env)
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


let add_sorts prelude binds =
  prelude := {!prelude with sorts = List.fold_left
                    (fun sorts (a,so) -> TyVar.Env.add a so sorts)
                    !prelude.sorts
                    binds}

let refresh_cons prelude env (Raw_Cons {tag; idxs; typs; args}) =
  let idxs = List.map (refresh_type_bind env) idxs in
  let typs = List.map (refresh_type_bind env) typs in
  add_sorts prelude idxs;
  add_sorts prelude typs;
  Raw_Cons ({
      tag;
      idxs;
      typs;
      args = List.map (refresh_typ env) args
    })


let refresh_destr prelude env (Raw_Destr {tag; idxs; typs; args; cont}) =
  let idxs = List.map (refresh_type_bind env) idxs in
  let typs = List.map (refresh_type_bind env) typs in
  add_sorts prelude idxs;
  add_sorts prelude typs;
  Raw_Destr ({
      tag;
      idxs;
      typs;
      args = List.map (refresh_typ env) args;
      cont = refresh_typ env cont
    })


let refresh_tycons_def prelude env def =
  let args = List.map (fun (x,so) -> (get_env x env, so)) def.args in
  add_sorts prelude args;
  {def with
   args;
   content = match def.content with
     | Declared -> Declared
     | Predefined -> Predefined
     | Defined typ -> Defined (refresh_typ env typ)
     | Data conses ->
       Data (List.map (fun (x,y,z) -> x, refresh_cons prelude env y, refresh_eqns env z) conses)
     | Codata destrs ->
       Codata (List.map (fun (x,y,z) -> x, refresh_destr prelude env y, refresh_eqns env z) destrs)
  }


and refresh_destr_def prelude env
    (Destrdef { typ_args; resulting_type; destructor; equations }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  add_sorts prelude typ_args;
  let destructor = refresh_destr prelude env destructor in
  let equations = map_eqns (refresh_typ env) equations in
  let resulting_type = refresh_typ env resulting_type in
  Destrdef {typ_args; resulting_type; destructor; equations}

let def_of_cons prelude (c : _ constructor_tag) = match c with
  | Unit -> Consdef {
      typ_args = [];
      constructor = unit;
      equations = [];
      resulting_type = unit_t
    }
  | Thunk ->
    let t = TyVar.fresh () in
    add_sorts prelude [t, sort_postype];
    Consdef {
      typ_args = [t, sort_postype];
      constructor = thunk (tvar t);
      equations = [];
      resulting_type = thunk_t (tvar t)
    }
  | Bool b -> Consdef {
      typ_args = [];
      constructor = cons (Bool b) [] [] [];
      equations = [];
      resulting_type = bool
    }
  | Int n -> Consdef {
      typ_args = [];
      constructor = cons (Int n) [] [] [];
      equations = [];
      resulting_type = int
    }
  | Tupple n ->
    let ts = List.init n ( fun _ -> TyVar.fresh ()) in
    add_sorts prelude (List.map (fun v -> (v, sort_postype)) ts);
    Consdef {
      typ_args = List.map (fun t -> (t, sort_postype)) ts;
      constructor = tuple (List.map tvar ts);
      equations = [];
      resulting_type = prod (List.map tvar ts)
    }
  | Inj (i, n) ->
    let ts = List.init n ( fun _ -> TyVar.fresh ()) in
    add_sorts prelude (List.map (fun v -> (v, sort_postype)) ts);
    Consdef {
      typ_args = List.map (fun t -> (t, sort_postype)) ts;
      constructor = inj i n (tvar (List.nth ts i));
      equations = [];
      resulting_type = sum (List.map tvar ts)
    }
  | PosCons cons ->
    let env = ref TyVar.Env.empty in
    let def = ConsVar.Env.find cons !prelude.cons in
    let Consdef { typ_args; constructor; resulting_type; equations } = def in
    let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
    add_sorts prelude typ_args;
    let constructor = refresh_cons prelude env constructor in
    let equations = map_eqns (refresh_typ env) equations in
    let resulting_type = refresh_typ env resulting_type in
    Consdef {typ_args; resulting_type; constructor; equations}


let def_of_destr prelude (destr : _ destructor_tag) = match destr with
  | Call n ->
    let args = List.init n ( fun _ -> TyVar.fresh ()) in
    add_sorts prelude (List.map (fun v -> (v, sort_postype)) args);
    let ret = TyVar.fresh () in
    add_sorts prelude [ret, sort_negtype];
    Destrdef {
      typ_args = (ret, sort_negtype) :: (List.map (fun t -> (t, sort_postype)) args);
      destructor = call (List.map tvar args) (tvar ret);
      equations = [];
      resulting_type = func (tvar ret :: List.map tvar args)
    }
  | Proj (i, n) ->
    let ts = List.init n ( fun _ -> TyVar.fresh ()) in
    add_sorts prelude (List.map (fun v -> (v, sort_negtype)) ts);
    Destrdef {
      typ_args = List.map (fun t -> (t, sort_negtype)) ts;
      destructor = proj i n (tvar (List.nth ts i));
      equations = [];
      resulting_type = choice (List.map tvar ts)
    }
  | Closure q ->
    let t = TyVar.fresh () in
    let q' = Option.fold
        ~none:(tvar (TyVar.fresh ()))
        ~some:(fun q -> Types.cons (Qual q))
        q in
    add_sorts prelude [t, sort_negtype];
    Destrdef {
      typ_args = [t, sort_negtype];
      destructor = closure ?q (tvar t);
      equations = [];
      resulting_type = app (Types.cons Closure) [q'; tvar t];
    }
  | NegCons destr ->
    refresh_destr_def prelude (ref TyVar.Env.empty) (DestrVar.Env.find destr !prelude.destr)


let def_of_tycons prelude = function
  | Cons t ->
    refresh_tycons_def prelude (ref TyVar.Env.empty) (TyConsVar.Env.find t !prelude.tycons)
  | t ->
    let cst x = ([], x) in
    let (-->) xs x = (xs, x) in
    let pos = Base Positive in
    let neg = Base Negative in
    let sos, rets = match t with
      | Unit | Zero | Int | Bool -> cst pos
      | Top | Bottom -> cst neg
      | Prod n | Sum n -> (List.init n (fun _ -> pos) ) --> pos
      | Choice n -> (List.init n (fun _ -> neg)) --> neg
      | Fun n -> (neg :: List.init n (fun _ -> pos)) --> neg
      | Thunk -> [pos]-->neg
      | Closure -> [sort_qual; neg]-->pos
      | Qual _ -> cst sort_qual
      | Fix -> [neg]-->neg
      | Cons c ->
        raise (Invalid_argument ("Not a predefined type constructor " ^ TyConsVar.to_string c))
    in
    {
      loc = dummy_pos;
      args = List.map (fun s -> (TyVar.fresh (), s)) sos;
      sort = sort_arrow sos rets;
      content = Predefined
    }
