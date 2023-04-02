open Vars
open Types
open Constructors
open Preprocess_ast
open Intern_common
open Intern_prelude
open Prelude



let intern_type_annot env scope typ = match typ with
  | Some typ -> intern_type !env scope typ
  | None -> TInternal (TyVar.fresh ())

let rec visit_many_vars vars k = function
    | [] -> vars, []
    | h::t ->
      let vars, h = k vars h in
      let vars, t = visit_many_vars vars k t in
      vars, h :: t

let fill_out_private_of_cons env (Raw_Cons cons) mk loc =
  let Consdef {constructor = Raw_Cons def; _} = def_of_cons !env.prelude cons.tag in
  let rec go priv typs = match priv, typs with
    | [], [] -> []
    | (_,so)::t, [] -> (mk so)::(go t [])
    | (_,so)::t, None::t' -> (mk so)::(go t t')
    | _::t, Some h::t' -> h :: (go t t')
    | [], _::_ ->
      fail_too_many_cons_parameters (List.length def.idxs) (List.length cons.idxs) cons.tag loc
  in
  Raw_Cons {cons with idxs = go def.idxs cons.idxs}

let fill_out_private_of_destr env (Raw_Destr destr) mk loc =
  let Destrdef {destructor = Raw_Destr def; _} =
    def_of_destr !env.prelude destr.tag in
  let rec go priv typs = match priv, typs with
    | [], [] -> []
    | (_,so)::t, [] -> (mk so)::(go t [])
    | (_,so)::t, None::t' -> (mk so)::(go t t')
    | _::t, Some h::t' -> h :: (go t t')
    | [], _::_ ->
      fail_too_many_destr_parameters (List.length def.idxs) (List.length destr.idxs) destr.tag loc
  in
  Raw_Destr { destr with idxs = go def.idxs destr.idxs}

let visit_cons vars env loc kx ki (Raw_Cons cons) =
  let tag = match cons.tag with
    | PosCons c -> begin
        try PosCons (StringEnv.find c !env.conses)
        with Not_found -> fail_undefined_cons c loc
      end
    | Unit -> Unit
    | Thunk -> Thunk
    | Bool b -> Bool b
    | Int n -> Int n
    | Tupple l -> Tupple l
    | Inj (i,n) -> Inj (i,n) in
  let vars, idxs = List.fold_left_map ki vars cons.idxs in
  let vars, args = List.fold_left_map kx vars cons.args in
  vars, Raw_Cons {tag; idxs; args}

let visit_destr vars env loc kx ki ka (Raw_Destr destr) =
  let tag = match destr.tag with
    | NegCons d -> begin
        try NegCons (StringEnv.find d !env.destrs)
        with Not_found -> fail_undefined_cons d loc
      end
    | Call l -> Call l
    | Proj (i,n) -> Proj (i,n)
    | Closure q -> Closure q in
  let vars, idxs = List.fold_left_map ki vars destr.idxs in
  let vars, args = List.fold_left_map kx vars destr.args in
  let vars, cont = ka vars destr.cont in
  vars, Raw_Destr {tag; idxs; args; cont}


let intern_pol = function
  | Some p -> Litt (Base p)
  | None -> Redirect (USortVar.fresh ())

let intern_sort env loc = function
  | Some so -> Litt (intern_sort env loc so)
  | None -> Redirect (USortVar.fresh ())

let rec intern_val env scope = function

  | Cst.Var {node; loc} ->
    let var =
      try get_var scope node
      with Not_found -> fail_undefined_var node loc in
    let val_typ = TInternal (TyVar.fresh ()) in
    MetaVal {node = Var var; loc; val_typ}

  | Cst.CoTop {loc} -> MetaVal {node = CoTop; loc; val_typ = top}

  | Cst.Bindcc {bind=(a,typ); pol; cmd; loc} ->
    let pol = intern_pol pol in
    let val_typ = intern_type_annot env scope typ in
    let scope = add_covar scope a in
    let a = get_covar scope a in
    let cmd = intern_cmd env scope cmd in
    MetaVal {node = Bindcc {bind = (a, val_typ); pol; cmd}; loc; val_typ}

  | Cst.Box {bind=(a,typ); cmd; loc; kind} ->
    let typ = intern_type_annot env scope typ in
    let scope = add_covar scope a in
    let a = get_covar scope a in
    let cmd = intern_cmd env scope cmd in
    MetaVal {node = Box {bind = (a, typ); cmd; kind}; loc; val_typ = boxed exp typ}

  | Cst.Macro_box {kind; valu; loc} ->
    let a_str = CoVar.to_string (CoVar.fresh ()) in
    let scope = add_covar scope a_str in
    intern_val env scope (Cst.V.box ~loc kind a_str None Cst.(valu |+| S.ret a_str))

  | Cst.Macro_fun {args; valu; loc} ->
    let a_str = CoVar.to_string (CoVar.fresh ()) in
    let scope = add_covar scope a_str in
    let func = Cst.(V.case ~loc:loc [
        call args (a_str, None) |=> (valu |~| S.ret a_str)
      ]) in
    intern_val env scope func

  | Cst.Cons {node;loc} ->
    let val_typ = TInternal (TyVar.fresh ()) in
    MetaVal {node = Cons (intern_cons env scope loc node); loc; val_typ}

  | Cst.Destr {cases = []; default = None; loc} ->
    intern_val env scope (CoTop {loc})

  | Cst.Destr {cases = []; default = Some (bind,cmd); loc} ->
    intern_val env scope (Bindcc {bind; cmd; loc; pol = None})

  | Cst.Destr {cases = (_::_) as cases ; default; loc} ->
    let val_typ = TInternal (TyVar.fresh ()) in
    let go_one (destr, cmd) =
      let scope, destr = intern_copatt env scope loc destr in
      let cmd = intern_cmd env scope cmd in
      (destr, cmd) in
    let cases = List.map go_one cases in
    let default = match default with
      | None -> None
      | Some ((name, typ), cmd) ->
        let typ = intern_type_annot env scope typ in
        let scope = add_covar scope name in
        let name = get_covar scope name in
        let cmd = intern_cmd env scope cmd in
        Some ((name, typ), cmd) in
    MetaVal {loc; val_typ; node = Destr {cases; default; for_type = tycons_for_copatts env cases}}

  | Fix {self=(x,tx); cont=(a,ta); cmd; loc} ->
    let scope = add_var (add_covar scope a) x in
    let x_typ = intern_type_annot env scope tx in
    let a_typ = intern_type_annot env scope ta in
    let val_typ = TInternal (TyVar.fresh ()) in
    MetaVal {loc; val_typ; node = Fix {
        self = (get_var scope x, x_typ);
        cont = (get_covar scope a, a_typ);
        cmd = intern_cmd env scope cmd;
      }}


and intern_cmd env scope cmd = match cmd with

  | Cst.Macro_term {name; pol; typ; valu; cmd; loc} ->
    let stk = Cst.S.(bind ~loc ?pol name typ cmd) in
    intern_cmd env scope (Command {loc; pol; valu; stk; typ})

  | Cst.Macro_env {pol; name; typ; stk; cmd; loc} ->
    let valu = Cst.V.(bindcc ~loc ?pol name typ cmd) in
    intern_cmd env scope (Command {loc; pol; valu; stk; typ})

  | Cst.Macro_match_val {patt; pol; valu; cmd; loc} ->
    let stk = Cst.S.case [patt, cmd] in
    intern_cmd env scope (Command {loc; pol; valu; stk; typ = None})

  | Cst.Macro_match_stk {copatt; pol; stk; cmd; loc} ->
    let valu = Cst.V.case [copatt, cmd] in
    intern_cmd env scope (Command {loc; pol; valu; stk; typ = None})

  | Cst.Command {pol; valu; stk; typ; loc} ->
    let mid_typ = intern_type_annot env scope typ in
    let valu = intern_val env scope valu in
    let stk = intern_stk env scope stk in
    let pol = intern_pol pol in
    Command {mid_typ; loc; valu; stk; pol}


and intern_stk env scope stk =
  let final_typ = tvar (TyVar.fresh ()) in

  match stk with

  | Cst.Ret {var; loc} ->
    MetaStack {loc; cont_typ = final_typ; node = Ret (get_covar scope var)}

  | Cst.CoZero {loc} ->
    MetaStack {loc; cont_typ = zero; node = CoZero}

  | Cst.CoBind {loc; bind=(name,typ); pol; cmd} ->
    let cont_typ = intern_type_annot env scope typ in
    let scope = add_var scope name in
    let name = get_var scope name in
    MetaStack {loc; cont_typ; node = CoBind {
        bind = (name, cont_typ);
        pol = intern_pol pol;
        cmd = intern_cmd env scope cmd
      }}

  | CoBox {kind; stk; loc} ->
    let cont_typ = TInternal (TyVar.fresh ()) in
    let node = CoBox {kind; stk = intern_stk env scope stk} in
    MetaStack {loc; cont_typ; node}

  | CoCons {cases = []; default = None; loc} ->
    intern_stk env scope (CoZero {loc})

  | CoCons {cases = []; default = Some (bind, cmd); loc} ->
    intern_stk env scope (CoBind {loc; bind; cmd; pol = None})

  | CoCons {cases = (_::_) as cases; default; loc} ->
    let go_one (cons, cmd) =
      let scope, cons = intern_patt env scope loc cons in
      let cmd = intern_cmd env scope cmd in
      (cons, cmd) in
    let default = match default with
      | None -> None
      | Some ((name, typ), cmd) ->
        let typ = intern_type_annot env scope typ in
        let scope = add_var scope name in
        let name = get_var scope name in
        let cmd = intern_cmd env scope cmd in
        Some ((name, typ), cmd) in
    let cases = List.map go_one cases in
    MetaStack {loc;
               cont_typ = tvar (TyVar.fresh ());
               node = CoCons {cases; default; for_type = tycons_for_patts env cases}
              }

  | CoDestr {node; loc} ->
    let cont_typ = TInternal (TyVar.fresh ()) in
    MetaStack {loc; cont_typ; node = CoDestr (intern_destr env scope loc node)}

  | CoFix {stk; loc} ->
    let cont_typ = TInternal (TyVar.fresh ()) in
    let stk = intern_stk env scope stk in
    MetaStack {loc; cont_typ; node = CoFix stk}


and tycons_for_patts env patts =
  match patts with
  | [] -> Misc.fail_invariant_break "tried to guess a type constructor for an empty pattern"
  | (Raw_Cons cons, _) :: _ ->
    let Consdef consdef = def_of_cons !env.prelude cons.tag in
    match consdef.resulting_type with
    | TCons {node = c; _} | TApp {tfun = TCons {node = c; _}; _} -> c
    | _ -> Misc.fail_invariant_break
             "A internalized datatype definition has no associated type constructor"

and intern_cons env vars loc cons =
  let mk _ = tvar (TyVar.fresh ()) in
  let _, cons = visit_cons vars env loc
      (fun vars valu -> (vars, intern_val env vars valu))
      (fun vars typ ->  vars, Some (intern_type_annot env vars typ))
      cons in
  fill_out_private_of_cons env cons mk loc

and intern_patt env scope loc patt =
  let mk so = (TyVar.fresh (), Litt so) in
  let kx scope (name, typ) =
    let typ = intern_type_annot env scope typ in
    let scope = add_var scope name in
    let var = get_var scope name in
    scope, (var, typ) in
  let kt scope bind =
    match bind with
    | None -> scope, None
    | Some (tvar, so) ->
      let so = intern_sort !env loc so in
      let scope = add_tyvar scope tvar in
      let new_var = get_tyvar scope tvar in
      (scope, Some (new_var, so)) in
  let scope, patt = visit_cons scope env loc kx kt patt in
  scope, fill_out_private_of_cons env patt mk loc


and tycons_for_copatts env copatts =
  match copatts with
  | [] ->  Misc.fail_invariant_break "tried to guess a type constructor for an empty pattern"
  | (Raw_Destr destr, _) :: _ ->
    let Destrdef destrdef = def_of_destr !env.prelude destr.tag in
    match destrdef.resulting_type with
    | TCons {node = c; _} | TApp {tfun = TCons {node = c; _}; _} -> c
    | _ ->  Misc.fail_invariant_break
              "A internalized computation type definition has no associated type constructor"

and intern_destr env scope loc destr =
  let mk _ = tvar (TyVar.fresh ()) in
  let _,destr = visit_destr scope env loc
      (fun vars valu -> (vars, intern_val env vars valu))
      (fun vars typ ->  vars, Some (intern_type_annot env vars typ))
      (fun vars stk -> (vars, intern_stk env vars stk))
      destr in
  fill_out_private_of_destr env destr mk loc

and intern_copatt env scope loc copatt =
  let mk so = (TyVar.fresh (), Litt so) in
  let kx scope (name, typ) =
    let scope = add_var scope name in
    let var = get_var scope name in
    let typ = intern_type_annot env scope typ in
    scope, (var, typ) in
  let ka scope (name, typ) =
    let scope = add_covar scope name in
    let var = get_covar scope name in
    let typ = intern_type_annot env scope typ in
    scope, (var, typ) in
  let kt scope bind =
    match bind with
    | None -> scope, None
    | Some (tvar, so) ->
      let so = intern_sort !env loc so in
      let scope = add_tyvar scope tvar in
      let new_var = get_tyvar scope tvar in
      (scope, Some (new_var, so)) in
  let scope, copatt = visit_destr scope env loc kx kt ka copatt in
  scope, fill_out_private_of_destr env copatt mk loc


type intern_result =
  | Def of prog_item
  | Exec of command_execution
  | Goal of TyConsVar.t * int * int

let intern_decl env scope def =
  let env = ref env in

  match def with

  | Cst.Term_declaration {name; typ; loc} ->
    let var = Var.of_string name in
    let def = Value_declaration {
        bind = (var,  intern_type_annot env scope (Some typ));
        loc;
        pol = Redirect (USortVar.fresh ())} in
    let scope = {scope with vars = StringEnv.add name var scope.vars} in
    (scope, (Def def, loc), !env)

  | Cst.Term_definition {name; typ; content; loc} ->
    let var = Var.of_string name in
    let def = Value_definition {
        bind = (var, intern_type_annot env scope typ);
        content = intern_val env scope content;
        loc;
        pol = Redirect (USortVar.fresh ())} in
    let scope = {scope with vars = StringEnv.add name var scope.vars} in
    (scope, (Def def, loc), !env)

  | Cst.Cmd_execution {name; typ; content; loc; cont} ->
    let final_type = intern_type_annot env scope typ in
    let scope = add_covar scope cont in
    let var = match name with
      | Some name -> Var.of_string name
      | None -> Var.of_string "anon" in
    let exec = Command_execution {
        name = var;
        conttyp = final_type;
        cont = get_covar scope cont;
        content = intern_cmd env scope content;
        loc;
        pol = Redirect (USortVar.fresh ())} in
    (scope, (Exec exec, loc) , !env)

  | Cst.Goal_selection {polynomial; degree; loc} ->
    let polynomial = StringEnv.find polynomial !env.tycons_vars in
    let sort = TyConsVar.Env.find polynomial !env.tycons_sort in
    let args, ret = unmk_arrow sort in
    let nat = Types.Index Primitives.sort_nat in
    if not (ret = nat) && List.for_all ((=) nat) args then
      fail_not_a_polynomial ()
    else
      (scope, (Goal (polynomial, degree, List.length args), loc), !env)

  | def -> Misc.fail_invariant_break ~loc:(Cst.loc_of_item def)
             "a prelude item was found among execution items during internalization "

let finalize_prog env prog =
  let init = (env.prelude, [], None, None) in
  let go (prelude, defs, exec, goal) (item,loc) =
    match item with
    | Def d -> (prelude, d::defs, exec, goal)
    | Exec e -> if exec <> None then
        fail_double_def "program entrypoint" loc
      else
        (prelude, defs, Some e, goal)
    | Goal (polynomial, degree, args_number) ->
      if goal <> None then
        fail_double_def "goal complexity for this file" loc
      else
        let goal = InternAst.Goal {polynomial; degree; args_number} in
        (prelude, defs, exec, Some goal)
  in
  let (prelude, defs, exec, goal) = List.fold_left go init prog in
  {prelude; declarations = defs; goal; command = exec}


let intern_prog env prog =
  let go (prog, env, decl) item =
    let decl, item, env = intern_decl env decl item in
    (item :: prog, env, decl) in
  let decl = empty_scope in
  let prog, env,_ = List.fold_left go ([],env,decl) prog in
  finalize_prog env prog
