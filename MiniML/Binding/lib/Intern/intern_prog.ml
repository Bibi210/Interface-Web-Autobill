open Vars
open Types
open Constructors
open Intern_common
open Prelude

open InternAst


let intern_type_annot env scope typ = match typ with
  | Some typ -> intern_type !env scope typ
  | None -> TInternal (TyVar.fresh ())

let rec visit_many_vars vars k = function
    | [] -> vars, []
    | h::t ->
      let vars, h = k vars h in
      let vars, t = visit_many_vars vars k t in
      vars, h :: t

let fill_out_private_of_cons env loc cons mk =
  let mk so = mk (unintern_sort so) in
  match cons with
  | PosCons (cons, typs, args) ->
    let new_cons =
      try StringEnv.find cons !env.conses
      with Not_found -> fail_undefined_cons cons loc in
    let Consdef {private_typs;_} = def_of_cons !env.prelude new_cons in
    let rec go priv typs = match priv, typs with
      | [], [] -> []
      | (_,so)::t, [] -> (mk so)::(go t [])
      | (_,so)::t, None::t' -> (mk so)::(go t t')
      | _::t, Some h::t' -> Some h :: (go t t')
      | [], _::_ -> assert false in
    PosCons (cons, go private_typs typs, args)
  | Unit -> Unit
  | Bool b -> Bool b
  | Int n -> Int n
  | Thunk x -> Thunk x
  | Tupple xs -> Tupple xs
  | Inj (i, n, x) -> Inj (i, n, x)

let fill_out_private_of_destr env loc destr mk =
  let mk so = mk (unintern_sort so) in
  match destr with
  | NegCons (destr, typs, args, cont) ->
    let new_destr =
      try StringEnv.find destr !env.destrs
      with Not_found -> fail_undefined_cons destr loc in
    let Destrdef {private_typs;_} = def_of_destr !env.prelude new_destr in
    let rec go priv typs = match priv, typs with
      | [], [] -> []
      | (_,so)::t, [] -> (mk so)::(go t [])
      | (_,so)::t, None::t' -> (mk so)::(go t t')
      | _::t, Some h::t' -> Some h :: (go t t')
      | [], _::_ -> assert false in
    NegCons(destr, go private_typs typs, args, cont)
  | Call (x, y) -> Call (x,y)
  | Proj (i, n, x) -> Proj (i, n, x)
  | Closure x -> Closure x

let visit_cons vars env loc kx kt = function
    | (Unit | Bool _ | Int _) as c -> vars, c
    | Thunk a ->
      let vars, a = kx vars a in
      (vars, Thunk a)
    | Tupple xs ->
      let vars, xs = visit_many_vars vars kx xs in
      vars, Tupple xs
    | Inj (i,n,a) -> let vars, a = kx vars a in vars, Inj (i,n,a)
    | PosCons (cons, typs, args) ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      let vars, typs = List.fold_left_map kt vars typs in
      let vars, args = List.fold_left_map kx vars args in
      vars, PosCons (cons, typs, args)

let visit_destr vars env loc kx kt ka = function
  | Call (xs,a) ->
    let vars, xs = visit_many_vars vars kx xs in
    let vars, a = ka vars a in
    vars, a, Call (xs, a)
  | Proj (i,n,a) -> let vars, a = ka vars a in vars, a, Proj (i,n,a)
  | Closure a -> let vars, a = ka vars a in vars, a, Closure a
  | NegCons (destr, typs, args, cont) ->
    let destr =
        try StringEnv.find destr !env.destrs
        with Not_found -> fail_undefined_cons destr loc in
    let vars, typs = List.fold_left_map kt vars typs in
    let vars, args = List.fold_left_map kx vars args in
    let vars, cont = ka vars cont in
      vars, cont, NegCons (destr, typs, args, cont)


let intern_definition env declared_vars def =

  let env = ref env in

  let intern_pol = function
    | Some p -> Litt (Base p)
    | None -> Redirect (USortVar.fresh ()) in

  let intern_sort env = function
    | Some so -> Litt (intern_sort env so)
    | None -> Redirect (USortVar.fresh ()) in

  let rec intern_val scope = function

    | Cst.Var {node; loc} ->
      let var =
        try get_var scope node
        with Not_found ->
        try StringEnv.find node declared_vars
        with Not_found -> fail_undefined_var node loc in
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Var var; loc; val_typ}

    | Cst.CoTop {loc} -> MetaVal {node = CoTop; loc; val_typ = top}

    | Cst.Bindcc {bind=(a,typ); pol; cmd; loc} ->
      let pol = intern_pol pol in
      let val_typ = intern_type_annot env scope typ in
      let scope = add_covar scope a in
      let a = get_covar scope a in
      let cmd = intern_cmd scope cmd in
      MetaVal {node = Bindcc {bind = (a, val_typ); pol; cmd}; loc; val_typ}

    | Cst.Box {bind=(a,typ); cmd; loc; kind} ->
      let typ = intern_type_annot env scope typ in
      let scope = add_covar scope a in
      let a = get_covar scope a in
      let cmd = intern_cmd scope cmd in
      MetaVal {node = Box {bind = (a, typ); cmd; kind}; loc; val_typ = boxed exp typ}

    | Cst.Macro_box {kind; valu; loc} ->
      let a_str = CoVar.to_string (CoVar.fresh ()) in
      let scope = add_covar scope a_str in
      intern_val scope (Cst.V.box ~loc kind a_str None Cst.(valu |+| S.ret a_str))

    | Cst.Macro_fun {args; valu; loc} ->
      let a_str = CoVar.to_string (CoVar.fresh ()) in
      let scope = add_covar scope a_str in
      let func = Cst.(V.case ~loc:loc [
          call args (a_str, None) |=> (valu |~| S.ret a_str)
        ]) in
      intern_val scope func

    | Cst.Cons {node;loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Cons (intern_cons env scope loc node); loc; val_typ}

    | Cst.Destr {node; loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      let go_one (destr, cmd) =
        let scope, _, destr = intern_copatt env scope loc destr in
        let cmd = intern_cmd scope cmd in
        (destr, cmd) in
      let destr = List.map go_one node in
      MetaVal {loc; val_typ; node = Destr destr}

    | Fix {self=(x,tx); cont=(a,ta); cmd; loc} ->
      let scope = add_var (add_covar scope a) x in
      let x_typ = intern_type_annot env scope tx in
      let a_typ = intern_type_annot env scope ta in
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {loc; val_typ; node = Fix {
          self = (get_var scope x, x_typ);
          cont = (get_covar scope a, a_typ);
          cmd = intern_cmd scope cmd;
        }}


  and intern_cmd scope cmd = match cmd with

    | Cst.Macro_term {name; pol; typ; valu; cmd; loc} ->
      let stk = Cst.S.(bind ~loc ?pol name typ cmd) in
      intern_cmd scope (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_env {pol; name; typ; stk; cmd; loc} ->
      let valu = Cst.V.(bindcc ~loc ?pol name typ cmd) in
      intern_cmd scope (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_match_val {patt; pol; valu; cmd; loc} ->
      let stk = Cst.S.case [patt, cmd] in
      intern_cmd scope (Command {loc; pol; valu; stk; typ = None})

    | Cst.Macro_match_stk {copatt; pol; stk; cmd; loc} ->
      let valu = Cst.V.case [copatt, cmd] in
      intern_cmd scope (Command {loc; pol; valu; stk; typ = None})

    | Cst.Command {pol; valu; stk; typ; loc} ->
      let mid_typ = intern_type_annot env scope typ in
      let final_typ = intern_type_annot env scope None in
      let valu = intern_val scope valu in
      let stk = intern_stk scope stk in
      let pol = intern_pol pol in
      Command {mid_typ; final_typ; loc; valu; stk; pol}


  and intern_stk scope stk =
    let final_typ = tvar (TyVar.fresh ()) in

    match stk with

    | Cst.Ret {var; loc} ->
      MetaStack {loc; cont_typ = final_typ; final_typ;
                 node = Ret (get_covar scope var)}

    | Cst.CoZero {loc} ->
      MetaStack {loc; cont_typ = zero; final_typ; node = CoZero}

    | Cst.CoBind {loc; bind=(name,typ); pol; cmd} ->
      let cont_typ = intern_type_annot env scope typ in
      let scope = add_var scope name in
      let name = get_var scope name in
      MetaStack {loc; cont_typ; final_typ; node = CoBind {
          bind = (name, cont_typ);
          pol = intern_pol pol;
          cmd = intern_cmd scope cmd
        }}

    | CoBox {kind; stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let node = CoBox {kind; stk = intern_stk scope stk} in
      MetaStack {loc; cont_typ; final_typ; node}

    | CoCons {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let go_one (cons, cmd) =
        let scope, cons = intern_patt env scope loc cons in
        let cmd = intern_cmd scope cmd in
        (cons, cmd) in
      MetaStack {loc; cont_typ; final_typ; node = CoCons (List.map go_one node)}

    | CoDestr {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      MetaStack {loc; cont_typ; final_typ;
                 node = CoDestr (intern_destr env scope loc node)}

    | CoFix {stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let stk = intern_stk scope stk in
      MetaStack {loc; cont_typ; final_typ; node = CoFix stk}


  and intern_cons env vars loc cons =
    let mk _ = None in
    let cons = fill_out_private_of_cons env loc cons mk in
    let _, cons = visit_cons vars env loc
        (fun vars valu -> (vars, intern_val vars valu))
        (fun vars typ ->  (vars, intern_type_annot env vars typ))
        cons in
    cons

  and intern_patt env scope loc patt =
    let mk so = Some (TyVar.to_string (TyVar.fresh ()), Some so) in
    let patt = fill_out_private_of_cons env loc patt mk in
    let kx scope (name, typ) =
      let typ = intern_type_annot env scope typ in
      let scope = add_var scope name in
      let var = get_var scope name in
      scope, (var, typ) in
    let kt scope bind =
      let (tvar, so) = match bind with
        | None -> assert false
        | Some x -> x in
      let so = intern_sort !env so in
      let scope = add_tyvar scope tvar in
      let new_var = get_tyvar scope tvar in
      (scope, (new_var, so)) in
    visit_cons scope env loc kx kt patt


  and intern_destr env scope loc destr =
    let mk _ = None in
    let destr = fill_out_private_of_destr env loc destr mk in
    let _,_,destr =  visit_destr scope env loc
        (fun vars valu -> (vars, intern_val vars valu))
        (fun vars typ ->  (vars, intern_type_annot env vars typ))
        (fun vars stk -> (vars, intern_stk vars stk))
        destr in
    destr

  and intern_copatt env scope loc copatt =
    let mk so = Some (TyVar.to_string (TyVar.fresh ()), Some so) in
    let copatt = fill_out_private_of_destr env loc copatt mk in
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
      let (tvar, so) = match bind with
        | None -> assert false
        | Some x -> x in
      let so = intern_sort !env so in
      let scope = add_tyvar scope tvar in
      let new_var = get_tyvar scope tvar in
      (scope, (new_var, so)) in
    visit_destr scope env loc kx kt ka copatt

  in

  let scope = empty_scope in

  let def' = match def with

    | Cst.Term_declaration {name; typ; loc} ->
      let var = Var.of_string name in
      Value_declaration {
        name = var;
        loc;
        typ = intern_type_annot env scope (Some typ);
        pol = Redirect (USortVar.fresh ())}

    | Cst.Term_definition {name; typ; content; loc} ->
      let var = Var.of_string name in
      Value_definition {
        name = var;
        typ = intern_type_annot env scope typ;
        content = intern_val scope content;
        loc;
        pol = Redirect (USortVar.fresh ())}

    | Cst.Cmd_execution {name; typ; content; loc; cont} ->
      let final_type = intern_type_annot env scope typ in
      let scope = add_covar scope cont in
      let var = match name with
        | Some name -> Var.of_string name
        | None -> Var.of_string "anon" in
      Command_execution {
        name = var;
        conttyp = final_type;
        cont = get_covar scope cont;
        content = intern_cmd scope content;
        loc;
        pol = Redirect (USortVar.fresh ())}

    | _ -> raise (Failure "FATAL Invariant break: in internalizer, \
                           a prelude definition has found its way \
                           in the term internalizer") in

  let declared_vars = match def, def' with
    | Cst.Term_declaration {name = old_name;_}, Value_declaration {name = new_name; _}
    | Cst.Term_definition {name = old_name; _}, Value_definition {name = new_name; _}->
      StringEnv.add old_name new_name declared_vars
    | _ -> declared_vars in

  (declared_vars, def', !env)