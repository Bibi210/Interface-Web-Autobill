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

let fill_out_private_of_cons env (Raw_Cons cons) mk =
  let Consdef {constructor = Raw_Cons def; _} = def_of_cons !env.prelude cons.tag in
  let rec go priv typs = match priv, typs with
    | [], [] -> []
    | (_,so)::t, [] -> (mk so)::(go t [])
    | (_,so)::t, None::t' -> (mk so)::(go t t')
    | _::t, Some h::t' -> h :: (go t t')
    | [], _::_ -> assert false in
  Raw_Cons {cons with idxs = go def.idxs cons.idxs}

let fill_out_private_of_destr env (Raw_Destr destr) mk =
  let Destrdef {destructor = Raw_Destr def; _} =
    def_of_destr !env.prelude destr.tag in
  let rec go priv typs = match priv, typs with
    | [], [] -> []
    | (_,so)::t, [] -> (mk so)::(go t [])
    | (_,so)::t, None::t' -> (mk so)::(go t t')
    | _::t, Some h::t' -> h :: (go t t')
    | [], _::_ -> assert false in
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

    | Cst.Destr {cases; default; loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      let go_one (destr, cmd) =
        let scope, destr = intern_copatt env scope loc destr in
        let cmd = intern_cmd scope cmd in
        (destr, cmd) in
      let cases = List.map go_one cases in
      let default = match default with
        | None -> None
        | Some ((name, typ), cmd) ->
          let typ = intern_type_annot env scope typ in
          let scope = add_covar scope name in
          let name = get_covar scope name in
          let cmd = intern_cmd scope cmd in
          Some ((name, typ), cmd) in
      MetaVal {loc; val_typ; node = Destr {cases; default}}

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
      let valu = intern_val scope valu in
      let stk = intern_stk scope stk in
      let pol = intern_pol pol in
      Command {mid_typ; loc; valu; stk; pol}


  and intern_stk scope stk =
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
          cmd = intern_cmd scope cmd
        }}

    | CoBox {kind; stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let node = CoBox {kind; stk = intern_stk scope stk} in
      MetaStack {loc; cont_typ; node}

    | CoCons {cases; default; loc} ->
      let go_one (cons, cmd) =
        let scope, cons = intern_patt env scope loc cons in
        let cmd = intern_cmd scope cmd in
        (cons, cmd) in
      let default = match default with
        | None -> None
        | Some ((name, typ), cmd) ->
          let typ = intern_type_annot env scope typ in
          let scope = add_var scope name in
          let name = get_var scope name in
          let cmd = intern_cmd scope cmd in
          Some ((name, typ), cmd) in
      let cases = List.map go_one cases in
      MetaStack {loc;
                 cont_typ = tvar (TyVar.fresh ());
                 node = CoCons {cases; default}
                }

    | CoDestr {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      MetaStack {loc; cont_typ; node = CoDestr (intern_destr env scope loc node)}

    | CoFix {stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let stk = intern_stk scope stk in
      MetaStack {loc; cont_typ; node = CoFix stk}


  and intern_cons env vars loc cons =
    let mk _ = tvar (TyVar.fresh ()) in
    let _, cons = visit_cons vars env loc
        (fun vars valu -> (vars, intern_val vars valu))
        (fun vars typ ->  vars, Some (intern_type_annot env vars typ))
        cons in
    fill_out_private_of_cons env cons mk

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
        let so = intern_sort !env so in
        let scope = add_tyvar scope tvar in
        let new_var = get_tyvar scope tvar in
        (scope, Some (new_var, so)) in
    let scope, patt = visit_cons scope env loc kx kt patt in
    scope, fill_out_private_of_cons env patt mk



  and intern_destr env scope loc destr =
    let mk _ = tvar (TyVar.fresh ()) in
    let _,destr = visit_destr scope env loc
        (fun vars valu -> (vars, intern_val vars valu))
        (fun vars typ ->  vars, Some (intern_type_annot env vars typ))
        (fun vars stk -> (vars, intern_stk vars stk))
        destr in
    fill_out_private_of_destr env destr mk

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
        let so = intern_sort !env so in
        let scope = add_tyvar scope tvar in
        let new_var = get_tyvar scope tvar in
        (scope, Some (new_var, so)) in
    let scope, copatt = visit_destr scope env loc kx kt ka copatt in
    scope, fill_out_private_of_destr env copatt mk

  in

  let scope = empty_scope in

  let def' = match def with

    | Cst.Term_declaration {name; typ; loc} ->
      let var = Var.of_string name in
      Value_declaration {
        bind = (var,  intern_type_annot env scope (Some typ));
        loc;
        pol = Redirect (USortVar.fresh ())}

    | Cst.Term_definition {name; typ; content; loc} ->
      let var = Var.of_string name in
      Value_definition {
        bind = (var, intern_type_annot env scope typ);
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
    | Cst.Term_declaration {name = old_name;_}, Value_declaration {bind = (new_name, _); _}
    | Cst.Term_definition {name = old_name; _}, Value_definition {bind = (new_name, _); _}->
      StringEnv.add old_name new_name declared_vars
    | _ -> declared_vars in

  (declared_vars, def', !env)
