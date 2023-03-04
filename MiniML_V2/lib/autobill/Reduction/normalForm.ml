open Ast
open Constructors
open FullAst
open Prelude
open HeadNormalForm
open Types

let rec typ_nf env (t:typ) = match t with
  | TCons _ -> t
  | TVar {node=x;_} | TInternal x -> begin
      try typenv_get env x with _ -> t
    end
  | TApp {tfun; args; loc} ->
    let tfun = typ_nf env tfun in
    let args = List.map (typ_nf env) args in
    match tfun with
    | TCons {node;_} ->
      let def = def_of_tycons env.prelude node in
      begin match def.content with
      | Defined typ ->
        let env = List.fold_left2
            (fun env (x,_) t -> typenv_add env x t)
            env def.args args in
        typ_nf env typ
      | _ -> TApp {tfun; args; loc}
      end
    | _ -> TApp {tfun; args; loc}

let bind_nf env (x,t) =
  let env = env_declare env x in
  (env, (x, typ_nf env t))

let cobind_nf env (a,t) =
  let env = coenv_declare env a in
  (env, (a, typ_nf env t))

let typbind_nf env (t,so) =
  let env = typenv_declare env t in
  (env, (t, so))

let rec val_nf env v = match v with
  | Var x ->
    if Vars.Var.Env.mem x env.declared_vars
    || (env_is_reduced_fixpoint env x && not env.always_reduce_fixpoints)
    || (env_is_shared env x && not env.reduce_sharing)
    then v
    else (try val_nf env (let MetaVal v = env_get env x in v.node)
          with Not_found -> v)
  | CoTop -> CoTop
  | Bindcc {bind; pol; cmd} ->
    let env, bind = cobind_nf env bind in
    let cmd' = cmd_nf env cmd in
    eta_reduce_bindcc
      (Bindcc {bind; pol; cmd = cmd'})
  | Box {bind; kind; cmd} ->
    let env, bind = cobind_nf env bind in
    Box {bind; kind; cmd = cmd_nf env cmd}
  | Cons cons -> Cons (cons_nf env cons)
  | Destr {cases; default} -> Destr {
      cases = List.map (copatt_nf env) cases;
      default = Option.map (fun (a,cmd) ->
          let env, a =  cobind_nf env a in (a, cmd_nf env cmd))
          default;
    }
  | Fix {cmd; self; cont} ->
    let env, self = bind_nf env self in
    let env, cont = cobind_nf env cont in
    Fix{self; cont ; cmd = cmd_nf env cmd}

and stack_nf env stk = match stk with
  | Ret a ->
    (try stack_nf env (let MetaStack s = coenv_get env a in s.node)
     with Not_found -> stk)
  | CoZero -> CoZero
  | CoBind {bind; pol; cmd} ->
    let env, bind = bind_nf env bind in
    let cmd = cmd_nf env cmd in
    eta_reduce_bind (CoBind {bind; pol; cmd})
  | CoBox {kind; stk} -> CoBox {kind; stk = metastack_nf env stk}
  | CoDestr destr -> CoDestr (destr_nf env destr)
  | CoCons {cases; default} -> CoCons {
      cases = List.map (patt_nf env) cases;
      default = Option.map (fun (x,cmd) ->
          let env, x = bind_nf env x in (x, cmd_nf env cmd))
          default;
    }

  | CoFix stk -> CoFix (metastack_nf env stk)

and cons_nf prog (Raw_Cons cons) = Raw_Cons {
    tag = cons.tag;
    idxs = List.map (typ_nf prog) cons.idxs;
    args = List.map (metaval_nf prog) cons.args;
  }

and destr_nf prog (Raw_Destr cons) = Raw_Destr {
    tag = cons.tag;
    idxs = List.map (typ_nf prog) cons.idxs;
    args = List.map (metaval_nf prog) cons.args;
    cont = metastack_nf prog cons.cont
  }

and patt_nf env (patt, cmd) =
  let Raw_Cons { tag; idxs; args } = patt in
  let env, idxs = List.fold_left_map
      (fun env (x,t) -> typenv_declare env x, (x,t)) env idxs in
  let env, args = List.fold_left_map
      (fun env (x,t) -> (env_declare env x, (x, typ_nf env t))) env args in
  let cmd = cmd_nf env cmd in
  (Raw_Cons {tag; idxs; args}, cmd)

and copatt_nf env (copatt, cmd) =
  let Raw_Destr { tag; idxs; args; cont } = copatt in
  let env, idxs = List.fold_left_map typbind_nf env idxs in
  let env, args = List.fold_left_map bind_nf env args in
  let env, cont = cobind_nf env cont in
  let cmd = cmd_nf env cmd in
  (Raw_Destr {tag; idxs; args; cont}, cmd)

and metaval_nf prog (MetaVal v) =
  MetaVal {
    node = val_nf prog v.node;
    val_typ = typ_nf prog v.val_typ;
    loc = v.loc}

and metastack_nf prog (MetaStack s) =
  MetaStack {
    node = stack_nf prog s.node;
    cont_typ = typ_nf prog s.cont_typ;
    loc = s.loc}

and cmd_nf env cmd =
  let pp ?(verbose = false) (_, cmd) =
    if verbose then begin
      Format.fprintf
        Format.std_formatter
        "@[<v 0>@,NF======================================================@,@]";
      PrettyPrinter.PP.pp_cmd Format.std_formatter cmd;
      Format.pp_print_cut Format.std_formatter ();
      Format.pp_print_flush Format.std_formatter ()
    end in
  let (env, cmd) = if env.reduce_commands then head_normal_form (env, cmd) else (env, cmd) in
  let (Command cmd) = cmd in
  let cmd = Command
      {loc = cmd.loc; pol = cmd.pol;
       valu = metaval_nf env cmd.valu;
       stk = metastack_nf env cmd.stk;
       mid_typ = typ_nf env cmd.mid_typ} in
  pp (env, cmd); cmd

and eta_reduce_bindcc valu = match valu with
  | Bindcc { cmd = Command cmd; bind = (a,_); _} ->
    begin match cmd.stk with
      | MetaStack {node = Ret b; _} when a = b->
        let MetaVal v = cmd.valu in v.node
      | _ -> valu
    end
  | _ -> valu

and eta_reduce_bind stk = match stk with
  | CoBind {bind = (x,_); cmd = Command cmd; _} ->
    begin match cmd.valu with
      | MetaVal {node = Var y; _} ->
        if x = y then let MetaStack s = cmd.stk in s.node
        else stk
      | _ -> stk
    end
  | _ -> stk
