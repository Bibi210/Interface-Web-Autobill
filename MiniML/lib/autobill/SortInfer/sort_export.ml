open Misc
open Vars
open Types
open Preprocess_ast
open Ast
open SortInfer

let export_ast env prog =

  let rec export_usort ?loc = function
    | Litt p -> p
    | Loc (loc', x) ->
      let loc = if loc' <> dummy_pos then Some loc' else loc in
      export_usort ?loc x
    | Redirect var ->
      try export_usort ?loc (SortInfer.get env var)
      with Not_found -> fail_ambiguous_sort (Option.value loc ~default:dummy_pos)

  and export_upol ?loc uso =
    match export_usort ?loc uso with
    | Base p -> p
    | Arrow _ | Index _ -> fail_ambiguous_sort (Option.value loc ~default:dummy_pos)

  and export_bind (var, typ) =
    let typ = export_typ typ in
    env.prelude := {!(env.prelude) with vars = Var.Env.add var typ !(env.prelude).vars};
    (var, typ)

  and export_cobind (covar, typ) =
    let typ = export_typ typ in
    env.prelude := {!(env.prelude) with covars = CoVar.Env.add covar typ !(env.prelude).covars};
    (covar, typ)

  and export_typebind (tyvar, uso) =
    let so = export_usort uso in
    (env.prelude) := {!(env.prelude) with
                      sorts = TyVar.Env.add tyvar so !(env.prelude).sorts};
    (tyvar, so)


  and export_typ typ = match typ with
    | TVar {node;_} | TInternal node ->
      let uso =
        try TyVar.Env.find node env.tyvarsorts
        with Not_found ->
          fail_invariant_break "An base type variable has no sort after polarity inference"
      in
      env.prelude := {
        !(env.prelude) with
        sorts = TyVar.Env.add node (export_usort uso) !(env.prelude).sorts
      };
      typ
    | TCons c -> TCons c
    | TApp {tfun;args;loc} ->
      TApp {tfun = export_typ tfun; args = List.map export_typ args; loc}

  and export_meta_val (MetaVal v) = FullAst.MetaVal {
      node = export_val v.loc v.node;
      val_typ = export_typ v.val_typ;
      loc = v.loc
    }

  and export_meta_stk (MetaStack stk) = FullAst.MetaStack {
      node = export_stk stk.loc stk.node;
      cont_typ = export_typ stk.cont_typ;
      loc = stk.loc
    }

  and export_cmd (Command cmd) = FullAst.Command {
      valu = export_meta_val cmd.valu;
      pol = export_upol ~loc:cmd.loc cmd.pol;
      stk = export_meta_stk cmd.stk;
      mid_typ = export_typ cmd.mid_typ;
      loc = cmd.loc
    }

  and export_val loc = function
    | Var v -> FullAst.Var v
    | CoTop -> FullAst.CoTop
    | Bindcc {bind; pol; cmd} ->
      let upol = export_upol ~loc pol in
      let bind = export_cobind bind in
      FullAst.Bindcc {bind = bind; pol = upol; cmd = export_cmd cmd}
    | Box {kind; bind; cmd} ->
      let bind = export_cobind bind in
      FullAst.Box {kind; bind; cmd = export_cmd cmd}
    | Cons cons -> FullAst.Cons (export_cons cons)
    | Destr {default; cases; for_type} -> FullAst.Destr {
        for_type;
        cases =
          List.map (fun (copatt, cmd) -> (export_copatt copatt, export_cmd cmd)) cases;
        default = Option.map (fun (a,cmd) -> (export_cobind a, export_cmd cmd)) default
      }
    | Fix {self; cmd; cont} ->
      let self = export_bind self in
      let cmd = export_cmd cmd in
      let cont = export_cobind cont in
      Fix {self; cmd; cont}


  and export_stk loc = function
    | Ret a -> FullAst.Ret a
    | CoZero -> FullAst.CoZero
    | CoBind {bind; pol; cmd} ->
      let bind = export_bind bind in
      let pol = export_upol ~loc pol in
      FullAst.CoBind {bind; pol; cmd = export_cmd cmd}
    | CoBox {kind; stk} -> FullAst.CoBox {kind; stk = export_meta_stk stk}
    | CoDestr destr -> FullAst.CoDestr (export_destr destr)
    | CoCons {default; cases; for_type} ->
      FullAst.CoCons {
        for_type;
        cases = List.map (fun (patt, cmd) -> (export_patt patt, export_cmd cmd))
            cases;
        default = Option.map (fun (a,cmd) -> (export_bind a, export_cmd cmd)) default
      }
    | CoFix stk -> CoFix (export_meta_stk stk)



  and export_cons (Raw_Cons c) = Raw_Cons {
      tag = c.tag;
      idxs = List.map export_typ c.idxs;
      args = List.map export_meta_val c.args}

  and export_destr (Raw_Destr d) = Raw_Destr {
      tag = d.tag;
      idxs = List.map export_typ d.idxs;
      args = List.map export_meta_val d.args;
      cont = export_meta_stk d.cont}

  and export_patt (Raw_Cons patt) = Raw_Cons {
      tag = patt.tag;
      idxs = List.map export_typebind patt.idxs;
      args = List.map export_bind patt.args
    }

  and export_copatt (Raw_Destr copatt) = Raw_Destr {
      tag = copatt.tag;
      idxs = List.map export_typebind copatt.idxs;
      args = List.map export_bind copatt.args;
      cont = export_cobind copatt.cont
    }

  in

  let export_def env item =
    let def = match item with
      | InternAst.Value_declaration {bind = (name, typ); pol; loc} ->
        FullAst.Value_declaration {
          bind = (name, export_typ typ);
          pol = export_upol ~loc pol;
          loc}
      | InternAst.Value_definition {bind = (name, typ); pol; content; loc} ->
        FullAst.Value_definition {
          loc;
          bind = (name, export_typ typ);
          pol = export_upol ~loc pol;
          content = (export_meta_val content)}
    in env, def in

  let export_exec env exec =
    let exec = match exec with
    | Some (Command_execution {name; pol; content; cont; conttyp; loc}) ->
      Some (FullAst.Command_execution {
        pol = export_upol ~loc pol;
        content = export_cmd content;
        name;
        conttyp = export_typ conttyp;
        cont; loc})
    | None -> None
  in env, exec

  in

  let env, declarations = List.fold_left_map export_def env prog.declarations in
  let env, command = export_exec env prog.command in
  let goal = match prog.goal with
    | None -> None
    | Some (Goal {polynomial; degree; args_number}) ->
      Some FullAst.(Goal {polynomial; degree; args_number}) in

  FullAst.{
    declarations;
    command;
    goal;
    prelude = env.prelude
  }
