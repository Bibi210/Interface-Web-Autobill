open Misc
open Vars
open Intern_common
open Types
open Ast
open InternAst

let export_ast env item =

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
    | Arrow _
    | Index _ -> fail_ambiguous_sort (Option.value loc ~default:dummy_pos)

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
        with Not_found -> assert false in
      env.prelude :=
        {!(env.prelude) with
         sorts = TyVar.Env.add node (export_usort uso) !(env.prelude).sorts
        };
      typ
    | TPos typ -> export_typ typ
    | TNeg typ -> export_typ typ
    | TFix t -> TFix (export_typ t)
    | TBox {kind;node;loc} -> TBox {kind; loc; node = export_typ node}
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
      final_typ = export_typ stk.final_typ;
      loc = stk.loc
    }

  and export_cmd (Command cmd) = FullAst.Command {
      valu = export_meta_val cmd.valu;
      pol = export_upol ~loc:cmd.loc cmd.pol;
      stk = export_meta_stk cmd.stk;
      mid_typ = export_typ cmd.mid_typ;
      final_typ = export_typ cmd.final_typ;
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
    | Destr copatts ->
      FullAst.Destr
        (List.map (fun (copatt, cmd) -> (export_copatt copatt, export_cmd cmd))
           copatts)
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
    | CoCons patts ->
      FullAst.CoCons
        (List.map (fun (patt, cmd) -> (export_patt patt, export_cmd cmd))
        patts)
    | CoFix stk -> CoFix (export_meta_stk stk)



  and export_cons cons = match cons with
    | Unit -> Unit
    | Bool b -> Bool b
    | Int n -> Int n
    | Inj (i,n,x) -> Inj (i,n,export_meta_val x)
    | Thunk x -> Thunk (export_meta_val x)
    | Tupple xs -> Tupple (List.map export_meta_val xs)
    | PosCons (cons, typs, args) ->
      PosCons (cons,
               List.map export_typ typs,
               List.map export_meta_val args)

  and export_destr destr = match destr with
    | Call (xs,a) -> Call (List.map export_meta_val xs, export_meta_stk a)
    | Proj (i,n,a) -> Proj (i,n,export_meta_stk a)
    | Closure a -> Closure (export_meta_stk a)
    | NegCons (cons, typs, args, cont) ->
      NegCons (cons,
               List.map export_typ typs,
               List.map export_meta_val args,
               export_meta_stk cont)

  and export_patt = function
    | Int n -> Int n
    | Bool b -> Bool b
    | Unit -> Unit
    | Inj(i,n,x) -> Inj (i,n,export_bind x)
    | Thunk x -> Thunk (export_bind x)
    | Tupple xs -> Tupple (List.map export_bind xs)
    | PosCons (cons, typs, args) ->
      PosCons (cons,
               List.map export_typebind typs,
               List.map export_bind args)

  and export_copatt = function
    | Call (xs,a) -> Call (List.map export_bind xs,
                           export_cobind a)
    | Proj (i,n,a) -> Proj (i,n,export_cobind a)
    | Closure a -> (Closure (export_cobind a))
    | NegCons (cons, typs, args, cont) ->
      NegCons (cons,
               List.map export_typebind typs,
               List.map export_bind args,
               export_cobind cont)
  in

  let def = match item with
    | InternAst.Value_declaration {name; typ; pol; loc} ->
      FullAst.Value_declaration
        {typ = export_typ typ;
         pol = export_upol ~loc pol;
         name; loc}
    | InternAst.Value_definition {name; typ; pol; content; loc} ->
      FullAst.Value_definition {name; loc;
                                typ = export_typ typ;
                                pol = export_upol ~loc pol;
                                content = (export_meta_val content)}
    | Command_execution {name; pol; content; cont; conttyp; loc} ->
      Command_execution {pol = export_upol ~loc pol;
                         content = export_cmd content;
                         name;
                         conttyp = export_typ conttyp;
                         cont; loc} in

  def, env
