open Vars
open Ast
open FullAst
open HeadNormalForm
open NormalForm

let prim_vars =
  let open Primitives in
  let vs = [
    op_add; op_sub; op_mul; op_div; op_mod; op_op;
    op_lt; op_leq; op_eq;
    op_and; op_or; op_not
  ] in
  List.fold_left (fun xs x -> Var.Env.add x () xs) Var.Env.empty vs

let visit_prog
    (run_command : ?declared_vars:unit Var.Env.t
     -> ?declared_covars:unit CoVar.Env.t
     -> ?declared_tyvars:unit TyVar.Env.t
     -> ?vars:meta_value Var.Env.t
     -> Prelude.prelude -> command -> command)
    (prog : FullAst.program) =

  let do_once declared_vars vars prog_item = match prog_item with

    | Value_declaration {bind = (name, _); _} ->
      (Var.Env.add name () declared_vars, vars, prog_item)

    | Value_definition def ->
      let a = CoVar.fresh () in
      let (name, typ) = def.bind in
      let cmd = FullAst.Command
          {pol = def.pol;
           loc = def.loc;
           mid_typ = typ;
           valu = def.content;
           stk = S.ret ~typ a;
          } in
      let declared_covars = CoVar.Env.singleton a () in
      let Command cmd = run_command ~declared_vars ~declared_covars ~vars prog.prelude cmd in
      let valu = MetaVal {
          node = Bindcc {bind = (a, typ); pol = def.pol; cmd = Command cmd};
          val_typ = typ;
          loc = def.loc} in
      (declared_vars,
       Var.Env.add name valu vars,
       Value_definition {def with content = valu}) in

  let do_cmd declared_vars vars = function
    | Some (Command_execution exec) ->
      let declared_covars = CoVar.Env.singleton exec.cont () in
      let cmd = run_command ~declared_vars ~declared_covars ~vars prog.prelude exec.content in
      (declared_vars,
       vars,
       Some (Command_execution {exec with content = cmd}))
    | None -> (declared_vars, vars, None) in

let rec loop declared env prog_items =
  match prog_items with
  | [] -> (declared, env, [])
    | h :: t ->
      let declared, env, h = do_once declared env h in
      let declared, env, t = loop declared env t in
      declared, env, h::t in

  let declared = Var.Env.empty in
  let env = Var.Env.empty in
  let declared, env, declarations = loop declared env prog.declarations in
  let _, _, command = do_cmd declared env prog.command in
  {prog with declarations; command}

let normal_form_visitor
    ?reduce_fixpoints:(fixpoints=false)
    ?reduce_sharing:(share=false)
    ?reduce_commands:(reduce_cmd=true)
    ?declared_vars:(declared_vars = Var.Env.empty)
    ?declared_covars:(declared_covars = CoVar.Env.empty)
    ?declared_tyvars:(declared_tyvars = TyVar.Env.empty)
    ?vars:(vars = Var.Env.empty)
    prelude cmd =
  let env =  {declared_vars;
              declared_covars;
              declared_tyvars;
              vars;
              covars = CoVar.Env.empty;
              tyvars = TyVar.Env.empty;
              fixpoint_vars_are_reduced = Var.Env.empty;
              shared_vars = Var.Env.empty;
              prelude;
              reduce_sharing = share;
              always_reduce_fixpoints = fixpoints;
              reduce_commands = reduce_cmd} in
  cmd_nf env cmd

let head_normal_form_visitor
    ?reduce_fixpoints:(fixpoints=true)
    ?reduce_sharing:(share=true)
    ?declared_vars:(declared_vars = Var.Env.empty)
    ?declared_covars:(declared_covars = CoVar.Env.empty)
    ?declared_tyvars:(declared_tyvars = TyVar.Env.empty)
    ?vars:(vars = Var.Env.empty)
    prelude cmd =
  let env = {declared_vars;
             declared_covars;
             declared_tyvars;
             vars;
             covars = CoVar.Env.empty;
             tyvars = TyVar.Env.empty;
             fixpoint_vars_are_reduced = Var.Env.empty;
             shared_vars = Var.Env.empty;
             prelude;
             reduce_commands = true;
             reduce_sharing = share;
             always_reduce_fixpoints = fixpoints} in
  let env, cmd = head_normal_form (env, cmd) in
  let env = {env with reduce_sharing = true;
                      always_reduce_fixpoints = false;
                      reduce_commands = true} in
  cmd_nf env cmd

let interpreter_visitor
    ?declared_tyvars:(declared_tyvars = TyVar.Env.empty)
    ?vars:(vars = Var.Env.empty)
    prelude cmd =
  let env = {declared_vars = prim_vars;
             declared_covars = CoVar.Env.empty;
             declared_tyvars;
             vars;
             covars = CoVar.Env.empty;
             tyvars = TyVar.Env.empty;
             fixpoint_vars_are_reduced = Var.Env.empty;
             shared_vars = Var.Env.empty;
             prelude;
             reduce_commands = true;
             reduce_sharing = true;
             always_reduce_fixpoints = true} in
  let env, cmd = head_normal_form (env, cmd) in
  let cmd = ReductPrimitives.go cmd in
  let env = {env with reduce_sharing = true;
                      always_reduce_fixpoints = false;
                      reduce_commands = true} in
  cmd_nf env cmd


let simplify_untyped_prog prog = visit_prog
    (normal_form_visitor
       ~reduce_commands:true
       ~reduce_fixpoints:false
       ~reduce_sharing:false)
    prog

let interpret_prog prog =
  visit_prog (head_normal_form_visitor ~reduce_fixpoints:true ~reduce_sharing:true) prog
