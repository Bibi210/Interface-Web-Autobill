open Vars
open Ast
open Constructors
open FullAst
open Prelude
open AlphaConversion

type runtime_env =  {
  covars : S.t CoVar.Env.t;
  vars : V.t Var.Env.t;
  tyvars : typ TyVar.Env.t;
  declared_vars : unit Var.Env.t;
  declared_covars : unit CoVar.Env.t;
  declared_tyvars : unit TyVar.Env.t;
  prelude : prelude;
  fixpoint_vars_are_reduced : bool Var.Env.t;
  shared_vars : unit Var.Env.t;
  always_reduce_fixpoints : bool;
  reduce_sharing : bool;
  reduce_commands : bool;
}

type runtime_prog = runtime_env * command

exception Internal_No_root_reduction

exception Box_kind_mismatch of runtime_prog

exception Malformed_program of runtime_prog

exception Malformed_case of runtime_prog

let curr (_, cmd) = cmd

let env (env, _) = env

let env_get env var =
  alpha_val empty_renaming (Var.Env.find var env.vars)

let env_declare env var = {env with declared_vars = Var.Env.add var () env.declared_vars}

let env_add env var valu =
  {env with vars = Var.Env.add var valu env.vars}

let env_set_fixpoint env var =
  {env with fixpoint_vars_are_reduced = Var.Env.add var false env.fixpoint_vars_are_reduced}

let env_is_fixpoint env var =
  Var.Env.mem var env.fixpoint_vars_are_reduced

let env_reduce_fixpoint env x =
  {env with fixpoint_vars_are_reduced = Var.Env.add x true env.fixpoint_vars_are_reduced}

let env_is_reduced_fixpoint env x =
  try Var.Env.find x env.fixpoint_vars_are_reduced with Not_found -> false

let env_set_shared env var =
  {env with shared_vars = Var.Env.add var () env.shared_vars}

let env_is_shared env var = Var.Env.mem var env.shared_vars

let typenv_get env var =
  alpha_typ empty_renaming (TyVar.Env.find var env.tyvars)

let typenv_add env var valu = {env with tyvars = TyVar.Env.add var valu env.tyvars}

let typenv_declare env var = {env with declared_tyvars = TyVar.Env.add var () env.declared_tyvars}

let coenv_get env covar =
  alpha_stk empty_renaming (CoVar.Env.find covar env.covars)

let coenv_add env covar stk = {env with covars = CoVar.Env.add covar stk env.covars}

let coenv_declare env var = {env with declared_covars = CoVar.Env.add var () env.declared_covars}

let fail_box_kind_mistatch cmd = raise (Box_kind_mismatch cmd)


let fail_malformed_program prog mess =
  Format.eprintf "%a@.%s@." PrettyPrinter.PP.pp_cmd (curr prog) mess;
  raise (Malformed_program prog)

let fail_malformed_case prog =raise (Malformed_case prog)

let rec reduct_match prog ((Raw_Cons cons) as c) patts default =
  match patts with
  | (Raw_Cons patt, cmd) :: patts ->
    if cons.tag = patt.tag then
      let env = List.fold_left2
          (fun env (x,_) v -> typenv_add env x v) (env prog) patt.idxs cons.idxs in
      let env = List.fold_left2
          (fun env (x,_) v -> env_add env x v) env patt.args cons.args in
      (env, cmd)
    else
      reduct_match prog c patts default
  | [] -> match default with
    | Some ((x,typ), cmd) -> (env_add (env prog) x (V.cons ~typ  c), cmd)
    | None -> raise Not_found


let rec reduct_comatch prog ((Raw_Destr destr) as d) copatts default
     =
  match copatts with
  | (Raw_Destr copatt, cmd) :: copatts ->
    if copatt.tag = destr.tag then
      let env = List.fold_left2
          (fun env (x,_) v -> typenv_add env x v) (env prog) copatt.idxs destr.idxs in
      let env = List.fold_left2
          (fun env (x,_) v -> env_add env x v) env copatt.args destr.args in
      let env = coenv_add env (fst copatt.cont) destr.cont in
      env, cmd
    else
      reduct_comatch prog d copatts default
  | [] ->  match default with
    | Some ((a,typ), cmd) -> (coenv_add (env prog) a (S.destr ~typ d), cmd)
    | None -> raise Not_found

let build_fixpoint_self env (_,t) (_,t') (Command cmd) =
  let b = CoVar.fresh () in
  let tfix = Types.fix t' in
  let self = V.box ~typ:t Exponential (b,tfix)
      (Command {pol = Types.Negative;
                valu = cmd.valu;
                stk = S.ret ~typ:tfix b;
                mid_typ = tfix;
                loc = Misc.dummy_pos}) in
  env, self


let reduct_head_once ((env, Command cmd) as prog) : runtime_prog =

  let (MetaVal v) = cmd.valu in
  let (MetaStack s) = cmd.stk in
  let v = v.node and s = s.node in
  match v,s with

  | Box {kind = kind1; bind = (a,_); cmd = mcmd1},
    CoBox {kind = kind2; stk = cont2} ->
    if kind1 <> kind2 then fail_box_kind_mistatch prog else
      (coenv_add env a cont2, mcmd1)

  | Cons cons, CoCons {cases; default; _} ->
    begin try reduct_match prog cons cases default
      with Not_found -> fail_malformed_case prog
    end

  | Destr {cases; default; _}, CoDestr destr ->
    begin try reduct_comatch prog destr cases default
      with Not_found -> fail_malformed_case prog
    end

  | Bindcc {pol = _; bind = (covar, _); cmd = mcmd1},
    CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    begin match cmd.pol with
      | Positive ->
        (coenv_add env covar cmd.stk, mcmd1)
      | Negative ->
        (env_add env var cmd.valu, mcmd2)
    end

  | (Fix _) as v,
    CoFix stk ->
    let v = alpha_preval empty_renaming v in
    let [@warning "-8"] Fix {self = (x,_) as self; cmd = curr'; cont = (a,_) as cont} = v in
    if env.always_reduce_fixpoints || not (env_is_reduced_fixpoint env x) then
      let env, self = build_fixpoint_self env self cont (Command cmd) in
      ((env_add (coenv_add (env_set_fixpoint env x) a stk) x self), curr')
    else
      raise Internal_No_root_reduction


  | Bindcc {pol = _; bind = (a,_); cmd = mcmd1}, _ ->
    (coenv_add env a cmd.stk, mcmd1)

  | _, CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    (env_add env var cmd.valu, mcmd2)

  | Var var, _ ->
    if Var.Env.mem var env.declared_vars
    || (env_is_reduced_fixpoint env var && not env.always_reduce_fixpoints)
    || (env_is_shared env var && not env.reduce_sharing)
    then raise Internal_No_root_reduction
    else begin try
        let env = if env_is_fixpoint env var then env_reduce_fixpoint env var else env in
        (env, Command {cmd with valu = env_get env var})
      with
        Not_found -> fail_malformed_program prog "undefined var"
    end

  | _, Ret a ->
    begin try
        (env, Command {cmd with stk = coenv_get env a})
      with
        Not_found ->
        if CoVar.Env.mem a env.declared_covars
        then raise Internal_No_root_reduction
        else fail_malformed_program prog "undefined continuation"
    end

  | CoTop, _ | _, CoZero -> raise Internal_No_root_reduction

  | _ -> raise Internal_No_root_reduction
(* | _ -> fail_malformed_program prog "incompatible val and stk" *)


let head_normal_form ?(verbose = false) prog =
  let pp (_, cmd) =
    if verbose then begin
      Format.fprintf
        Format.std_formatter
        "@[<v 0>@,HNF======================================================@,@]";
      PrettyPrinter.PP.pp_cmd Format.std_formatter cmd;
      Format.pp_print_cut Format.std_formatter ();
      Format.pp_print_flush Format.std_formatter ()
    end in
  let prog = ref prog in
    let rec loop () =
    prog := reduct_head_once !prog;
    pp !prog;
    loop () in
  try pp !prog; loop () with Internal_No_root_reduction -> !prog
