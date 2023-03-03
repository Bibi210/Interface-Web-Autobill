open Vars
open Ast
open Constructors
open FullAst
open Prelude

type runtime_prog =  {
  cont : S.t CoVar.Env.t;
  env : V.t Var.Env.t;
  typs : typ TyVar.Env.t;
  declared : unit Var.Env.t;
  declared_cont : unit CoVar.Env.t;
  prelude : prelude;
  curr : command;
  reduce_fixpoints : bool;
  reduce_sharing : bool;
}

exception Internal_No_root_reduction

exception Box_kind_mismatch of runtime_prog

exception Malformed_program of runtime_prog

exception Malformed_case of runtime_prog

let env_get env var = Var.Env.find var env

let env_add_subst env var valu = Var.Env.add var valu env

let typ_get env var = TyVar.Env.find var env

let typ_add_subst env var valu = TyVar.Env.add var valu env

let coenv_get env covar = CoVar.Env.find covar env

let coenv_add_subst env covar stk = CoVar.Env.add covar stk env

let fail_box_kind_mistatch cmd = raise (Box_kind_mismatch cmd)

let fail_malformed_program cmd mess =
  Format.print_string mess;
  Format.print_newline ();
  PrettyPrinter.PP.pp_cmd Format.err_formatter cmd.curr;
  raise (Malformed_program cmd)

let fail_malformed_case prog =raise (Malformed_case prog)


let rec reduct_match prog cons patts = match cons, patts with

  | Unit, (Unit, cmd)::_ ->
    {prog with curr = cmd}

  | Thunk v, (Thunk (x,_), cmd)::_ ->
    {prog with curr = cmd; env = env_add_subst prog.env x v}

  | Tupple vs, (Tupple vars, cmd)::_ ->
    let env = List.fold_left2 env_add_subst prog.env (List.map fst vars) vs in
    {prog with env; curr = cmd}

  | Inj (i1,n1,v), (Inj (i2,n2,(x,_)), cmd)::_ when  (i1,n1) = (i2,n2) ->
    {prog with curr = cmd; env = env_add_subst prog.env x v}

  | PosCons (cons, typs, args), (PosCons (cons', tyvars, vars), cmd)::_ when cons = cons' ->
    let typs = List.fold_left2
        (fun env (x,_) v -> typ_add_subst env x v) prog.typs tyvars typs in
     let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) prog.env vars args in
    {prog with env; typs; curr = cmd}

  | _, _::t -> reduct_match prog cons t

  | _, [] -> raise Not_found


let rec reduct_comatch prog copatts destr = match destr, copatts with
  | Call (vs,s), (Call (vars,(a,_)),cmd)::_ ->
    let env = List.fold_left2 env_add_subst prog.env (List.map fst vars) vs in
    {prog with env; curr = cmd; cont = coenv_add_subst prog.cont a s}

  | Proj (i1,n1,s), (Proj (i2,n2,(a,_)), cmd)::_ when (i1,n1) = (i2,n2) ->
    {prog with curr = cmd; cont = coenv_add_subst prog.cont a s}

  | Closure s, (Closure (a,_), cmd)::_ ->
    {prog with curr = cmd; cont = coenv_add_subst prog.cont a s}

  | NegCons (cons, typs, args, s), (NegCons (cons', tyvars, vars, (a,_)), cmd)::_ when cons = cons' ->
    let typs = List.fold_left2
        (fun env (x,_) v -> typ_add_subst env x v) prog.typs tyvars typs in
    let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) prog.env vars args in
    let cont = coenv_add_subst prog.cont a s in
    {prog with typs; env; cont; curr = cmd }

  | _, _::t -> reduct_comatch prog t destr

  | _, [] -> raise Not_found


let reduct_head_once prog : runtime_prog =

  let (Command cmd) = prog.curr in
  let (MetaVal v) = cmd.valu in
  let (MetaStack s) = cmd.stk in
  let v = v.node and s = s.node in
  match v,s with

  | Box {kind = kind1; bind = (a,_); cmd = mcmd1},
    CoBox {kind = kind2; stk = cont2}
    when prog.reduce_sharing
    ->
    if kind1 <> kind2 then fail_box_kind_mistatch prog;
    { prog with cont = coenv_add_subst prog.cont a cont2; curr = mcmd1}


  | Cons cons1, CoCons patts2 ->
    begin try reduct_match prog cons1 patts2
      with Not_found -> fail_malformed_case prog
    end

  | Destr copatts1, CoDestr destr2 ->
    begin try reduct_comatch prog copatts1 destr2
      with Not_found -> fail_malformed_case prog
    end

  | Bindcc {pol = _; bind = (covar, _); cmd = mcmd1},
    CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    begin match cmd.pol with
    | Positive ->
      {prog with cont = coenv_add_subst prog.cont covar cmd.stk; curr = mcmd1}
    | Negative ->
      {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}
    end

  | (Fix {self=(x,t); cmd = curr'; cont=(a,t')}),
    CoFix stk
    when prog.reduce_fixpoints ->
    let self = V.box Types.exp (a,t')
        (Command {pol = Types.Negative;
                  valu = cmd.valu;
                  stk = S.ret a;
                  mid_typ = t;
                  final_typ = t;
                  loc = Misc.dummy_pos}) in
    {prog with env = env_add_subst prog.env x self;
               curr = curr';
               cont = coenv_add_subst prog.cont a stk}


  | Bindcc {pol = _; bind = (a,_); cmd = mcmd1}, _ ->
    {prog with cont = coenv_add_subst prog.cont a cmd.stk; curr = mcmd1}

  | _, CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}

  | Var var, _ ->
    begin try
        {prog with curr = Command {cmd with valu = env_get prog.env var}}
      with
        Not_found ->
        if Var.Env.mem var prog.declared
        then raise Internal_No_root_reduction
        else fail_malformed_program prog "undefined var"
    end

  | _, Ret a ->
    begin try
        {prog with curr = Command {cmd with stk = coenv_get prog.cont a}}
      with
        Not_found ->
        if CoVar.Env.mem a prog.declared_cont
        then raise Internal_No_root_reduction
        else fail_malformed_program prog "undefined continuation"
    end

  | CoTop, _ | _, CoZero -> raise Internal_No_root_reduction

  | _ -> fail_malformed_program prog "incompatible val and stk"

let head_normal_form prog =
  let prog = ref prog in
  let rec loop () =
    prog := reduct_head_once !prog;
    loop () in
  try loop () with Internal_No_root_reduction -> !prog
