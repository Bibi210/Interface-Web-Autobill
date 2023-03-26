open Vars
open Ast
open Constructors
open FullAst
open Prelude
open HeadNormalForm
open Types

let rec typ_nf prog (t:typ) = match t with
  | TCons _ -> t
  | TBox b -> TBox {b with node = typ_nf prog b.node}
  | TFix f -> TFix (typ_nf prog f)
  | TPos t | TNeg t -> typ_nf prog t
  | TVar {node=x;_} | TInternal x -> begin
      match try Some (typ_get prog.typs x) with _ -> None with
      | None -> t
      | Some t -> typ_nf prog t
    end
  | TApp {tfun; args; loc} ->
    let tfun = typ_nf prog tfun in
    let args = List.map (typ_nf prog) args in
    match tfun with
    | TCons {node=Cons c;_} ->
      let def = def_of_tycons prog.prelude c in
      begin match def.content with
      | Defined typ ->
        let typs = List.fold_left2
            (fun env (x,_) t -> typ_add_subst env x t)
            prog.typs def.args args in
        typ_nf {prog with typs} typ
      | _ -> TApp {tfun; args; loc}
      end
    | _ -> TApp {tfun; args; loc}

and bind_nf prog (x,t) = (x, typ_nf prog t)

let rec val_nf prog v = match v with
  | Var x ->
    begin match Var.Env.find_opt x prog.env with
      | Some (MetaVal v) -> val_nf prog v.node
      | None -> v
      end
  | CoTop -> CoTop
  | Bindcc {bind; pol; cmd} ->
    let prog' = cmd_nf {prog with curr = cmd} in
    let bind = bind_nf prog bind in
    eta_reduce_bindcc (Bindcc {bind; pol; cmd = prog'.curr})
  | Box {bind; kind; cmd} ->
    let bind = bind_nf prog bind in
    let prog' = cmd_nf {prog with curr = cmd} in
    Box {bind; kind; cmd = prog'.curr}
  | Cons cons -> Cons (cons_nf prog cons)
  | Destr copatts -> Destr (List.map (copatt_nf prog) copatts)
  | Fix {cmd; self; cont} ->
    let self = bind_nf prog self in
    (* NOTE the normal form of the body is obtained with a *free* variable x,
       since we do not substitute "self" when it is in scope. *)
    let prog' = cmd_nf {prog with curr = cmd;
                                  declared = Var.Env.add (fst self) () prog.declared} in
    Fix{self=self; cmd = prog'.curr; cont}

and stack_nf prog stk = match stk with
  | Ret a ->
    begin try
        let MetaStack stk' = CoVar.Env.find a prog.cont in
        stack_nf prog stk'.node
      with _ -> stk
    end
  | CoZero -> CoZero
  | CoBind {bind; pol; cmd} ->
    let prog' =
      cmd_nf {prog with
              curr = cmd;
              declared = Var.Env.add (fst bind) () prog.declared} in
    let bind = bind_nf prog bind in
    eta_reduce_bind (CoBind {bind; pol; cmd = prog'.curr})
  | CoBox {kind; stk} -> CoBox {kind; stk = metastack_nf prog stk}
  | CoDestr destr -> CoDestr (destr_nf prog destr)
  | CoCons patts -> CoCons (List.map (patt_nf prog) patts)
  | CoFix stk -> CoFix (metastack_nf prog stk)

and cons_nf prog cons = match cons with
  | Unit | Int _ | Bool _ -> cons
  | Thunk v -> Thunk (metaval_nf prog v)
  | Tupple vs ->
    Tupple (List.map (metaval_nf prog) vs)
  | Inj (i, n, v) -> Inj (i, n, metaval_nf prog v)
  | PosCons (cons, typs, args) ->
    PosCons (cons,
             List.map (typ_nf prog) typs,
             List.map (metaval_nf prog) args)

and destr_nf prog destr = match destr with
  | Call (vs, s) ->
    Call (List.map (metaval_nf prog) vs, metastack_nf prog s)
  | Proj (i, n, s) -> Proj (i, n, metastack_nf prog s)
  | Closure s -> Closure (metastack_nf prog s)
  | NegCons (destr, typs, vs, s) ->
    NegCons (destr,
             List.map (typ_nf prog) typs,
             List.map (metaval_nf prog) vs, metastack_nf prog s)

and patt_nf prog (patt, cmd) =
  let open Constructors in
  let patt, binds = match patt with
    | Unit | Int _ | Bool _ -> patt, []
    | Thunk b -> Thunk (bind_nf prog b), [b]
    | Inj (i,n,b) -> Inj(i,n, bind_nf prog b), [b]
    | Tupple bs -> Tupple (List.map (bind_nf prog) bs), bs
    | PosCons (c, typs, bs)-> PosCons (c, typs, List.map (bind_nf prog) bs), bs in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let prog' = cmd_nf {prog with curr = cmd; declared} in
  patt, prog'.curr

and copatt_nf prog (copatt, cmd) =
  let copatt, binds, cont = match copatt with
    | Call (xs, a) -> Call (List.map (bind_nf prog) xs, bind_nf prog a), xs, a
    | NegCons (c, typs, xs, a) ->
      NegCons (c, typs, List.map (bind_nf prog) xs, bind_nf prog a), xs, a
    | Proj (i,n,a) -> Proj (i,n,bind_nf prog a), [], a
    | Closure a -> Closure (bind_nf prog a), [], a
    in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let declared_cont = CoVar.Env.add (fst cont) () prog.declared_cont in
  let prog' = cmd_nf {prog with curr = cmd; declared; declared_cont} in
  copatt, prog'.curr

and metaval_nf prog (MetaVal v) =
  MetaVal {v with
           node = val_nf prog v.node;
           val_typ = typ_nf prog v.val_typ}

and metastack_nf prog (MetaStack s) =
  MetaStack {s with
             node = stack_nf prog s.node;
             cont_typ = typ_nf prog s.cont_typ;
            final_typ = typ_nf prog s.final_typ}

and cmd_nf prog =
  let prog = head_normal_form prog in
  let (Command cmd) = prog.curr in
  let cmd = Command
      {cmd with
       valu = metaval_nf prog cmd.valu;
       stk = metastack_nf prog cmd.stk;
       mid_typ = typ_nf prog cmd.mid_typ;
       final_typ = typ_nf prog cmd.final_typ} in
  {prog with curr = cmd}

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
