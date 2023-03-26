open Misc
open Vars
open Types
open Prelude
open Intern_common
open Intern_prettyPrinter
open Format
open InternAst


let pos_uso = Litt (Base Positive)

let neg_uso = Litt (Base Negative)

let idx_uso i = Litt (Index i)

let get env uso =
  USortVar.Env.find uso env.unifier

let get_opt env uso =
  USortVar.Env.find_opt uso env.unifier

let rec get_loc env polvar =
  try match get env polvar with
    | Loc (loc, _) -> loc
    | Redirect u -> get_loc env u
    | _ -> dummy_pos
  with
  | Not_found -> dummy_pos

let set uso env sovar =
    let uso =
      try match get env sovar with
        | Loc (loc, _) -> Loc (loc, uso)
        | _ -> uso
      with Not_found -> uso in
    {env with unifier = USortVar.Env.add sovar uso env.unifier}

let unify_uso env uso1 uso2 =

  let acc = ref [] in
  let add sovar = acc := sovar :: !acc in
  let finalize p = List.fold_left (set p) env !acc in

  let rec collect loc = function
    | Litt p -> loc, Some p
    | Loc (loc, v) -> collect loc v
    | Redirect v -> begin
        add v;
        match get_opt env v with
        | None -> loc, None
        | Some upol -> collect loc upol
    end in

  let loc1, so1 = collect dummy_pos uso1 in
  let loc2, so2 = collect dummy_pos uso2 in
  let loc = if loc1 <> dummy_pos then loc1 else loc2 in

  let p = match so1, so2 with
  | Some p, None
  | None, Some p -> (Litt p)
  | None, None -> (Redirect (USortVar.fresh ()))
  | Some p1, Some p2 when p1 = p2 -> (Litt p1)
  | Some s1, Some s2 ->
    Printf.printf "%s %s" (string_of_sort SortVar.to_string s1) (string_of_sort SortVar.to_string s2);
    fail_polarity_mismatch uso1 uso2 loc1 loc2 in

  finalize (Loc (loc, p))


let unify_def ?debug env item =

  let prelude = env.prelude in
  let env = ref env in

  let rec pp_upol fmt = function
    | Litt so -> pp_sort fmt so
    | Loc (loc,u) -> fprintf fmt "loc(%s)%a" (string_of_position loc) pp_upol u
    | Redirect v -> pp_print_string fmt (USortVar.to_string v) in

  let rec unify uso1 uso2=
    env := unify_uso !env uso1 uso2

  and unify_tyvar_sort sort tvar =
    match TyVar.Env.find_opt tvar !env.tyvarsorts with
    | Some u -> unify u sort
    | None -> env := {!env with tyvarsorts = TyVar.Env.add tvar sort !env.tyvarsorts}

  and unify_typ upol1 typ = match typ with
    | TBox _ | TPos _ -> unify upol1 pos_uso
    | TFix _ | TNeg _ -> unify upol1 neg_uso
    | TCons {node;_} -> unify_typecons upol1 node
    | TApp {tfun;_} -> unify_typ upol1 tfun
    | TVar {node=var; _}| TInternal var ->
      try unify upol1 (TyVar.Env.find var !env.tyvarsorts)
      with
      | Not_found ->
        let pol = Redirect (USortVar.fresh ()) in
        env := {!env with tyvarsorts = TyVar.Env.add var pol !env.tyvarsorts};
        unify upol1 pol

  and unify_typecons upol1 (tcons : 'a Types.type_cons) =
    match tcons with
    | Unit | Zero | Int | Bool -> unify upol1 pos_uso
    | Top | Bottom -> unify upol1 neg_uso
    | Thunk -> unify upol1 neg_uso
    | Closure -> unify upol1 pos_uso
    | Prod _ | Sum _ -> unify upol1 pos_uso
    | Choice _ -> unify upol1 neg_uso
    | Fun _ -> unify upol1 neg_uso
    | Cons cons ->
      let consdef = TyConsVar.Env.find cons !prelude.tycons in
      let _, ret_so = unmk_arrow consdef.sort in
      unify upol1 (Litt ret_so)

  and unify_bind upol (var, typ) loc =
    let polvar =
      try Var.Env.find var !env.varsorts
      with Not_found ->
        let polvar = USortVar.fresh () in
        env := {!env with varsorts = Var.Env.add var polvar !env.varsorts};
        polvar in
    unify upol (Loc (loc, Redirect polvar));
    unify_typ upol typ

  and unify_cobind upol (covar, typ) loc =
    let polvar =
      try CoVar.Env.find covar !env.covarsorts
      with Not_found ->
        let polvar = USortVar.fresh () in
        env := {!env with
                covarsorts = CoVar.Env.add covar polvar !env.covarsorts
               };
        polvar in
    unify upol (Loc (loc, Redirect polvar));
    unify_typ upol typ

  and unify_val upol valu loc =
    match valu with
    | Var v ->
      let polvar =
        try Var.Env.find v !env.varsorts
        with Not_found -> fail_undefined_var (Var.to_string v) loc in
      unify upol (Loc (loc, Redirect polvar))
    | CoTop -> unify upol (Loc (loc, neg_uso))
    | Bindcc {bind; pol; cmd} ->
      unify_cobind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd upol cmd
    | Box {bind; cmd; _} ->
      unify upol (Loc (loc, pos_uso));
      unify_cobind neg_uso bind loc;
      unify_cmd neg_uso cmd
    | Cons cons ->
      unify_cons loc upol cons
    | Fix {self=(x,t); cmd; cont=bind} ->
      unify_typ neg_uso t;
      unify_bind pos_uso (x, boxed exp t) loc;
      unify_cobind neg_uso bind loc;
      unify_cmd neg_uso cmd;
      unify upol (Loc (loc, neg_uso))
    | Destr copatts ->
      let go (copatt, cmd) = unify_copatt copatt cmd upol loc in
      List.iter go copatts

  and unify_stk upol final_upol loc stk =
    match stk with
    | Ret a ->
      unify (Loc (loc, upol)) final_upol;
      let polvar =
        try CoVar.Env.find a !env.covarsorts
        with Not_found -> fail_undefined_var (CoVar.to_string a) loc in
      unify upol (Loc (loc, Redirect polvar))
    | CoZero -> unify upol (Loc (loc, pos_uso))
    | CoBind {bind; pol; cmd} ->
      unify_bind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd final_upol cmd
    | CoBox {stk;_} ->
      unify upol pos_uso;
      unify_meta_stk neg_uso final_upol stk
    | CoDestr destr ->
      unify_destr loc upol final_upol destr
    | CoCons patts ->
      let go (patt, cmd) =
        unify_patt patt upol loc;
        unify_cmd final_upol cmd in
      List.iter go patts
    | CoFix stk ->
      unify upol neg_uso;
      unify_meta_stk neg_uso final_upol stk

  and unify_cons loc upol cons =
    match cons with
    | Unit | Inj _ | Thunk _ | Tupple _ | Int _ | Bool _ ->
      let args, upol', args_upol = match cons with
        | Unit | Int _ | Bool _ -> [], pos_uso, pos_uso
        | Inj(_, _, x) -> [x], pos_uso, pos_uso
        | Thunk x -> [x], neg_uso, pos_uso
        | Tupple xs -> xs, pos_uso, pos_uso
        | PosCons _ -> assert false in
      unify upol (Loc (loc, upol'));
      List.iter (unify_meta_val args_upol) args
    | PosCons (cons, typs, args) ->
      unify upol (Loc (loc, pos_uso));
      let Consdef { private_typs; _ } = def_of_cons prelude cons in
      List.iter2 (fun t (_, so) -> unify_typ (Litt so) t) typs private_typs;
      List.iter (unify_meta_val pos_uso) args

  and unify_destr loc upol final_upol destr =
    match destr with
    | Call _ | Proj _ | Closure _ ->
      let args, cont, upol', cont_pol = match destr with
        | Call (x,a) -> x, a, neg_uso, neg_uso
        | Proj (_, _, a)-> [], a, neg_uso, neg_uso
        | Closure a -> [], a, pos_uso, neg_uso
        | NegCons _ -> assert false in
      List.iter (unify_meta_val pos_uso) args;
      unify upol (Loc (loc, upol'));
      unify_meta_stk cont_pol final_upol cont
    | NegCons (destr, typs, args, cont) ->
      unify upol (Loc (loc, neg_uso));
      let Destrdef { private_typs; _ } = def_of_destr prelude destr in
      List.iter2 (fun t (_, so) -> unify_typ (Litt so) t) typs private_typs;
      unify_meta_stk neg_uso final_upol cont;
      List.iter (unify_meta_val pos_uso) args

  and unify_patt patt upol loc =
    begin match patt with
      | Thunk _ -> unify upol (Loc (loc, neg_uso))
      | _ ->  unify upol (Loc (loc, pos_uso))
    end;
    begin match patt with
      | Unit | Int _ | Bool _ -> ()
      | Inj (_, _, bind) -> unify_bind pos_uso bind loc
      | Thunk bind -> unify_bind pos_uso bind loc
      | Tupple xs ->
        List.iter (fun x -> unify_bind pos_uso x loc) xs
      | PosCons (cons, typs, args) ->
        let Consdef { private_typs; _} = def_of_cons prelude cons in
        let private_typs = List.map (fun (t,so) -> (t, Litt so)) private_typs in
        unify upol pos_uso;
        List.iter (fun bind -> unify_bind pos_uso bind loc) args;
        List.iter2 (fun (x,so) (_,so') ->
            unify_tyvar_sort so x;
            unify_tyvar_sort so' x)
          typs private_typs
    end

  and unify_copatt copatt cmd upol loc =
    begin match copatt with
      | Closure _ -> unify upol (Loc (loc, pos_uso))
      | _ ->  unify upol (Loc (loc, neg_uso))
    end;
    begin match copatt with
      | Call (bindxs, binda) ->
        List.iter (fun x -> unify_bind pos_uso x loc) bindxs;
        unify_cobind neg_uso binda loc;
      | Proj (_, _, binda) ->
        unify_cobind neg_uso binda loc;
      | Closure binda ->
        unify_cobind neg_uso binda loc;
      | NegCons (destr, typs, args, cont) ->
        let Destrdef {private_typs; _} = def_of_destr prelude destr in
        let private_typs = List.map (fun (t,so) -> (t, Litt so)) private_typs in
        unify upol neg_uso;
        List.iter (fun bind -> unify_bind pos_uso bind loc) args;
        unify_cobind neg_uso cont loc;
        List.iter2 (fun (x, so) (_, so')->
            unify_tyvar_sort so x;
            unify_tyvar_sort so' x)
          typs private_typs;

    end;
    (* If we don't unify the commands after the pattern, the bound variables
       won't be in scope *)
    unify_cmd neg_uso cmd;

  and unify_meta_val pol (MetaVal {node; val_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "value with(%a,%a) %a"
          pp_upol pol
          pp_typ val_typ
          pp_pre_value node;
        dump_env std_formatter !env;
        Format.pp_print_flush fmt ();
      | None -> ()
    end;

    unify_val pol node loc;
    unify_typ pol val_typ;

  and unify_meta_stk cont_pol final_pol (MetaStack {node; cont_typ; final_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "stack with(%a:%a) final(%a:%a) %a"
          pp_typ cont_typ
          pp_upol cont_pol
          pp_typ final_typ
          pp_upol final_pol
          pp_pre_stack node;
        dump_env std_formatter !env;
        pp_print_flush fmt ()
      | None -> ()
    end ;
    unify_typ cont_pol cont_typ;
    unify_typ final_pol final_typ;
    unify_stk cont_pol final_pol loc node;


  and unify_cmd final_pol (Command cmd) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "command final(%a) %a"
          pp_upol final_pol
          pp_cmd (Command cmd);
        dump_env std_formatter !env;
        pp_print_flush fmt ();
      | None -> ()
    end ;
    unify_meta_val cmd.pol cmd.valu;
    unify_meta_stk cmd.pol final_pol cmd.stk;
    unify_typ cmd.pol cmd.mid_typ;
    unify_typ final_pol cmd.final_typ

  in

  begin match item with
    | Value_declaration item ->
      unify_typ item.pol item.typ
    | Value_definition item ->
      unify_typ item.pol item.typ;
      unify_meta_val item.pol item.content
    | Command_execution item ->
      let upol = Redirect (USortVar.fresh ()) in
      unify_cobind upol (item.cont, item.conttyp) item.loc;
      unify_cmd upol item.content;
      unify upol item.pol
  end;

  begin match item with
    | Value_declaration {name; pol; loc; _} | Value_definition {name; pol; loc; _} ->
      let polvar = USortVar.fresh () in
      env := {!env with varsorts = Var.Env.add name polvar !env.varsorts};
      unify pol (Loc (loc, Redirect polvar));
    | _ -> ()
  end;

  !env
