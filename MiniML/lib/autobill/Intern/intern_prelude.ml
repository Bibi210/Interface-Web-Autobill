open Vars
open Types
open Constructors
open Prelude
open FirstOrder
open Preprocess_ast
open Intern_common

let rec intern_sort env loc = function
  | Base p -> Base p
  | Arrow (s,t) -> Arrow (intern_sort env loc s, intern_sort env loc t)
  | Index i ->
    try Index (StringEnv.find i env.sort_vars) with
    | Not_found -> fail_undefined_sort i loc


let rec intern_type env scope = function

  | TVar {node; loc} ->
    begin
      try TCons {node = Cons (StringEnv.find node env.tycons_vars); loc}
      with
        Not_found ->
        try TVar {node = get_tyvar scope node; loc}
        with
          Not_found -> fail_undefined_type node loc
    end

  | TCons {node; loc} ->
      Types.cons ~loc (match node with
        | Cons cons ->
          let name =
            try StringEnv.find cons env.tycons_vars
            with _ -> fail_undefined_type cons loc in
          Cons name
        | Unit -> Unit
        | Zero -> Zero
        | Top -> Top
        | Bottom -> Bottom
        | Prod n -> Prod n
        | Sum n -> Sum n
        | Choice n -> Choice n
        | Fun n -> Fun n
        | Thunk -> Thunk
        | Closure q -> Closure q
        | Fix -> Fix)

  | TInternal var -> intern_type env scope (TVar {node = var; loc = Misc.dummy_pos})

  | TApp {tfun; args; loc} ->
    if args = [] then
      intern_type env scope tfun
    else
      TApp {tfun = intern_type env scope tfun;
            args = List.map (intern_type env scope) args;
            loc}



let internalize_all_sortvar env prog =
  let go env = function
    | Cst.Sort_declaration {name; _} ->
      {env with sort_vars = StringEnv.add name (SortVar.of_string name) env.sort_vars}
    | _ -> env in
  List.fold_left go env prog


let internalize_all_typcons env prog =

  let internalize_one_typecons = function
    | Cst.Type_definition {name; args; sort; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, sort), loc)
    | Cst.Data_definition {name; args; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, Base Positive), loc)
    | Cst.Codata_definition {name; args; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, Base Negative), loc)
    | Cst.Type_declaration {name; sort; loc; _} -> Some (name, ([], sort), loc)
    | _ -> None in

  let go env item  =
    match internalize_one_typecons item with
    | None -> env
    | Some (cons, (args, ret_sort), loc) ->
      let args = List.map (intern_sort env loc) args in
      let ret_sort = intern_sort env loc ret_sort in
      let sort = sort_arrow args ret_sort in
      if List.mem cons type_cons_names then
        fail_double_def ("type constructor " ^ cons) loc
      else
        match StringEnv.find_opt cons env.tycons_vars with
        | Some _ -> fail_double_def ("type constructor " ^ cons) loc
        | None ->
          let real_cons = (TyConsVar.of_string cons) in
          {env with
           tycons_vars = StringEnv.add cons real_cons env.tycons_vars;
           tycons_sort = TyConsVar.Env.add real_cons sort env.tycons_sort;
          } in

  List.fold_left go env prog

let internalize_all_rels env prog =

  let go env item = match item with
    | Cst.Rel_declaration {name; args; loc} ->
      let args = List.map (intern_sort env loc) args in
      begin match StringEnv.find_opt name env.rels with
      | Some _ -> fail_double_def ("relation " ^ name) loc
      | None ->
        let real_name = RelVar.of_string name in
        env.prelude := {!(env.prelude) with
                        relations = RelVar.Env.add real_name args !(env.prelude).relations};
        {env with rels = StringEnv.add name real_name env.rels}
      end
    | _ -> env in
  List.fold_left go env prog


let rec sort_check_type loc env expected_sort (typ : InternAst.typ) =
  let typ, sort = sort_infer_type loc env typ in
  if sort <> expected_sort then
    fail_bad_sort loc sort expected_sort
  else
    typ


and sort_infer_type loc env typ = match typ with

  | TVar {node; loc} ->
    begin try
        TVar {node = node; loc}, TyVar.Env.find node env.prelude_typevar_sort
      with
        Not_found -> fail_undefined_type (TyVar.to_string node) loc
    end
  | TInternal var -> sort_infer_type loc env (TVar {node = var; loc = Misc.dummy_pos})

  | TCons {node; loc} ->
    let rec arr n arg ret =
      if n = 0 then ret else arr (n-1) arg (Arrow (arg, ret)) in
    let aux sort output = (TCons {node=output;loc}, sort) in
    begin match node with
      | Unit -> aux sort_postype Types.Unit
      | Zero -> aux sort_postype Zero
      | Top -> aux sort_negtype Top
      | Bottom -> aux sort_negtype Bottom
      | Fix -> aux sort_negtype Fix
      | Thunk -> aux (arr 1 sort_postype sort_negtype) Types.Thunk
      | Closure q -> aux (sort_arrow [sort_negtype] sort_postype) (Types.Closure q)
      | Prod n -> aux (arr n sort_postype sort_postype) (Prod n)
      | Sum n -> aux (arr n sort_postype sort_postype) (Sum n)
      | Fun n ->
        aux (arr 1 sort_negtype (arr n sort_postype sort_negtype)) (Fun n)
      | Choice n -> aux (arr n sort_negtype sort_negtype) (Choice n)
      | Cons cons ->
        let sort =
          try TyConsVar.Env.find cons env.tycons_sort
          with _ -> fail_undefined_type (TyConsVar.to_string cons) loc in
        aux sort (Cons cons)
    end

  | TApp {tfun; args; loc} ->
    let tfun, sort = sort_infer_type loc env tfun in
    let go sort arg = match sort with
      | Arrow (sort, sort') -> sort', sort_check_type loc env sort arg
      | _ -> fail_cant_apply_type "type application with head that has not function sort" loc in
    let ret, args = List.fold_left_map go sort args in
    TApp {tfun; args; loc}, ret

let intern_and_sort_check_eqn loc env scope = function
  | Cst.Eq (a,b, ()) ->
    let a,asort = sort_infer_type loc env (intern_type env scope a) in
    let b,bsort = sort_infer_type loc env (intern_type env scope b) in
    if asort = bsort then
      FullFOL.Eq (a,b,asort)
    else
      fail_bad_sort loc asort bsort
  | Rel (rel, args) ->
    let rel' = StringEnv.find rel env.rels in
    let args_sort = try
        RelVar.Env.find rel' !(env.prelude).relations
      with _ -> fail_undefined_rel rel loc in
    let args = List.map2
        (fun so t -> sort_check_type loc env so (intern_type env scope t))
        args_sort args in
    Rel (rel', args)

let sort_check_tycons_args ?(sort_check = (fun _ -> ())) env loc scope args =

  let scope, new_args = List.fold_left_map
      (fun scope (x,s) ->
         let s = intern_sort env loc s in
         sort_check s;
         let scope = add_tyvar scope x in
         (scope, (get_tyvar scope x, s)))
      scope
      args in
  let env = List.fold_left
      (fun env (tyvar,sort) ->
         {env with prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
      env
      new_args in
  env, scope, new_args

let sort_check_paramters env loc scope args =
  let sort_check so =
    if not (is_base_index_sort so) then begin
      let mess = "The parameters here must have a base parameter sort" in
      raise (Bad_sort (mess, loc)) end in
  sort_check_tycons_args ~sort_check env loc scope args

let sort_check_one_item env item =

  let scope = empty_scope in

  match item with

  | Cst.Type_declaration {name; sort; loc} ->
    let name = StringEnv.find name env.tycons_vars in
    let typdef = {
      sort = intern_sort env loc sort;
      loc = loc;
      args = [];
      content = Declared} in
    env.prelude := { !(env.prelude) with
                     tycons = TyConsVar.Env.add name typdef !(env.prelude).tycons};
    env

  | Cst.Type_definition {name; args; content; loc; sort} ->

    let sort = intern_sort env loc sort in
    let name = StringEnv.find name env.tycons_vars in
    let new_env, new_scope, new_args = sort_check_tycons_args env loc scope args in
    let typdef = {
      sort = TyConsVar.Env.find name env.tycons_sort;
      loc = loc;
      args = new_args;
      content = Defined (sort_check_type loc new_env sort
                           (intern_type new_env new_scope content))} in
    env.prelude :=
      {!(env.prelude) with tycons = TyConsVar.Env.add name typdef !(env.prelude).tycons};
    env

  | Cst.Data_definition {name = "Bool" ; content; loc = _; args = []} ->

    let new_name = Primitives.tycons_bool in

    let go_one env (Raw_Cons cons, _) =
      let tag = match cons.tag with
        | Bool b -> string_of_bool b
        | _ -> Misc.fail_invariant_break "Hidden definition of booleans is invalid" in
      let new_tag = ConsVar.of_string tag in
      let new_cons = Raw_Cons {
          tag = PosCons new_tag;
          idxs =[];
          args = []
        } in
      let cons_def = Consdef {
          typ_args = [];
          constructor = new_cons;
          equations = [];
          resulting_type = typecons new_name []} in
      env, (tag, new_tag, cons_def, (PosCons new_tag, new_cons, [] )) in

    let env, res = List.fold_left_map go_one env content in
    let consdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let conses = List.map (fun (x,y,_,_) -> (x,y)) res in
    let env = {
      env with
      tycons_vars = StringEnv.add "Bool" new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort_postype env.tycons_sort;
      conses = StringEnv.add_seq (List.to_seq conses) env.conses } in
    let tyconsdef = {
      sort = sort_postype;
      loc = Misc.dummy_pos;
      args = [];
      content = Data (List.map (fun (_,_,_,x) -> x) res)} in
    env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      cons = ConsVar.Env.add_seq (List.to_seq consdefs) !(env.prelude).cons};
    env


  | Cst.Data_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let env, scope, new_tyvars =
      sort_check_tycons_args env loc scope args in

    let go_one env (Raw_Cons cons, eqns) =
      let tag = match cons.tag with
        | PosCons c -> c
        | _ ->
          let open Format in
          Constructors.pp_constructor_tag CstPrettyPrinter.pp_cons_aux  str_formatter cons.tag;
          let tag_str = flush_str_formatter () in
          fail_bad_constructor tag_str loc in
      if StringEnv.mem tag env.conses then
        fail_double_def ("constructor " ^ tag) loc;
      let env, scope, new_idxs =
        sort_check_paramters env loc scope cons.idxs in
      let new_args = List.map
          (fun typ -> sort_check_type loc env sort_postype (intern_type env scope typ))
          cons.args in
      let new_eqns = List.map
          (intern_and_sort_check_eqn loc env scope)
          eqns in
      let new_tag = ConsVar.of_string tag in
      let new_cons = Raw_Cons {
          tag = PosCons new_tag;
          idxs = new_idxs;
          args = new_args
        } in
      let new_content = PosCons new_tag, new_cons, new_eqns in
      let cons_def = Consdef {
          typ_args = new_tyvars;
          constructor = new_cons;
          equations = new_eqns;
          resulting_type =
            typecons new_name (List.map (fun (x,_) -> tvar x) new_tyvars)} in
      env, (tag, new_tag, cons_def, new_content) in

    let env, res = List.fold_left_map go_one env content in
    let consdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let conses = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = sort_arrow (List.map snd new_tyvars) sort_postype in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      conses = StringEnv.add_seq (List.to_seq conses) env.conses } in
    let tyconsdef = {
      sort;
      loc;
      args = new_tyvars;
      content = Data (List.map (fun (_,_,_,x) -> x) res)} in
    env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      cons = ConsVar.Env.add_seq (List.to_seq consdefs) !(env.prelude).cons};
    env

  | Cst.Codata_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let env, scope, new_tyvars
      = sort_check_tycons_args env loc scope args in
    let go_one env (Raw_Destr destr, eqns) =
      let tag = match destr.tag with
        | NegCons d -> d
        | _ ->
          let open Format in
          Constructors.pp_destructor_tag CstPrettyPrinter.pp_cons_aux  str_formatter destr.tag;
          let tag_str = flush_str_formatter () in
          fail_bad_destructor tag_str loc in
      if StringEnv.mem tag env.destrs then
        fail_double_def ("destructor " ^ tag) loc;
      let env, scope, new_idxs =
        sort_check_paramters env loc scope destr.idxs in
      let new_args = List.map
          (fun typ -> sort_check_type loc env sort_postype
              (intern_type env scope typ))
          destr.args in
      let new_cont = intern_type env scope destr.cont in
      let new_cont = sort_check_type loc env sort_negtype new_cont in
      let new_eqns = List.map
          (intern_and_sort_check_eqn loc env scope)
          eqns in
      let new_tag = DestrVar.of_string tag in
      let new_destr = Raw_Destr {
          tag = NegCons new_tag;
          idxs = new_idxs;
          args = new_args;
          cont = new_cont
        } in
      let new_content = (NegCons new_tag, new_destr, new_eqns) in
      let destr_def = Destrdef {
          typ_args = new_tyvars;
          destructor = new_destr;
          equations = new_eqns;
          resulting_type =
            typecons new_name (List.map (fun (x,_) -> tvar x) new_tyvars);
        } in
      env, (tag, new_tag, destr_def, new_content) in

    let env, res = List.fold_left_map go_one env content in
    let destrdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let destrs = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = sort_arrow (List.map snd new_tyvars) (Base Negative) in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      destrs = StringEnv.add_seq (List.to_seq destrs) env.destrs} in
    let tyconsdef = {
      sort;
      loc;
      args = new_tyvars;
      content = Codata (List.map (fun (_,_,_,x) -> x) res)} in
    env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      destr = DestrVar.Env.add_seq (List.to_seq destrdefs) !(env.prelude).destr};
    env

  | Cst.Sort_declaration {name; _} ->
    let name' = StringEnv.find name env.sort_vars in
    env.prelude := {
      !(env.prelude) with
      sort_defs = SortVar.Env.add name' () !(env.prelude).sort_defs
    };
    env

  | Cst.Rel_declaration {name; args; loc} ->
    let name' = StringEnv.find name env.rels in
    let args' = List.map (intern_sort env loc) args in
    if not (List.for_all is_base_index_sort args') then begin
      let mess = "Relations cannot have high-order parameter arguments" in
      raise (Bad_sort (mess, loc))
    end;
    env.prelude := {
      !(env.prelude) with
      relations = RelVar.Env.add name' args' !(env.prelude).relations
    };
    env

  | Cst.Term_definition _
  | Cst.Term_declaration _
  | Cst.Cmd_execution _
  | Cst.Goal_selection _ -> env
