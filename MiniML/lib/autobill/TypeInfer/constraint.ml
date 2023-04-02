open Misc
open Format

include UnionFind

exception Type_error of string * position option

module Make (U : Unifier_params) = struct

  include UnionFind.Make (U)

  let existentials = ref []

  let model = ref []

  type post = formula

  type con =
    | CTrue
    | CFalse
    | CLoc of position * con
    | CEq of uvar * uvar
    | CAnd of con list
    | CCases of con list
    | CExists of uvar list * con
    | CGuardedExists of uvar list * eqn list * con
    | CDef of int * string * uvar * con
    | CVar of int * uvar
    | CUnivIdx of {
        typs : uvar list;
        inner : con;
        duty : uvar list;
        accumulated : uvar list;
        eqns : eqn list;
      }
    | CExistsIdx of {
        typs : uvar list;
        inner : con;
        duty : uvar list;
        accumulated : uvar list;
        eqns : eqn list;
      }


  type kontext =
    | KEmpty
    | KLoc of position * kontext
    | KAnd of post list * kontext * con list
    | KCases of post list * kontext * con list
    | KDef of int * string * uvar * kontext
    | KUnivIdx of {
        typs : uvar list;
        inner : kontext;
        duty : uvar list;
        accumulated : uvar list;
        eqns : eqn list;
      }
      | KExistsIdx of {
        typs : uvar list;
        duty : uvar list;
        inner : kontext;
        accumulated : uvar list;
        eqns : eqn list;
      }


  let eq u v = CEq (u,v)

  let cvar x u = CVar(x, u)

  let ( @+ ) c d = CAnd [c;d]

  let exists ?st:(eqns) xs con =
    match eqns with
    | None -> CExists (xs,con)
    | Some eqns -> CGuardedExists (xs, eqns, con)

  let pp_uvars fmt vars =
    fprintf fmt "@[(%a)@]" (pp_print_list ~pp_sep:pp_print_space pp_uvar) vars

  let rec pp_constraint fmt = function
    | CTrue -> pp_print_string fmt ":true"
    | CFalse -> pp_print_string fmt ":false"
    | CEq (a,b) -> fprintf fmt "(:eq-base %a %a)" pp_uvar a pp_uvar b
    | CVar (x, t) -> fprintf fmt "(:var-isa %d %a)" x pp_term t
    | CLoc (loc, c) ->
      fprintf fmt "@[<v 1>(:located \"%s\"@ %a)@]" (string_of_position loc) pp_constraint c
    | CAnd [] -> fprintf fmt ":true"
    | CAnd [c] -> pp_constraint fmt c
    | CAnd cons ->
      fprintf fmt "@[<v 1>(:and@ %a)@]" (pp_print_list ~pp_sep:pp_print_space pp_con_paren) cons
    | CCases cons ->
      fprintf fmt "@[<v 1>(:cases@ %a)@]" (pp_print_list ~pp_sep:pp_print_space pp_con_paren) cons
    | CExists (vars, con) ->
      fprintf fmt "@[<v 1>(:exists %a@ %a)@]" pp_uvars vars pp_constraint con
    | CGuardedExists (vars, eqns, con) ->
      fprintf fmt "@[<v 2>(:exists %a@ :witness %a@ %a@]"
        pp_uvars vars
        pp_eqns eqns
        pp_constraint con
    | CDef (x, s, t, c) ->
      fprintf fmt "@[<v 1>(:var-def %s=%d :isa %a :in@ %a)@]"
        s x pp_term t pp_constraint c
    | CUnivIdx {typs; accumulated; inner; duty; eqns} ->
      fprintf fmt "@[<v 1>(:forall-idx %a :got %a :goal %a@ :assume %a@ %a)@]"
        pp_uvars duty
        pp_uvars accumulated
        pp_uvars typs
        pp_eqns eqns
        pp_constraint inner
    | CExistsIdx {typs; accumulated; duty; inner; eqns} ->
      fprintf fmt "@[<v 1>(:exists-idx %a :witness: %a goal %a@ :witness %a@ %a)@]"
        pp_uvars accumulated
        pp_uvars duty
        pp_uvars typs
        pp_eqns eqns
        pp_constraint inner

  and pp_con_paren fmt con = fprintf fmt "(%a)" pp_constraint con

  let pp_kontext fmt ctx =

    let rec once ctx = match ctx with
      | KEmpty -> pp_print_cut fmt ()
      | KLoc (loc, ctx) ->
        fprintf fmt "--- (:located %s)@," (string_of_position loc);
        once ctx
      | KAnd (posts, ctx, rests) ->
        fprintf fmt "--- @[<v 1>(:and :done %a@ :todo %a)@]@,"
          (pp_print_list ~pp_sep:pp_print_cut pp_formula) posts
          (pp_print_list ~pp_sep:pp_print_cut pp_constraint) rests;
        once ctx
      | KCases (posts, ctx, rests) ->
        fprintf fmt "--- @[<v 1>(:cases :done %a@ :todo %a)@]@,"
          (pp_print_list ~pp_sep:pp_print_cut pp_formula) posts
          (pp_print_list ~pp_sep:pp_print_cut pp_constraint) rests;
        once ctx
      | KDef (x, s, t, ctx) ->
        fprintf fmt "--- (:var-def %s=%d :isa %a)@," s x pp_uvar t;
        once ctx
      | KUnivIdx {typs; accumulated; inner; duty; eqns} ->
        fprintf fmt "--- @[<v 1>(:forall-idx %a :got %a :goal %a@ :assume %a)@]@,"
          pp_uvars duty
          pp_uvars accumulated
          pp_uvars typs
          pp_eqns eqns;
        once inner
      | KExistsIdx {typs; accumulated; inner;duty; eqns} ->
        fprintf fmt "--- @[<v 1>(:exists-idx %a :witness %a :goal %a@ :witness %a)@]@,"
          pp_uvars accumulated
          pp_uvars duty
          pp_uvars typs
          pp_eqns eqns;
          once inner
    in
    pp_open_vbox fmt 0;
    once ctx;
    pp_close_box fmt ()

  let _trace stack con post =
    let fmt = std_formatter in begin
      pp_set_geometry err_formatter ~max_indent:180 ~margin:200;
      fprintf fmt "@.==========================================@.";
      fprintf fmt "============== NEW CYCLE =================@.";
      fprintf fmt "==========================================@.";
      fprintf fmt "@.------------context stack---------------@.";
      pp_kontext fmt stack;
      fprintf fmt "@.--------------constraint----------------@.";
      pp_constraint fmt con;
      fprintf fmt "@.------------post-constraint-------------@.";
      fprintf fmt "global vars: @[<h>%a@]@." pp_uvars !existentials;
      fprintf fmt "global eqns: %a@." pp_eqns !model;
      pp_formula fmt post;
      fprintf fmt "@.-------------type of variables-------------@.";
      let pp_bind fmt (x,(us,u)) =
        fprintf fmt "(:var-scheme %d :forall %a :isa %d)@."
          x
          pp_uvars us
          u in
      pp_print_list pp_bind fmt !_nvar_env;
      fprintf fmt "@.-------------substitution-------------@.";
      fprintf fmt "%a@." pp_subst !_state;
    end

  let rec lookup_scheme stack x = match stack with
    | KEmpty ->
     fail_invariant_break ("Unbound var during constraint solving: v" ^ string_of_int x)
    | KAnd (_, ctx, _) | KCases (_, ctx, _) -> lookup_scheme ctx x
    | KLoc (_, ctx) -> lookup_scheme ctx x
    | KDef (y,_,a,ctx) -> if x = y then ([], a, []) else lookup_scheme ctx x
    | KUnivIdx { inner;_} -> lookup_scheme inner x
    | KExistsIdx {inner;_}  -> lookup_scheme inner x


  let check_freevars_in_stack eqns stack =
    let fvs = freevars_of_eqns (fun x -> UFOL.S.singleton x) eqns in
    let rec go fvs = function
      | KEmpty -> UFOL.S.elements fvs
      | KAnd (_, ctx, _) | KCases (_, ctx, _) -> go fvs ctx
      | KLoc (_, ctx) -> go fvs ctx
      | KDef (_, _, _, ctx) -> go fvs ctx
      | KUnivIdx univ -> go (UFOL.S.diff fvs (UFOL.S.of_list univ.accumulated)) univ.inner
      | KExistsIdx exists -> go (UFOL.S.diff fvs (UFOL.S.of_list exists.accumulated)) exists.inner in
    let fvs = go fvs stack in
      if fvs <> [] then begin
      let mess = Format.(
            fprintf str_formatter "variables out of bound in eqns %a@." pp_eqns eqns;
            flush_str_formatter ()) in
      Misc.fail_invariant_break mess
    end

  let lift_exist us ?st:(eqns=[]) stack =
    let rec go stack = match stack with
      | KEmpty -> begin
          let idx = List.filter (fun x -> not (is_syntactic_sort (get_sort x))) us in 
          existentials := idx @ !existentials;
          model := eqns @ !model;
          KEmpty
        end
      | KAnd (cons, ctx, post) -> KAnd (cons, go ctx, post)
      | KCases (cons, ctx, post) -> KCases (cons, go ctx, post)
      | KLoc (loc, ctx) -> KLoc (loc, go ctx)
      | KDef (x,s,a,ctx) -> KDef (x,s,a,go ctx)
      | KUnivIdx univ ->
        KUnivIdx { univ with
          accumulated = us @ univ.accumulated;
          eqns = eqns @ univ.eqns;
        }
      | KExistsIdx exists ->
        KExistsIdx { exists with
          accumulated = us @ exists.accumulated;
          eqns = eqns @ exists.eqns;
        }

    in
    go stack

  let rec fail_unification info ctx u v =
    let mess = Printf.sprintf "unification failed between %d and %d: %s" u v info in
    match ctx with
    | KLoc (loc, _) -> raise (Type_error (mess, Some loc))
    | KEmpty -> raise (Type_error (mess, None))
    | KAnd (_, ctx, _) | KDef (_, _, _, ctx) | KCases (_, ctx, _)
    | KUnivIdx {inner=ctx;_} | KExistsIdx {inner=ctx;_} ->
      fail_unification info ctx u v

  let unify_or_fail ctx u v =
    try unify u v with
    | UnifySort (u,v) -> fail_unification "different number of arguments" ctx u v
    | Unify (u,v) -> fail_unification "different type constructors" ctx u v

  type 'a elaboration = 'a -> con * (output_env -> 'a)

  let ( >>> ) con gen = con, gen


  let lift_idx_freevars post =

    let open UFOL in

    let rec advance r p ctx = match p with

      | PTrue | PFalse | PEqn _ -> backtrack r p ctx
      | PLoc (loc, p) -> advance r p (KLoc (loc, ctx))
      | PAnd ps -> advance r PTrue (KAnd ([], ctx, ps))
      | PCases ps -> advance r PTrue (KCases ([], ctx, ps))
      | PExists (xs, ys, eqns, p) ->
        let ctx = KExists ([], ys, eqns, ctx) in
        let ctx = List.fold_left (lift (r+1)) ctx xs in
        advance (r+1) p ctx
      | PForall (xs, ys, eqns, p) ->
        let ctx = KForall ([], ys, eqns, ctx) in
        let ctx = List.fold_left (lift (r+1)) ctx xs in
        advance (r+1) p ctx

    and backtrack r p ctx = match ctx with
      | KEmpty ->
        if r > 0 then
          fail_invariant_break "During typechecking, ranking is out of sync with constraint"
        else
          p
      | KLoc (loc ,ctx) -> backtrack r (PLoc (loc, p)) ctx
      | KAnd (dones, ctx, []) -> backtrack r (PAnd (p::dones)) ctx
      | KCases (dones, ctx, []) -> backtrack r (PCases (p::dones)) ctx
      | KAnd (dones, ctx, p'::todos) -> advance r p' (KAnd (p::dones, ctx, todos))
      | KCases (dones, ctx, p'::todos) -> advance r p' (KCases (p::dones, ctx, todos))
      | KForall (xs, ys, eqns, ctx) ->
        backtrack (r-1) (PForall (xs, ys, eqns, p)) ctx
      | KExists (xs, ys, eqns, ctx) ->
        backtrack (r-1) (PExists (xs, ys, eqns, p)) ctx


    and lift r ctx v =

      let r_goal = rank v in

      let rec go r ctx = match ctx with
        | KEmpty ->
          Misc.fail_invariant_break "Failed to lift type variable when creating logical constriant"
        | KLoc (loc, ctx) -> KLoc (loc, go r ctx)
        | KAnd (dones, ctx, todos) -> KAnd (dones, go r ctx, todos)
        | KCases (dones, ctx, todos) -> KCases (dones, go r ctx, todos)
        | KForall (xs, ys, eqns, ctx) ->
          if r <= r_goal then
            KForall (v :: xs, ys, eqns, ctx)
          else
            KForall (xs, ys, eqns, go (r-1) ctx)
        | KExists (xs, ys, eqns, ctx) ->
          if r <= r_goal then
            KExists (v :: xs, ys, eqns, ctx)
          else
            KExists (xs, ys, eqns, go (r-1) ctx) in

      go r ctx

    in

    advance 0 post UFOL.KEmpty


  let finalize_post_con env c =
    let c = lift_idx_freevars c in
    let c = PExists (!existentials, [], !model, c) in
    let model_id x = FFOL.Eq (deep_of_var (env.get x), env.u x, get_sort x) in
    let finalize_eqns  = List.map (function
        | UFOL.Eq (a, b, so) -> FFOL.Eq (env.u a, env.u b, so)
        | Rel (rel, args) -> Rel (rel, List.map env.u args)) in
    let rec go = function
      | UFOL.PTrue -> FFOL.PTrue
      | PFalse -> PFalse
      | PLoc (loc, c) -> PLoc (loc, go c)
      | PEqn eqns -> PEqn (finalize_eqns eqns)
      | PAnd cs -> PAnd (List.map go cs)
      | PCases cs -> PCases (List.map go cs)
      | PExists (vars, duty, eqns, c) ->
        PExists (List.map env.get vars,
                 List.map env.get duty,
                 List.map model_id vars @ finalize_eqns eqns,
                 go c)
      | PForall (vars, duty, eqns, c) ->
        PForall (List.map env.get vars,
                 List.map env.get duty,
                 List.map model_id vars @ finalize_eqns eqns,
                 go c) in
    go c


  let solve ?trace:(do_trace=false) (elab : 'a elaboration) (x : 'a) =

    let rec entrypoint () =
      reset_unifier ();
      let con, gen = elab x in
      if do_trace then _trace KEmpty con PTrue;
      let post = advance KEmpty con in
      let env = finalize_env () in
      let post = finalize_post_con env post in
      gen env, post

    and advance stack con =
      if do_trace then _trace stack con PTrue;
      match con with

      | CTrue -> backtrack stack PTrue
      | CFalse -> backtrack stack PFalse
      | CEq (a,b) ->
        let eqns = (List.map (fun (u,v) -> Eq (u,v, get_sort u)) (unify_or_fail stack a b)) in
        backtrack (lift_exist [] ~st:eqns stack) PTrue
        (* backtrack stack (PEqn eqns)  *)
      | CAnd [] -> backtrack stack PTrue
      | CAnd [con] -> advance stack con
      | CAnd (h::t) -> advance (KAnd ([], stack, t)) h
      | CCases [] -> backtrack stack PTrue
      | CCases [con] -> advance stack con
      | CCases (h::t) -> advance (KCases ([], stack, t)) h
      | CDef (x,s,u,con) -> advance (KDef (x,s,u,stack)) con
      | CLoc (loc, con) -> advance (KLoc (loc, stack)) con

      | CExists (us, con) -> advance (lift_exist us stack) con
      | CGuardedExists (us, eqns, con) -> advance (lift_exist us ~st:eqns stack) con

      | CVar (x,u) ->
        let (vs, v, eqns) = lookup_scheme stack x in
        let stack = lift_exist vs ~st:eqns stack in
        let eqs = unify_or_fail stack u v in
        backtrack stack (PEqn (List.map (fun (u,v) -> Eq (u,v,get_sort u)) eqs))

      | CUnivIdx {duty; accumulated; eqns; inner; typs} ->
        enter ();
        advance (KUnivIdx {duty; accumulated; eqns; typs; inner = stack}) inner
      | CExistsIdx {duty; accumulated; eqns; inner; typs} ->
        enter ();
        advance (KExistsIdx {accumulated; duty; eqns; typs; inner = stack}) inner


    and backtrack stack post =
      if do_trace then _trace stack CTrue post;
      let compress post = UFOL.compress_logic ~remove_loc:false post in 
      match stack with

      | KEmpty -> post
      | KAnd (posts, stack, []) -> backtrack stack (compress (PAnd (post :: posts)))
      | KCases (posts, stack, []) -> backtrack stack (compress (PCases (post :: posts)))
      | KLoc (loc, stack) -> backtrack stack (PLoc (loc, post))
      | KAnd (posts, stack, h::t) -> advance (KAnd (post :: posts, stack, t)) h
      | KCases (posts, stack, h::t) -> advance (KCases (post :: posts, stack, t)) h
      | KDef (x, _, u, stack) -> define x ([],u); backtrack stack post

      | KUnivIdx {duty; accumulated; eqns; inner; typs} ->
        let tmp mess (us, vs) =
          if do_trace then
            printf "%s forall %a. %a\n"
              mess
              pp_uvars us
              pp_uvars vs in
        tmp ("checking scheme") (accumulated, typs);
        (* Shorten paths we will take, normalize the variables *)
        let normalize_vars v =
          let v = List.map repr v in
          List.fold_left insert_nodup [] v in
        let typs = normalize_vars typs in
        let accumulated = normalize_vars accumulated in
        let duty = normalize_vars duty in
        let fvs = List.concat ([accumulated; duty]
                               @ List.map freevars_of_type typs) in
        tmp "extended to" (fvs, typs);
        (* lower each variable, lowest-ranked vars first *)
        Array.iteri lift_freevars (ranked_freevars fvs !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let accumulated, old = extract_old_vars accumulated !_rank in
        tmp "after lifing" (accumulated, typs);
        let inner = lift_exist old inner in
        leave ();
        let idx = List.filter
            (fun x -> not (is_syntactic_sort (get_sort x))
                      && not (List.mem x duty))
            accumulated in 
        backtrack inner (PForall (idx, duty, eqns, post))

      | KExistsIdx {accumulated; eqns; duty; inner; typs} ->

        let tmp mess (us, vs) =
          if do_trace then
            printf "%s forall %a. %a\n"
              mess
              pp_uvars us
              pp_uvars vs in
        tmp ("lifting scheme") (accumulated, typs);
        (* Shorten paths we will take, normalize the variables *)
        let normalize_vars v =
          let v = List.map repr v in
          List.fold_left insert_nodup [] v in
        let typs = normalize_vars typs in
        let accumulated = normalize_vars accumulated in
        let duty = normalize_vars duty in
        (* lower each variable, lowest-ranked vars first *)
        let fvs = List.concat ([accumulated; duty]
                               @ List.map freevars_of_type typs) in
        (* lower each variable, lowest-ranked vars first *)
        Array.iteri lift_freevars (ranked_freevars fvs !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let accumulated, old = extract_old_vars accumulated !_rank in
        let inner = lift_exist old inner in
        leave ();
        let idx = List.filter
            (fun x -> not (is_syntactic_sort (get_sort x))
                    && not (List.mem x duty))
            accumulated in 
        backtrack inner (PExists (idx, duty, eqns, post))
    in

    (* entrypoint is defined at the top of elaborate *)
    entrypoint ()

end
