open Misc
open Format

include UnionFind

exception Invariant_break of string

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
    | CExists of uvar list * con
    | CGuardedExists of uvar list * eqn list * con
    | CDef of int * uvar * con
    | CVar of int * uvar
    | CGoal of {
        typs : uvar list;
        inner : con;
        outer : con;
        quantification_duty : uvar list;
        accumulated : uvar list;
        univ_eqns : eqn list;
        existentials : uvar list;
        exist_eqns : eqn list;
      }

  type kontext =
    | KEmpty
    | KLoc of position * kontext
    | KAnd of post list * kontext * con list
    | KDef of int * uvar * kontext
    | KLet1 of {
        typs : uvar list;
        inner : kontext;
        outer : con;
        quantification_duty : uvar list;
        accumulated : uvar list;
        univ_eqns : eqn list;
        existentials : uvar list;
        exist_eqns : eqn list;
      }
    | KLet2 of {
        typs : uvar list;
        outer : kontext;
        quantified : uvar list;
        eqns : eqn list;
        post : post;
      }

  let eq u v = CEq (u,v)

  let cvar x u = CVar(x, u)

  let ( @+ ) c d = CAnd [c;d]

  let exists ?st:(eqns) xs con =
    match eqns with
    | None -> CExists (xs,con)
    | Some eqns -> CGuardedExists (xs, eqns, con)

  let pp_amper_sep fmt () = fprintf fmt "@ & "

  let pp_comma_sep fmt () = fprintf fmt ",@ "

  let pp_uvars = pp_print_list ~pp_sep:pp_comma_sep pp_uvar

  let rec pp_constraint fmt = function
    | CTrue -> pp_print_string fmt "T"
    | CFalse -> pp_print_string fmt "F"
    | CEq (a,b) -> fprintf fmt "%a=%a" pp_uvar a pp_uvar b
    | CVar (x, t) -> fprintf fmt "v%d : %a" x pp_term t
    | CLoc (loc, c) ->
      fprintf fmt "@[<v 2>at \"%s\":@,%a@]" (string_of_position loc) pp_constraint c
    | CAnd cons ->
      fprintf fmt "@[<hov 0>%a@]" (pp_print_list ~pp_sep:pp_amper_sep pp_con_paren) cons
    | CExists (vars, con) ->
      fprintf fmt "@[<b 2>exists %a.@ %a@]" pp_uvars vars pp_constraint con
    | CGuardedExists (vars, eqns, con) ->
      fprintf fmt "@[<b 2>exists %a.@ (%a)@ &%a@]" pp_uvars vars pp_eqns eqns pp_constraint con
    | CDef (x, t, c) ->
      fprintf fmt "@[<b 2>var v%d : %a in@ %a@]" x pp_term t pp_constraint c
    | CGoal {typs; accumulated; inner; outer; quantification_duty;
             univ_eqns; exist_eqns; existentials; _} ->
      fprintf fmt "@[<b 2>let forall %a. exists %a. %a => %a@ &(%a) (got %a)@ = %a@ in %a@]"
        pp_uvars quantification_duty
        pp_uvars existentials
        pp_eqns univ_eqns
        pp_eqns exist_eqns
        (pp_print_list ~pp_sep:pp_comma_sep pp_uvar) typs
        pp_uvars accumulated
        pp_constraint inner
        pp_constraint outer

  and pp_con_paren fmt con = fprintf fmt "(%a)" pp_constraint con

  let pp_kontext fmt ctx =

    let rec once ctx = match ctx with
      | KEmpty -> pp_print_cut fmt ()
      | KLoc (loc, ctx) ->
        fprintf fmt "-location: %s@," (string_of_position loc);
        once ctx
      | KAnd (posts, ctx, rests) ->
        fprintf fmt "@[<v 2>-conjonction:@,@[<hov 2>done:%a@]@,@[<hov 2>todo:%a@]@]@,"
          (pp_print_list ~pp_sep:pp_print_cut pp_formula) posts
          (pp_print_list ~pp_sep:pp_print_cut pp_constraint) rests;
        once ctx
      | KDef (x, t, ctx) ->
        fprintf fmt "-var v%d : %a@," x pp_uvar t;
        once ctx
      | KLet1 {typs; inner; outer; quantification_duty;
               accumulated; univ_eqns; existentials; exist_eqns;_ } ->
        fprintf fmt "@[<v 2>-let forall @[<h>%a@]. exists @[<h>%a@]. (%a)@, eqns: %a => %a@, accumulated: @[%a@]@,todo: %a@]@,"
          pp_uvars quantification_duty
          pp_uvars existentials
          (pp_print_list ~pp_sep:pp_comma_sep pp_uvar) typs
          pp_eqns univ_eqns
          pp_eqns exist_eqns
          pp_uvars accumulated
          pp_constraint outer;
        once inner
      | KLet2 { typs; outer; quantified; eqns; post } ->
        fprintf fmt "@[<v 2>-letted forall %a. %a => (%a)@,post:%a@]"
          pp_uvars quantified
          pp_eqns eqns
          (pp_print_list ~pp_sep:pp_comma_sep pp_uvar) typs
          (pp_formula ~with_loc:false) post;
        once outer
    in

    pp_open_vbox fmt 0;
    once ctx;
    pp_close_box fmt ()

  let _trace stack con post =
    let fmt = err_formatter in
    pp_open_vbox fmt 0;
    fprintf fmt "@,==========================================@,";
    fprintf fmt "============== NEW CYCLE =================@,";
    fprintf fmt "==========================================@,";
    fprintf fmt "@,------------context stack---------------@,";
    pp_kontext fmt stack;
    fprintf fmt "@,--------------constraint----------------@,";
    pp_constraint fmt con;
    fprintf fmt "@,------------post-constraint-------------@,";
    fprintf fmt "global vars: @[<h>%a@]@," pp_uvars !existentials;
    fprintf fmt "global eqns: %a@," pp_eqns !model;
    pp_formula fmt post;
    fprintf fmt "@,-------------type variables-------------@,";
    let pp_bind fmt (x,(us,u)) =
      fprintf fmt "v%d : forall @[<h>%a@]. %d"
         x
         pp_uvars us
         u in
    pp_print_list ~pp_sep:pp_comma_sep pp_bind fmt !_nvar_env;
      fprintf fmt "@,-------------substitution-------------@,";
    pp_subst fmt !_state;
    pp_print_newline fmt ();
    pp_close_box fmt ()
    

    let rec compress_cand c =
    let rec acc cc c : con list = match c with
      | CAnd [] -> cc
      | CAnd (h::t) -> acc (acc cc h) (CAnd t)
      | CExists (u, con) -> CExists (u, compress_cand con) :: cc
      | CLoc (loc, con) -> CLoc (loc, compress_cand con) :: cc
      | CDef (x,u,con) -> CDef (x,u,compress_cand con) :: cc
      | CGoal lett ->
        CGoal {lett with inner = compress_cand lett.inner;
                        outer = compress_cand lett.outer} :: cc
      | _ -> [c] @ cc in
    match acc [] c with
    | [d] -> d
    | l -> CAnd l

  let float_cexists c =
    let rec aux c = match c with
      | CExists (u,d) ->
        let fv,d = aux d in u@fv,d
      | CDef (x,u,d) ->
        let fv,d = aux d in
        fv, CDef (x,u,d)
      | CAnd cc ->
        let fvs, cc = List.split (List.map aux cc) in
        let fvs = List.concat fvs in
        fvs, CAnd cc
      | CLoc (loc, c) ->
        let fvs, c = aux c in
        fvs, CLoc (loc, c)
      | CGoal lett ->
        let (fv1,inner), (fv2,outer) = aux lett.inner, aux lett.outer in
        let inner = if fv1 = [] then inner else CExists (fv1,inner) in
        fv2, CGoal {lett with inner; outer}
      | CEq _ | CVar _ | CTrue | CFalse -> [], c
      | CGuardedExists (xs, eqns, c) ->
        let ys, c = aux c in
        [], CGuardedExists (xs@ys, eqns, c) in
    let fv,c = aux c in
    CExists (fv,c)

  let rec lookup_scheme stack x = match stack with
    | KEmpty ->
      raise (Failure ("Broken invariant: Unbound var during constraint solving: v"
                      ^ string_of_int x))
    | KAnd (_, ctx, _) -> lookup_scheme ctx x
    | KLoc (_, ctx) -> lookup_scheme ctx x
    | KDef (y,a,ctx) -> if x = y then ([], a, []) else lookup_scheme ctx x
    | KLet1 { inner;_} -> lookup_scheme inner x
    | KLet2 {outer;_}  -> lookup_scheme outer x

  let lift_exist us ?st:(eqns=[]) stack =
    let us, idx = List.partition (fun x -> is_syntactic_sort (get_sort x)) us in
    let rec go stack = match stack with
      | KEmpty -> begin
          existentials := idx @ !existentials;
          model := eqns @ !model;
          KEmpty
        end
      | KAnd (cons, ctx, post) -> KAnd (cons, go ctx, post)
      | KLoc (loc, ctx) -> KLoc (loc, go ctx)
      | KDef (x,a,ctx) -> KDef (x,a, go ctx)
      | KLet1 lett->
        KLet1 {lett with
               accumulated = List.fold_left insert_nodup us lett.accumulated;
               existentials = List.fold_left insert_nodup idx lett.existentials;
               exist_eqns =  List.fold_left insert_nodup eqns lett.exist_eqns}
      | KLet2 lett -> KLet2 {lett with outer = go lett.outer} in
    go stack

  let rec fail_unification ctx u v = match ctx with
    | KLoc (loc, _) ->
      let message = Printf.sprintf "unification failed around position %s between %d and %d"
          (string_of_position loc)
          u
          v in
      raise (Failure message)
    | KEmpty ->
      let message = Printf.sprintf "unification failed between %d and %d" u v in
      raise (Failure message)
    | KAnd (_, ctx, _) | KDef (_, _, ctx) | KLet1 {inner=ctx;_} | KLet2 {outer=ctx;_} ->
      fail_unification ctx u v

  let unify_or_fail ctx u v =
    try unify u v with Unify (u',v') -> fail_unification ctx u' v'

  exception Done

  exception Not_sufficiently_polymorphic of int list

  type 'a elaboration = 'a -> con * (output_env -> 'a)

  let ( >>> ) con gen = con, gen


  let finalize_post_con env c =
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
      | PExists (vars, eqns, c) ->
        PExists (List.map env.get vars,
                 List.map model_id vars @ finalize_eqns eqns,
                 go c)
      | PForall (vars, exist, eqns, c) ->
        PForall (List.map env.get vars,
                 List.map env.get exist,
                 List.map model_id (vars @ exist) @ finalize_eqns eqns,
                 go c) in
    let existentials = List.fold_left insert_nodup [] (List.map repr !existentials) in
    FFOL.PExists (List.map env.get existentials,
                  finalize_eqns !model @ List.map model_id existentials,
                  go c)

  let solve ?trace:(do_trace=false) (elab : 'a elaboration) (x : 'a) =

    let rec entrypoint () =
      reset_unifier ();
      let con, gen = elab x in
      if do_trace then _trace KEmpty con PTrue;
      let con = compress_cand (float_cexists con) in
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
        backtrack stack
          (PEqn (List.map (fun (u,v) -> Eq (u,v, get_sort u)) (unify_or_fail stack a b)))
      | CAnd [] -> backtrack stack PTrue
      | CAnd [con] -> advance stack con
      | CAnd (h::t) -> advance (KAnd ([], stack, t)) h
      | CDef (x,u,con) -> advance (KDef (x,u,stack)) con
      | CLoc (loc, con) -> advance (KLoc (loc, stack)) con

      | CExists (us, con) -> advance (lift_exist us stack) con
      | CGuardedExists (us, eqns, con) -> advance (lift_exist us ~st:eqns stack) con

      | CVar (x,u) ->
        let (vs, v, eqns) = lookup_scheme stack x in
        let stack = lift_exist vs ~st:eqns stack in
        let eqs = unify_or_fail stack u v in
        backtrack stack (PEqn (List.map (fun (u,v) -> Eq (u,v,get_sort u)) eqs))

      | CGoal { typs; accumulated;
                inner; outer;
                quantification_duty; existentials;
                exist_eqns; univ_eqns} ->
        enter ();
        let stack = KLet1 { typs; outer; quantification_duty;
                          inner = stack; existentials; accumulated;
                           univ_eqns; exist_eqns} in
        advance stack inner

    and backtrack stack post =
      if do_trace then _trace stack CTrue post;
      match stack with

      | KEmpty -> post
      | KAnd (posts, stack, []) -> backtrack stack (PAnd (post :: posts))
      | KLoc (loc, stack) -> backtrack stack (PLoc (loc, post))
      | KAnd (posts, stack, h::t) -> advance (KAnd (post :: posts, stack, t)) h
      | KDef (x, u, stack) -> define x ([],u); backtrack stack post
      | KLet2 {outer;post=post';_} -> backtrack outer (PAnd [post;post'])

      | KLet1 { typs; inner; outer;
                quantification_duty; existentials;
                exist_eqns; univ_eqns; accumulated} ->

        let tmp mess (us, vs) =
          if do_trace then
            fprintf err_formatter "%s forall %a. %a\n"
              mess
              pp_uvars us
              pp_uvars vs
           in

        tmp ("checking scheme") (accumulated, typs);
        (* Shorten paths we will take, normalize the variables *)
        let typs = List.map repr typs in
        let quantification_duty = List.map repr quantification_duty in
        let accumulated = List.map repr accumulated in
        let accumulated = List.fold_left insert_nodup [] accumulated in
        let existentials = List.map repr existentials in
        let existentials = List.fold_left insert_nodup [] existentials in

        accumulated |> List.iter (fun u ->
        (* There sould be no cycles in cells of syntactic sorts *)
        if not (occurs_check u) then begin
          pp_subst err_formatter !_state;
          raise (Cycle u);
        end);

        tmp "checks passed, now lifting" (accumulated, typs);
        (* lower each variable, lowest-ranked vars first *)
        let fvs = List.concat (existentials
                               :: accumulated
                               :: List.map freevars_of_type typs) in
        Array.iteri lift_freevars (ranked_freevars fvs !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let accumulated, old = extract_old_vars accumulated !_rank in
        let existentials, old' = extract_old_vars existentials !_rank in

        (* verify we polymorphized enough *)
        let xs =
          List.filter (fun x -> is_syntactic_sort (get_sort x)) accumulated
        and ys =
          List.filter (fun x -> is_syntactic_sort (get_sort x)) quantification_duty in
        if not (is_sublist ys xs) then begin
          fprintf err_formatter "===xs===";
          List.iter (fun x -> print_int x; print_newline ()) xs;
          fprintf err_formatter "===ys not sublist===";
          List.iter (fun x -> print_int x; print_newline ()) ys;
          raise (Not_sufficiently_polymorphic typs)
        end;

        let scheme = (accumulated, typs) in
        tmp "After lifting scheme" scheme;
        let inner = lift_exist old inner in
        let inner = lift_exist old' inner in

        leave ();

        let idx = List.filter
            (fun x -> not (is_syntactic_sort (get_sort x)))
            accumulated in
        let existentials = List.filter (fun x -> not (List.mem x idx)) existentials in

        let post = PForall (idx, existentials, univ_eqns, PAnd [post; PEqn exist_eqns]) in
        let ctx = KLet2 {
            typs; post;
            quantified = ys;
            outer=inner;
            eqns = univ_eqns
          } in
        advance ctx outer
    in

    (* entrypoint is defined at the top of elaborate *)
    entrypoint ()

end
