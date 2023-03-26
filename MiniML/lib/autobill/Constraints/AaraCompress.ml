open FirstOrder
open FullFOL


module Subst = Map.Make (struct
    type t = var
    let compare = compare
  end)

let rec apply_term s (t : term) = match t with
  | TCons _ -> t
  | TApp {tfun;args;loc} -> TApp {
      tfun = apply_term s tfun;
      args = List.map (apply_term s) args;
      loc }
  | TVar {node=v;_}| TInternal v -> match Subst.find_opt v s with
    | None -> t
    | Some t -> apply_term s t

let apply_eqn s eqn = match eqn with
  | Eq (a,b,so) -> Eq (apply_term s a, apply_term s b, so)
  | Rel (r, args) -> Rel (r, List.map (apply_term s) args)

let is_cyclic_for x t = List.mem x (freevars_of_typ t)

let add_binding s x t = Subst.add x t s

let extend_with_override s s' =
    let s' = Subst.map (apply_term s) s' in
    Subst.merge (fun _ a b ->
      match a,b with
      | Some x, None | None, Some x -> Some x
      | None, None -> None
      | Some x, Some _ -> Some x
    ) s s'

let eqn_to_subst rank eqn = match eqn with
  | Rel _ -> Subst.empty
  | Eq (a,b,_) -> match a, b with
    | ((TVar {node=x;_} | TInternal x) as t) , ((TVar {node=y;_} | TInternal y) as u) ->
      if x = y then
        Subst.empty
      else if rank y < rank x && not (is_cyclic_for x u) then
        Subst.singleton x u
      else if not (is_cyclic_for y t) then
        Subst.singleton y t
      else
        Subst.empty
    | (TVar {node=x;_} | TInternal x), t
    | t, (TVar {node=x;_} | TInternal x) ->
      if is_cyclic_for x t then Subst.empty else Subst.singleton x t
    | _ -> Subst.empty

let rec eqns_to_subst rank eqns = match eqns with
  | [] -> Subst.empty
  | eqn :: eqns ->
    let s = eqn_to_subst rank eqn in
    let s' = eqns_to_subst rank (List.map (apply_eqn s) eqns) in
    extend_with_override s s'

let substitute_variables f =

  let ranks = ref Subst.empty in
  let get_rank x = Subst.find x !ranks in
  let set_rank r x = ranks := Subst.add x r !ranks in

  let rec build r f = match f with
    | PTrue | PFalse -> Subst.empty
    | PLoc (_, f) -> build r f
    | PEqn eqns -> eqns_to_subst get_rank eqns
    | PAnd fs ->
      List.fold_left extend_with_override Subst.empty (List.map (build r) fs)
    | PCases fs ->
      List.fold_left extend_with_override Subst.empty (List.map (build r) fs)
    | PExists (xs, ys, eqns, f) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      let s = eqns_to_subst get_rank eqns in
      let s' = build (r+2) f in
      extend_with_override s' s
    | PForall (xs, ys, _, _) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      Subst.empty in

  let rec apply s f = match f with
    | PTrue | PFalse -> f
    | PLoc (loc, f) -> PLoc (loc, apply s f)
    | PAnd fs -> PAnd (List.map (apply s) fs)
    | PEqn eqns -> PEqn (List.map (apply_eqn s) eqns)
    | PCases fs -> PCases (List.map (apply s) fs)
    | PExists (xs, ys, eqns, f) ->
      PExists (xs, ys, List.map (apply_eqn s) eqns, apply s f)
    | PForall (xs, ys, eqns, f) ->
      PForall (xs, ys, List.map (apply_eqn s) eqns, apply s f) in

  let rec transform r f =
    let s = build r f in
    let f = apply s f in
    match f with
    | PTrue | PFalse | PEqn _ -> f
    | PLoc (loc, f) -> PLoc (loc, transform r f)
    | PAnd fs -> PAnd (List.map (transform r) fs)
    | PCases fs -> PCases (List.map (transform r) fs)
    | PExists (xs, ys, eqns, f) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      let f = transform (r+2) f in
      let fvs = freevars_of_formula f @ freevars_of_eqns eqns in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PExists (filter xs, filter ys, eqns, transform (r+2) f)
    | PForall (xs, ys, eqns, f) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      let s = eqns_to_subst get_rank eqns in
      let f = transform (r+2) (apply s f) in
      let fvs = freevars_of_formula f @ freevars_of_eqns eqns in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PForall (filter xs, filter ys, List.map (apply_eqn s) eqns, f) in

  transform 0 f

let compress_unification f =
  compress_logic (substitute_variables
                    (compress_logic
                       (substitute_variables
                          (compress_logic f))))
