open Misc
open Format

module type FOL_Params = sig
  type sort
  type rel
  type var
  type term

  val pp_sort : formatter -> sort -> unit
  val pp_term : formatter -> term -> unit
  val pp_var : formatter -> var -> unit
  val pp_rel : formatter -> rel -> unit

end

module FOL (P : FOL_Params) = struct

  include P

  type eqn =
    | Eq of term * term * sort
    | Rel of rel * term list

  type formula =
    | PTrue
    | PFalse
    | PLoc of position * formula
    | PEqn of eqn list
    | PAnd of formula list
    | PExists of var list * eqn list * formula
    | PForall of var list * var list * eqn list * formula

  type ctx =
    | KEmpty
    | KLoc of position * ctx
    | KAnd of formula list * ctx * formula list
    | KForall of var list * var list * eqn list * ctx
    | KExists of var list * eqn list * ctx

  let pp_list pp fmt l =
    fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_space pp) l

  let pp_eqn fmt = function
    | Eq (a,b,_) ->
      fprintf fmt "(:eq %a %a)" pp_term a pp_term b
    | Rel (rel,args) -> fprintf fmt "@[<hov 1>(:rel \"%a\"@ %a)@]" pp_rel rel (pp_list pp_term) args

  let pp_eqns fmt eqns = fprintf fmt "@[<hov 1>(%a)@]" (pp_list pp_eqn) eqns

  let pp_vars fmt vars = fprintf fmt "@[(%a)@]" (pp_list pp_var) vars

  let pp_formula ?with_loc:(with_loc=false) fmt f =
    let rec pp fmt f = match f with
      | PTrue -> fprintf fmt ":true"
      | PFalse -> fprintf fmt ":false"
      | PLoc (loc, f) ->
        if with_loc then
          fprintf fmt "@[<v 1>(:located \"%s\"@ %a)@]" (string_of_position loc) pp f
        else
          pp fmt f
      | PEqn eqns -> pp_eqns fmt eqns
      | PAnd fs -> fprintf fmt "@[<v 1>(:and@ %a)@]" (pp_list pp) fs
      | PExists (vars, eqns, rest) ->
        fprintf fmt "@[<v 1>(:exists %a@ :witness %a@ :then %a)@]"
          pp_vars vars
          pp_eqns eqns
          pp rest
      | PForall (vars, exists, eqns, rest) ->
        fprintf fmt "@[<v 1>(:forall %a@ :exists %a@ :assume %a@ :then %a)@]"
          pp_vars vars
          pp_vars exists
          pp_eqns eqns
          pp rest

    in pp fmt f

  let string_of_formula ?with_loc:(with_loc=false) f =
    pp_formula ~with_loc str_formatter f; flush_str_formatter ()

let map_eqns f eqns =
  let go = function
    | Eq (a,b, so) -> Eq (f a, f b, so)
    | Rel (rel, args) -> Rel (rel, List.map f args) in
  List.map go eqns

let rec map f_var f_term = function
  | PTrue -> PTrue
  | PFalse -> PFalse
  | PLoc (loc, c) -> PLoc (loc, map f_var f_term c)
  | PEqn eqns -> PEqn (map_eqns f_term eqns)
  | PAnd cs -> PAnd (List.map (map f_var f_term) cs)
  | PExists (xs, eqns, c) ->
    PExists (List.map f_var xs, map_eqns f_term eqns, map f_var f_term c)
  | PForall (xs, ys, eqns, c) ->
    PForall (List.map f_var xs,
             List.map f_var ys,
             map_eqns f_term eqns,
             map f_var f_term c)

  let visit_eqns f eqns =
    let go = function
      | Eq (a,b, _) -> f a; f b
      | Rel (_, args) -> List.iter f args in
    List.iter go eqns

  let visit f_var f_term =
    let rec go = function
    | PTrue
    | PFalse -> ()
    | PLoc (_, c) -> go c
    | PEqn eqns -> visit_eqns f_term eqns
    | PAnd cs -> List.iter go cs
    | PExists (xs, eqns, c) ->
      List.iter f_var xs;
      visit_eqns f_term eqns;
      go c
    | PForall (xs, ys, eqns, c) ->
      List.iter f_var xs;
      List.iter f_var ys;
      visit_eqns f_term eqns;
      go c

  in go


type compress_quantifiers_t =
  | Univ of var list * var list * eqn list
  | Exist of var list * eqn list

let rec compress_logic ?(remove_loc = false) c =

  let canary = ref true in

  let kill () = (canary := false) in

  let rec compress_eqns eqns =
    let rec remove_ids = function
    | [] -> []
    | eqn::eqns -> match eqn with
      | Eq (a,b,_) when a = b -> kill (); remove_ids eqns
      | Eq (a,b,so) when b > a -> (Eq (b,a,so)) :: (remove_ids eqns)
      | _ -> eqn :: (remove_ids eqns) in
    List.fold_left insert_nodup [] (remove_ids eqns)

  and advance c ctx = match c with
    | PTrue | PEqn [] -> shortcut_true ctx
    | PFalse -> shortcut_false ctx
    | PEqn eqns -> backtrack (PEqn (compress_eqns eqns)) ctx
    | PLoc (loc, c) ->
      if remove_loc then advance c ctx else advance c (lift_loc loc ctx)
    | PAnd [] -> backtrack PTrue ctx
    | PAnd (x::xs) -> advance x (lift_and xs ctx)
    | PExists ([], [], x) -> kill (); advance x ctx
    | PForall ([], [], [], x) -> kill (); advance x ctx
    | PExists (vs, eqns, x) ->
      advance x (lift_quant (Exist (vs, compress_eqns eqns)) ctx)
    | PForall (vs, ws, eqns, x) ->
      advance x (lift_quant (Univ (vs, ws, compress_eqns eqns)) ctx)

  and backtrack c ctx = match ctx with
    | KEmpty -> c
    | KLoc (loc, ctx) -> backtrack (PLoc (loc, c)) ctx
    | KAnd ([], ctx, []) -> kill (); backtrack c ctx
    | KAnd (xs, ctx, []) -> backtrack (PAnd (c::xs)) ctx
    | KAnd (xs, ctx, y::ys) -> advance y (KAnd (c::xs, ctx, ys))
    | KForall (vs, ws, eqns, ctx) -> backtrack (PForall (vs, ws, eqns, c)) ctx
    | KExists (vs, eqns, ctx) -> backtrack (PExists (vs, eqns, c)) ctx

  and lift_loc loc = function
    | KLoc (_, ctx) -> kill (); KLoc (loc, ctx)
    | ctx -> KLoc (loc, ctx)

  and lift_quant vs ctx = match vs,ctx with
    | Exist (vs, eqns), KExists (vs', eqns', ctx') ->
      let vs = List.fold_left Misc.insert_nodup vs vs' in
      kill (); KExists (vs, eqns@eqns', ctx')
    | Univ (vs, ws, eqns), KForall (vs', ws', eqns', ctx') ->
      let vs = List.fold_left Misc.insert_nodup vs vs' in
      let ws = List.fold_left Misc.insert_nodup ws ws' in
      kill (); KForall (vs, ws, eqns@eqns', ctx')
    | Exist (vs, eqns), _ -> KExists (vs, eqns, ctx)
    | Univ (vs, ws, eqns), _ -> KForall (vs, ws, eqns, ctx)

  and lift_and cs ctx = match ctx with
    | KAnd (xs, ctx, ys) -> kill (); KAnd (xs, ctx, cs @ ys)
    | ctx -> KAnd ([], ctx, cs)

  and shortcut_false ctx = match ctx with
    | KEmpty -> PFalse
    | KLoc (_, ctx)
    | KAnd (_, ctx, _)
    | KExists (_, _, ctx) -> kill (); shortcut_false ctx
    | KForall _ -> backtrack PFalse ctx

  and shortcut_true ctx = match ctx with
    | KEmpty -> PTrue
    | KLoc (_, ctx)
    | KForall (_, _, _, ctx) -> kill (); shortcut_true ctx
    | KAnd (xs, ctx, []) -> backtrack (PAnd xs) ctx
    | KAnd (xs, ctx, y::ys) -> advance y (KAnd (xs, ctx, ys))
    | KExists _ -> backtrack PTrue ctx in

  let c = advance c KEmpty in
  if !canary then c else compress_logic ~remove_loc c

end

module FullFOL =  FOL(struct
    open Vars
    open Types
    type var = TyVar.t
    type sort = SortVar.t Types.sort
    type rel = RelVar.t
    type term = typ

    let pp_var = TyVar.pp
    let pp_rel = RelVar.pp
    let pp_sort = pp_sort SortVar.to_string
    let pp_term = pp_typ TyConsVar.pp TyVar.pp
  end)
