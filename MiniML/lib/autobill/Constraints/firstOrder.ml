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
    | PCases of formula list
    | PExists of var list * var list * eqn list * formula
    | PForall of var list * var list * eqn list * formula

  type ctx =
    | KEmpty
    | KLoc of position * ctx
    | KAnd of formula list * ctx * formula list
    | KCases of formula list * ctx * formula list
    | KForall of var list * var list * eqn list * ctx
    | KExists of var list * var list * eqn list * ctx

  let pp_list pp fmt l =
    fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_space pp) l

  let pp_eqn fmt = function
    | Eq (a,b,_) ->
      fprintf fmt "(:eq-idx %a %a)" pp_term a pp_term b
    | Rel (rel,args) -> fprintf fmt "@[<hov 1>(:rel \"%a\"@ %a)@]" pp_rel rel (pp_list pp_term) args

  let pp_eqns fmt eqns =
    fprintf fmt "@[<v 1>(%a)@]" (pp_list pp_eqn) eqns

  let pp_vars fmt vars = fprintf fmt "@[(%a)@]" (pp_list pp_var) vars

  let pp_formula ?with_loc:(with_loc=true) fmt f =
    let rec pp fmt f = match f with
      | PTrue -> fprintf fmt ":true"
      | PFalse -> fprintf fmt ":false"
      | PLoc (loc, f) ->
        if with_loc then
          fprintf fmt "@[<hv 1>(:loc \"%s\"@ %a)@]"
            (string_of_position ~with_filename:false loc) pp f
        else
          pp fmt f
      | PEqn eqns -> fprintf fmt "(:model %a)" pp_eqns eqns
      | PAnd fs -> fprintf fmt "@[<v 1>(:and@ %a)@]" (pp_list pp) fs
      | PCases fs -> fprintf fmt "@[<v 1>(:cases@ %a)@]" (pp_list pp) fs
      | PExists (vars, duty, eqns, rest) ->
        fprintf fmt "@[<v 1>(:exists %a@ :witness %a@ :with %a@ :then %a)@]"
          pp_vars vars
          pp_vars duty
          pp_eqns eqns
          pp rest
      | PForall (vars, duty, eqns, rest) ->
        fprintf fmt "@[<v 1>(:forall %a@ :let %a@ :assume %a@ :then %a)@]"
          pp_vars duty
          pp_vars vars
          pp_eqns eqns
          pp rest

    in pp fmt f

  let string_of_formula ?with_loc:(with_loc=true) f =
    pp_formula ~with_loc str_formatter f; flush_str_formatter ()

  module S : Set.S with type elt = var =
    Set.Make (struct type t = var let compare = compare end)

  let freevars_of_eqn fv_term = function
    | Eq (a,b,_) -> S.union (fv_term a) (fv_term b)
    | Rel (_, args) -> List.fold_left (fun vs t -> S.union vs (fv_term t)) S.empty args

  let freevars_of_eqns fv_term eqns =
    List.fold_left (fun vs eqn -> S.union vs (freevars_of_eqn fv_term eqn)) S.empty eqns

  let rec freevars_of_formula fv_term = function
    | PTrue | PFalse -> S.empty
    | PLoc (_, c) -> freevars_of_formula fv_term c
    | PEqn eqns -> freevars_of_eqns fv_term eqns
    | PAnd cs ->
      List.fold_left (fun vs c -> S.union vs (freevars_of_formula fv_term c)) S.empty cs
    | PCases cs ->
      List.fold_left (fun vs c -> S.union vs (freevars_of_formula fv_term c)) S.empty cs
    | PExists (xs,ys,eqns,c) | PForall (xs,ys,eqns,c) ->
      let fvs = S.union (freevars_of_eqns fv_term eqns) (freevars_of_formula fv_term c) in
      S.diff fvs (S.union (S.of_list xs) (S.of_list ys))

type compress_quantifiers_t =
  | Univ of var list * var list * eqn list
  | Exist of var list * var list * eqn list
  | Neutral of eqn list

let rec compress_logic ?(remove_loc = true) c =

  let canary = ref true in

  let kill () = (canary := false) in

  let rec compress_eqns eqns =
    let rec remove_ids = function
    | [] -> []
    | eqn::eqns -> match eqn with
      | Eq (a,b,_) when a = b -> kill (); remove_ids eqns
      | _ -> eqn :: (remove_ids eqns) in
    List.fold_left insert_nodup [] (remove_ids eqns)

  and advance c ctx = match c with
    | PTrue | PEqn [] -> backtrack PTrue ctx
    | PFalse -> backtrack PFalse ctx
    | PEqn eqns ->
      (* let ctx = lift_quant (Neutral (compress_eqns eqns)) ctx in *)
      backtrack (PEqn (compress_eqns eqns)) ctx
    | PLoc (loc, c) ->
      if remove_loc then advance c ctx else advance c (lift_loc loc ctx)
    | PAnd xs -> compress_and xs ctx
    | PCases cs -> advance PTrue (KCases ([], ctx, cs))
    | PExists ([], [], [], c) -> kill (); advance c ctx
    | PExists ([], [], eqns, c) -> kill (); advance (PAnd [PEqn (eqns); c]) ctx
    | PExists (xs, ys, eqns, c) ->
      advance c (lift_quant (Exist (xs, ys, compress_eqns eqns)) ctx)
    | PForall ([], [], [], x) -> kill (); advance x ctx
    | PForall (vars, duty, eqns, c) ->
      advance c (lift_quant (Univ (vars, duty, compress_eqns eqns)) ctx)

  and backtrack c ctx = match ctx with
    | KEmpty -> c
    | KLoc (loc, ctx) -> backtrack (PLoc (loc, c)) ctx
    | KAnd ([], ctx, []) -> kill (); backtrack c ctx
    | KAnd (xs, ctx, []) ->
      let xs = if c = PTrue then xs else c :: xs in
      backtrack (PAnd xs) ctx
    | KAnd (xs, ctx, y::ys) ->
      let xs = if c = PTrue then xs else c :: xs in
      advance y (KAnd (xs, ctx, ys))
    | KCases ([], ctx, []) -> kill (); backtrack c ctx
    | KCases (xs, ctx, []) ->
      let xs = if c = PTrue then xs else c :: xs in
      backtrack (PCases xs) ctx
    | KCases (xs, ctx, y::ys) ->
      let xs = if c = PTrue then xs else c :: xs in
      advance y (KCases (xs, ctx, ys))
    | KForall (xs, ys, eqns, ctx) ->
      let xs = List.fold_left insert_nodup [] xs in
      let ys = List.fold_left insert_nodup [] ys in
      backtrack (PForall (xs, ys, eqns, c)) ctx
    | KExists (xs, ys, eqns, ctx) ->
      let xs = List.fold_left insert_nodup [] xs in
      let ys = List.fold_left insert_nodup [] ys in
      backtrack (PExists (xs, ys, eqns, c)) ctx

  and compress_and cs ctx =
    let rec go acc eqns cs = match cs with
      | [] -> eqns, acc
      | PTrue :: cs -> go acc eqns cs
      | PLoc (_,c) :: cs when remove_loc -> go acc eqns (c :: cs)
      | PAnd (cs') :: cs -> kill (); go acc eqns (cs' @ cs)
      | PEqn eqns' :: cs -> go acc (eqns' @ eqns) cs
      | c :: cs -> go (c::acc) eqns cs in
    let eqns, cs = go [] [] cs in
    match eqns, cs with
    | [], [] -> backtrack PTrue ctx
    | [], cs -> advance PTrue (KAnd ([], ctx, cs))
    | eqns, [] -> advance (PEqn eqns) ctx
    | eqns, cs -> advance PTrue (KAnd ([], ctx, PEqn eqns :: cs))

  and lift_loc loc = function
    | KLoc (_, ctx) -> kill (); KLoc (loc, ctx)
    | ctx -> KLoc (loc, ctx)

  and lift_quant vs ctx =
    if vs = Univ ([], [], []) || vs = Exist ([], [], []) then
      ctx
    else match vs,ctx with
      | Exist (xs, ys, eqns), KExists (xs', ys', eqns', ctx') ->
        let xs = List.fold_left Misc.insert_nodup xs xs' in
        let ys = List.fold_left Misc.insert_nodup ys ys' in
        kill (); KExists (xs, ys, eqns@eqns', ctx')
      | Univ (us, vs, eqns), KForall (us', vs', eqns', ctx') ->
        let us = List.fold_left Misc.insert_nodup us us' in
        let vs = List.fold_left Misc.insert_nodup vs vs' in
        kill (); KForall (us, vs, eqns@eqns', ctx')
      | Exist (xs, ys, eqns), _ -> KExists (xs, ys, eqns, ctx)
      | Univ (us, vs, eqns), _ -> KForall (us, vs, eqns, ctx)
      | Neutral eqns, KExists (us, vs, eqns', ctx) -> KExists (us, vs, eqns @ eqns', ctx)
      | Neutral eqns, KForall (us, vs, eqns', ctx) -> KForall (us, vs, eqns @ eqns', ctx)
      | Neutral _, KLoc (loc, ctx) -> KLoc (loc, lift_quant vs ctx)
      | Neutral _, KAnd (cs, ctx, cs') -> KAnd (cs, lift_quant vs ctx, cs')
      | Neutral _, KCases _ -> assert false
      | Neutral eqns, KEmpty -> KAnd ([PEqn eqns], KEmpty, [])
  in
  let c = advance c KEmpty in
  if not !canary then
    compress_logic ~remove_loc c
  else
    advance c KEmpty


end

module FullFOL =  struct

  open Vars
  open Types

  include FOL(struct
      type var = TyVar.t
      type sort = SortVar.t Types.sort
      type rel = RelVar.t
      type term = typ

      let pp_var = TyVar.pp
      let pp_rel = RelVar.pp
      let pp_sort = pp_sort SortVar.to_string
      let pp_term = pp_typ TyConsVar.pp TyVar.pp
    end)

  let freevars_of_typ t =
    let rec go = function
      | TCons _ -> []
      | TInternal v | TVar {node=v;_} -> [v]
      | TApp {tfun;args;_} -> List.concat (List.map go (tfun::args)) in
    List.fold_left insert_nodup [] (go t)

  let freevars_of_eqn eqn = match eqn with
    | Rel (_, ts) -> List.fold_left
                       (fun acc t -> List.fold_left insert_nodup acc (freevars_of_typ t))
                       []
                       ts
    | Eq (t,u,_) -> List.fold_left insert_nodup (freevars_of_typ t) (freevars_of_typ u)

  let freevars_of_eqns eqns =
    List.fold_left
      (fun acc eqn -> List.fold_left insert_nodup acc (freevars_of_eqn eqn))
      []
      eqns

  let rec freevars_of_formula f = match f with
    | PTrue | PFalse -> []
    | PEqn eqns -> freevars_of_eqns eqns
    | PLoc (_, f) -> freevars_of_formula f
    | PAnd fs | PCases fs ->
      List.fold_left
        (fun acc t -> List.fold_left insert_nodup acc (freevars_of_formula t))
        []
        fs
    | PForall (xs, ys, eqns, f) | PExists (xs, ys, eqns, f) ->
      let bound = xs @ ys in
      let fvs = List.fold_left insert_nodup [] (freevars_of_eqns eqns) in
      let fvs = List.fold_left insert_nodup fvs (freevars_of_formula f) in
      List.filter (fun x -> not (List.mem x bound)) fvs

end
