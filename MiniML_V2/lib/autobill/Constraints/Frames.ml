open Vars
open FirstOrder
open FullFOL
open Format

type frame = Frame of (TyVar.t list * eqn list)

type frame_tree =
  | Leaf of bool
  | Loc of Misc.position * frame_tree
  | Witness of frame * frame_tree
  | And of frame_tree list
  | Or of frame_tree list

type frame_tree_ctx =
  | KAnd of frame_tree list * frame_tree list
  | KOr of frame_tree list * frame_tree list

let frame v e = Frame (v,e)

let pp_vars fmt vars =
  fprintf fmt "@[(%a)@]" (pp_print_list TyVar.pp) vars

let pp_frame fmt (Frame (v,e))=
  fprintf fmt "@[<v 1>(:let %a@ :model %a)@]" pp_vars v pp_eqns e

let rec pp_frame_tree fmt (tree : frame_tree) = match tree with
  | Leaf b -> fprintf fmt ":%a" pp_print_bool b
  | Loc (loc, c) ->
    fprintf fmt "@[<v 1>(:loc %s@ :then %a)@]" (Misc.string_of_position loc) pp_frame_tree c
  | Witness (frame, c) ->
    fprintf fmt "@[<v 1>(:frame %a@ :then %a)@]" pp_frame frame pp_frame_tree c
  | And cs -> fprintf fmt "@[<v 1>(:and %a)@]" (pp_print_list pp_frame_tree) cs
  | Or cs -> fprintf fmt "@[<v 1>(:or %a)@]" (pp_print_list pp_frame_tree) cs

let rec formula_to_frame_tree = function
  | PTrue -> Leaf true
  | PFalse -> Leaf false
  | PLoc (loc, c) -> Loc (loc, formula_to_frame_tree c)
  | PEqn eqns -> Witness (frame [] eqns, Leaf true)
  | PAnd cs -> And (List.map formula_to_frame_tree cs)
  | PCases cs -> Or (List.map formula_to_frame_tree cs)
  | PExists (xs, ys, eqns, c) -> Witness (frame (xs@ys) eqns, formula_to_frame_tree c)
  | PForall (xs, ys, eqns, c) -> Witness (frame (xs@ys) eqns, formula_to_frame_tree c)
