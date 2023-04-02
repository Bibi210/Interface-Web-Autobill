open Vars
open Types
open FirstOrder
open FullFOL
open Format

let pp_tyvar fmt v = pp_print_string fmt (String.lowercase_ascii (TyVar.to_string v))

let pp_tyconsvar fmt v = pp_print_string fmt (String.lowercase_ascii (TyConsVar.to_string v))

let pp_relvar fmt v = pp_print_string fmt (String.lowercase_ascii (RelVar.to_string v))

let pp_tycons fmt c = match c with
  | Cons c -> pp_tyconsvar fmt c
  | _ -> Misc.fail_invariant_break "A parameter constructor is not a type constructor variable"

let pp_list pp fmt l = pp_print_list ~pp_sep:pp_print_space pp fmt l

let pp_and pp fmt l = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,/\\ ") pp fmt l

let rec pp_typ fmt (t:typ) = match t with
| TCons c -> pp_tycons fmt c.node
| TApp {tfun; args = []; _} -> pp_typ fmt tfun
| TApp {tfun; args; _} ->
  fprintf fmt "(%a %a)" pp_typ tfun (pp_list pp_typ) args
| TVar {node;_} | TInternal node -> pp_tyvar fmt node

let pp_eqn fmt = function
  | Eq (a,b,_) -> fprintf fmt "@[(%a = %a)@]" pp_typ a pp_typ b
  | Rel (r, args) -> fprintf fmt "@[(%a %a)@]" pp_relvar r (pp_list pp_typ) args

let pp_eqns fmt = function
  | [] -> fprintf fmt "True"
  | [eqn] -> pp_eqn fmt eqn
  | eqn::eqns ->  fprintf fmt "@[<v 2>    (%a@,/\\ %a)@]" pp_eqn eqn (pp_and pp_eqn) eqns

let pp_binder str fmt vs =
  let pp_bind fmt v = fprintf fmt "(%a:nat)" pp_tyvar v in
  if vs = [] then ()
  else fprintf fmt "@[%s %a,@]@," str (pp_list pp_bind) vs


let rec pp_formula fmt (c : formula) = match c with
| PTrue -> fprintf fmt "True"
| PFalse -> fprintf fmt "False"
| PLoc (loc, c) -> fprintf fmt "@[<v 0>(* at location %s *)@,%a@]"
                     (Misc.string_of_position loc)
                     pp_formula c
| PEqn eqns -> pp_eqns fmt eqns
| PAnd [] | PCases [] -> fprintf fmt "True"
| PAnd [c] | PCases [c] -> pp_formula fmt c
| PAnd cs | PCases cs -> fprintf fmt "@[<v 0>(%a)@]" (pp_and pp_formula) cs
| PExists ([], [], eqns, c) -> pp_formula fmt (PAnd [PEqn eqns; c])
| PExists (xs, ys, eqns, c) ->
  fprintf fmt "@[<v 0>(%a %a@,   /\\ %a)@]"
    (pp_binder "exists") (xs@ys)
    pp_eqns eqns
    pp_formula c
| PForall (xs, ys, eqns, c) ->
  fprintf fmt "@[<v 0>(%a %a %a@,   -> %a)@]"
    (pp_binder "forall") (ys)
    (pp_binder "forall") (xs)
    pp_eqns eqns
    pp_formula c


let nat_prelude =
{|Definition add x y := x+y.
Definition one := 1.
Definition z := 0.|}

let export_as_coq_term c =
  let open Format in
  fprintf str_formatter "%s@.Goal %a.@." nat_prelude pp_formula c;
  flush_str_formatter ()
