open Vars
open FirstOrder
open FullFOL
open Primitives
open Format
open Ast
open FullAst

open Polynomials

exception Invariant_break_not_convertible_to_optimization of string

let fail_untranslatable mess = raise (Invariant_break_not_convertible_to_optimization mess)

let convert_to_optimization f (Goal goal : goal) =

  let globals = ref [] in
  let mk_param () =
    let p = TyVar.fresh () in
    globals := p :: !globals;
    p in

  let goal_args : TyVar.t list =
    let names = ["X"; "Y"; "Z"; "T"; "S"; "R"; "N"; "M"; "P"; "Q"] in
    List.init goal.args_number (fun i ->
        if i < List.length names then
          TyVar.of_string (List.nth names i)
        else
          TyVar.fresh ()) in
  let goal_poly, output  =
    let callback _ = mk_param () in
    let poly = free_poly ~callback ~base:goal_args ~degree:goal.degree in
    let output =
      pp_set_geometry str_formatter ~max_indent:1000000000 ~margin:1000000000000;
      fprintf str_formatter "%a(%a) = %a"
        (TyConsVar.pp ~debug:false) goal.polynomial
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") (TyVar.pp ~debug:false)) goal_args
        (Poly.pp ~for_mzn:true) poly;
      flush_str_formatter ()
    in
    (poly, output) in


  let env = ref TyVar.Env.empty in
  let env_add x t = (env := TyVar.Env.add x t !env) in
  let env_get x = TyVar.Env.find x !env in
  let add_var x = env_add x (Poly.of_mono (Mono.of_var x)) in
  let add_param vars x =
    let p = free_poly ~callback:(fun _ -> mk_param ()) ~base:vars ~degree:goal.degree in
    env_add x p
  in

  let rec convert_term (t:term) = match t with
    | TVar {node=v;_} | TInternal v -> env_get v
    | TCons {node=Cons c;_} -> begin
        if c = nat_zero then Poly.zero
        else if c = nat_one then Poly.unit
        else if c = nat_large then Poly.scale (Scalar.of_int 1000) (Poly.of_mono Mono.unit)
        else fail_untranslatable "Undefined constant"
      end
    | TApp {tfun = TCons {node = Cons c; _}; args; _} ->
      if c = nat_add then
        List.fold_left (fun acc t -> Poly.add acc (convert_term t)) Poly.zero args
      else if c = nat_mult then
        List.fold_left (fun acc t -> Poly.mult acc (convert_term t)) Poly.zero args
      else if c = goal.polynomial then begin
        if (List.length args <> goal.args_number) then
          Misc.fail_invariant_break
            "Infered polynomial cannot be evaluated due to bad sorting";
        let args = List.map convert_term args in
        let instanciate_args v =
          let rec find_idx i = function
            | w::t -> if v=w then i else find_idx (i+1) t
            | [] ->
              Misc.fail_invariant_break "Infered polynomial has an unexpected free variable" in
          List.nth args (find_idx 0 goal_args) in
        Poly.subst instanciate_args goal_poly
      end
      else
        fail_untranslatable "Application with an uninterpreatble head"
    | TApp {tfun = TApp {tfun;args=args1;_}; args=args2; loc} ->
      convert_term (TApp {tfun; args = args1 @ args2; loc})
    | TApp {tfun = (TVar _ | TInternal _ | TCons _); _} ->
      fail_untranslatable "Application with an uninterpreatble head"
    | TCons _ ->
      fail_untranslatable "Undefined constructor"
  in

  let convert_eqn eqn =
    match eqn with
    | Eq (t,u,_) -> Poly.sub (convert_term t) (convert_term u)
    | Rel _ -> fail_untranslatable "unsupported relation" in

  let convert_eqns (eqns : eqn list) = List.map convert_eqn eqns in

  let rec convert_fol vars (f : formula) =
    match f with
    | PTrue -> []
    | PFalse -> fail_untranslatable "Refutation patterns are unsupported"
    | PLoc (_, f) -> convert_fol vars f
    | PEqn eqns -> convert_eqns eqns
    | PAnd fs | PCases fs -> List.concat (List.map (convert_fol vars) fs)
    | PForall (xs,ys,eqns,f) ->
      List.iter add_var (xs@ys);
      if eqns <> [] then
        fail_untranslatable "Implications are unsupported"
      else
        convert_fol (xs@ys@vars) f
    | PExists (xs,ys,eqns,f) ->
      List.iter (add_param vars) (xs@ys);
      (convert_eqns eqns) @ (convert_fol vars f) in

  let f = convert_fol [] f in
  let goal = Poly.eval (fun _ -> Scalar.of_int 1000) goal_poly in
  (!globals, f, goal, output)


let pp_solution fmt (globals, polys, goal, output) =
  let pp_global fmt v = fprintf fmt "var int: %a; constraint %a >= 0;"
      (TyVar.pp ~debug:true) v (TyVar.pp ~debug:true) v in
  let pp_scalar fmt (_,v) = fprintf fmt "constraint %a = 0;" (Scalar.pp ~for_mzn:false) v in
  let pp_poly fmt p = pp_print_list ~pp_sep:pp_print_newline pp_scalar fmt (Poly.P.bindings p) in
  let pp_goal fmt p = fprintf fmt "solve minimize %a;" (Scalar.pp ~for_mzn:false) p in
  let pp_output fmt s = fprintf fmt "output [\"%s\"]" s in
  fprintf fmt "@[<v 0>%a@.%a@.%a@.%a@.@]"
    (pp_print_list pp_global) globals
    (pp_print_list ~pp_sep:pp_print_newline pp_poly) polys
    pp_goal goal
    pp_output output

let convert_to_minizinc_file goal post_con =
  try
    let post_con = AaraCompress.compress_unification post_con in
    let res = convert_to_optimization post_con goal in
    pp_solution Format.str_formatter res;
    Format.flush_str_formatter ()
  with
  | Invariant_break_not_convertible_to_optimization info ->
    Misc.fatal_error "Generating complexity model" info
