open Vars
open FirstOrder
open FullFOL
open Primitives
open Format

module Scalar = struct

  type t =
    | Param of var
    | Cst of int
    | Mult of t * t
    | Add of t * t

  let of_int n = Cst n
  let of_param v = Param v
  let unit = of_int 1
  let zero = of_int 0
  let mult a b = Mult (a,b)
  let add a b = Add (a,b)

  let rec simplify = function
    | Param v -> Param v
    | Cst n -> Cst n
    | Mult (a,b) ->
      begin match simplify a, simplify b with
        | Cst a, Cst b -> Cst (a*b)
        | Cst 1, a | a, Cst 1 -> a
        | a,b -> Mult (a,b)
      end
    | Add (a,b) ->
      begin match simplify a, simplify b with
        | Cst a, Cst b -> Cst (a+b)
        | Cst 0, a | a, Cst 0 -> a
        | a,b -> Add (a,b)
      end

  let rec pp fmt x = match simplify x with
    | Param v -> fprintf fmt "%a" TyVar.pp v
    | Cst n -> pp_print_int fmt n
    | Mult (a,b) -> fprintf fmt "(%a * %a)" pp a pp b
    | Add (a,b) -> fprintf fmt "(%a + %a)" pp a pp b

end

module Mono = struct

  type t =
    | Var of var
    | Cst

  let unit = Cst
  let of_var v = Var v
  let mult a b = match a,b with
    | Cst, Cst -> Cst
    | Var v, Cst | Cst, Var v -> Var v
    | Var _, Var _ -> failwith "unimplemented"

  let pp fmt coeff = function
    | Var v -> fprintf fmt "(%a * ?%a)" Scalar.pp coeff TyVar.pp v
    | Cst -> Scalar.pp fmt coeff

end

module Poly = struct

  module P = Map.Make (struct type t = Mono.t let compare = compare end)

  type t = Scalar.t P.t

  let zero = P.empty
  let of_mono m = P.singleton m (Scalar.of_int 1)
  let unit = of_mono Cst
  let scale a p = P.map (fun b -> Scalar.mult a b) p
  let add p q = P.union (fun _ a b -> Some (Scalar.add a b)) p q
  let sub p q = add p (scale (Scalar.of_int (-1)) q)
  let mult p q =
    let res = ref zero in
    let add_term p = (res := add !res p) in
    P.iter (fun p a ->
        P.iter (fun q b ->
          add_term (scale (Scalar.mult a b) (of_mono (Mono.mult p q)))
          ) q
      ) p;
    !res

  let pp fmt p =
    if p = P.empty then pp_print_string fmt "0" else
    let p = P.bindings p in
    let pp_one fmt (m,a) = Mono.pp fmt a m in
    let pp_sep fmt () = fprintf fmt " + " in
    (pp_print_list ~pp_sep pp_one) fmt p

end


let convert_to_optimization callback f goal =

  let globals = ref [] in
  let mk_param () =
    let p = TyVar.fresh () in
    globals := p :: !globals;
    p in

  let env = ref TyVar.Env.empty in
  let env_add x t = (env := TyVar.Env.add x t !env) in
  let env_get x = TyVar.Env.find x !env in

  let add_var x = env_add x (Poly.of_mono (Mono.of_var x)) in
  let add_param vars x =
  let t = List.fold_left (fun p v ->
        let a = mk_param () in
        Poly.add p (Poly.scale (Scalar.of_param a) (Poly.of_mono (Mono.of_var v)))
      ) Poly.zero vars in
  let cst_term = mk_param () in
    env_add x (Poly.add t (Poly.scale (Param cst_term) Poly.unit))
  in

  let rec convert_term (t:term) = match t with
    | TVar {node=v;_} | TInternal v -> env_get v
    | TCons {node=Cons c;_} -> begin
        if c = nat_zero then Poly.zero
        else if c = nat_one then Poly.unit
        else failwith "unimplemented parameter constant"
        end
    | TApp {tfun = TCons {node = Cons c; _}; args; _} ->
      if c = nat_add then
        List.fold_left (fun acc t -> Poly.add acc (convert_term t)) Poly.zero args
      else if c = nat_mult then
        List.fold_left (fun acc t -> Poly.mult acc (convert_term t)) Poly.zero args
      else
        callback c (List.map convert_term args)
    | _ -> assert false (* Impossible on indices *) in

  let convert_eqn eqn =
    match eqn with
    | Eq (t,u,_) -> Poly.sub (convert_term t) (convert_term u)
    | Rel _ -> failwith "unimplemented" in

  let convert_eqns (eqns : eqn list) =
    List.map convert_eqn eqns in

  let rec convert_fol vars (f : formula) =
    match f with
    | PTrue -> []
    | PFalse -> failwith "unimplemented"
    | PLoc (_, f) -> convert_fol vars f
    | PEqn eqns -> convert_eqns eqns
    | PAnd fs | PCases fs -> List.concat (List.map (convert_fol vars) fs)
    | PForall (xs,ys,eqns,f) ->
      List.iter add_var (xs@ys);
      assert (eqns = []);
      convert_fol (xs@ys@vars) f
    | PExists (xs,ys,eqns,f) ->
      List.iter (add_param vars) (xs@ys);
      (convert_eqns eqns) @ (convert_fol vars f) in

  let f = convert_fol [] f in
  let goal = convert_term goal in
  (!globals, f, goal)


let pp_solution fmt (globals, polys, goal) =
  let pp_global fmt v = fprintf fmt "var int: %a; constraint %a >= 0;" TyVar.pp v TyVar.pp v in
  let pp_scalar fmt (_,v) = fprintf fmt "constraint %a = 0;" Scalar.pp v in
  let pp_poly fmt p = pp_print_list pp_scalar fmt (Poly.P.bindings p) in
  let pp_goal fmt p = fprintf fmt "solve minimize %a;" Poly.pp p in
  fprintf fmt "@[<v 0>%a@.%a@.%a@.@]"
    (pp_print_list pp_global) globals
    (pp_print_list pp_poly) polys
    pp_goal goal
