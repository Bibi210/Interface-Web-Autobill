open FirstOrder
open Types

include FullFOL

type 'term fol_var_multiplicity =
  | Out_of_scope
  | Not_used
  | Only_Root of 'term
  | Some_Non_Root
  | Dont_substitute

let remove_useless_vars con =

  let warn v =
    Printf.printf "variable out of scope: %s\n" (Vars.TyVar.to_string v) in

  let module S = Vars.TyVar.Env in

  let _vars = ref S.empty in

  let add var term =
    let upd x = match x with
      | None -> warn var; Some Out_of_scope
      | Some Not_used -> Some (Only_Root term)
      | Some (Only_Root _) -> Some Some_Non_Root
      | Some (Dont_substitute | Out_of_scope | Some_Non_Root) -> x in
    _vars := S.update var upd !_vars in

  let add_binder status var = _vars := S.add var status !_vars in

  let add_term var =
    let upd x = match x with
      | None -> warn var; Some Out_of_scope
      | Some (Not_used | Only_Root _) -> Some Some_Non_Root
      | Some (Dont_substitute | Out_of_scope | Some_Non_Root) -> x in
    _vars := S.update var upd !_vars in

  let rec fill_out c = match c with
    | PTrue | PFalse -> ()
    | PLoc (_, c) -> fill_out c
    | PEqn eqns -> List.iter fill_out_eqn eqns
    | PAnd cs -> List.iter fill_out cs
    | PForall (vars, exist, eqns, c) ->
      List.iter (add_binder Dont_substitute) vars;
      List.iter (add_binder Not_used) exist;
      List.iter fill_out_eqn eqns;
      fill_out c
    | PExists (vars, eqns, c) ->
      List.iter (add_binder Not_used) vars;
      List.iter fill_out_eqn eqns;
      fill_out c

  and fill_out_eqn = function
    (* If the two terms are identical vars, then we don't register the trivial
       definition "x = x" *)
    | Eq (((TVar {node=node1;_} | TInternal node1) as v1),
          ((TVar {node=node2;_} | TInternal node2) as v2), _) ->
      if node1 != node2 then begin
        match S.find_opt node1 !_vars, S.find_opt node2 !_vars with
        | (None | Some Out_of_scope), (None | Some Out_of_scope) ->
          warn node2; warn node1;
          add_binder Out_of_scope node1;
          add_binder Out_of_scope node2
        | (None | Some Out_of_scope), _ ->
          warn node1; add_binder Out_of_scope node1; add_term node2
        | _, (None | Some Out_of_scope) ->
          warn node2; add_term node1; add_binder Out_of_scope node2
        | Some Dont_substitute, Some Dont_substitute ->
          add_term node1; add_term node2
        | Some Dont_substitute, _ ->
          add node1 v2; add_term node2
        | _ -> add node2 v1; add_term node1
      end
    | Eq ((TVar {node;_} | TInternal node), term, _)
    | Eq (term, (TVar {node;_} | TInternal node), _) ->
      begin match S.find_opt node !_vars with
      | None | Some Out_of_scope ->
        warn node; add_binder Out_of_scope node; fill_out_term term
      | _ -> add node term; fill_out_term term
      end
    | Eq (term1, term2, _) -> fill_out_term term1; fill_out_term term2
    | Rel (_, terms) -> List.iter fill_out_term terms

  and fill_out_term = function
    | TVar {node;_} | TInternal node -> add_term node
    | TPos typ -> fill_out_term typ
    | TNeg typ -> fill_out_term typ
    | TFix t -> fill_out_term t
    | TBox {node;_} -> fill_out_term node
    | TCons _ -> ()
    | TApp {tfun;args;_} ->
      fill_out_term tfun; List.iter fill_out_term args in

  let replace vars =
    let aux x = match S.find_opt x !_vars with
      | None -> assert false
      | Some Out_of_scope -> warn x; true
      | Some Dont_substitute -> true
      | Some (Only_Root _) -> false
      | Some Not_used -> true
      | Some Some_Non_Root -> true in
    List.filter aux vars in

  let rec subst typ = match typ with
    | TVar {node;_} | TInternal node ->
      begin match S.find_opt node !_vars with
        | Some (Only_Root t) -> t
        | _ -> TInternal node
      end
    | TPos typ -> subst typ
    | TNeg typ -> subst typ
    | TFix t -> TFix (subst t)
    | TBox {kind;node;loc} -> TBox {kind; loc; node = subst node}
    | TCons c -> TCons c
    | TApp {tfun;args;loc} ->
      TApp {tfun = subst tfun; args = List.map subst args; loc} in

  let rec go c = match c with
    | PTrue | PFalse -> c
    | PLoc (loc, c) -> PLoc (loc, go c)
    | PEqn eqns -> PEqn (List.map go_eqn eqns)
    | PAnd cs -> PAnd (List.map go cs)
    | PExists (vars, eqns, c) ->
      PExists (replace vars, List.map go_eqn eqns, go c)
    | PForall (vars, exist, eqns, c) ->
      PForall (replace vars, replace exist, List.map go_eqn eqns, go c)

  and go_eqn = function
    | Eq (a, b, so) -> Eq (subst a, subst b, so)
    | Rel (rel, args) -> Rel (rel, List.map subst args) in

  fill_out con; go con


let normalize con = (compress_logic (remove_useless_vars con))
