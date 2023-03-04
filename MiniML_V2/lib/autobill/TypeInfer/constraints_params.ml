open Constraint
open Types
open Vars
open Ast
open Prelude
open FullAst

module type Prelude = sig
  val it : prelude
end

module Params (Prelude : Prelude) = struct

  type sort = SortVar.t Types.sort

  type rel = RelVar.t

  let string_of_rel = RelVar.to_string

  let sort_of_rel rel = RelVar.Env.find rel !(Prelude.it).relations

  let is_valid_sort so = (Types.is_base_index_sort so)
                         || (Types.is_monotype_sort_with_base_indices so)

  let is_syntactic_sort = function
    | Base _ | Qualifier -> true
    | Index _ | Arrow _ -> false

  let pp_rel = RelVar.pp

  let pp_var = TyVar.pp

  let pp_sort = pp_sort SortVar.to_string

  let pp_deep = pp_typ TyConsVar.pp TyVar.pp

  type node =
    | Var of TyVar.t * sort
    | Cons of TyConsVar.t type_cons

  type deep = typ

  type var = TyVar.t

  let eq a b = a = b

  let string_of_sort = Types.string_of_sort SortVar.to_string

  let string_of_node = function
    | Var (v,sort) -> TyVar.to_string v ^ ":" ^ string_of_sort sort
    | Cons c ->
      pp_type_cons TyConsVar.pp Format.str_formatter c;
      Format.flush_str_formatter ()

  let pp_node fmt node = Format.pp_print_string fmt (string_of_node node)

  let sort_of_cons = function
    | Var (_,so) -> unmk_arrow so
    | Cons c -> unmk_arrow (def_of_tycons Prelude.it c).sort

  let deep_of_var s = TInternal s

  let mk_var = TyVar.fresh

  let deep_of_cons args k =
    match k, args with
    | Var (v,_), _ -> tvar v
    | Cons c, args ->
      if args = [] then (cons c) else app (cons c) args

  let folded_of_deep fold_var deep =
    let fold k args = Fold (Shallow (k, args)) in
    let subst = ref TyVar.Env.empty in
    let add v (typ_opt, so) = (subst := TyVar.Env.add v (typ_opt,so) !subst) in
    let get v =
      match TyVar.Env.find_opt v !subst with
      | Some x -> x
      | None -> (None, TyVar.Env.find v !(Prelude.it).sorts)
    in

    let rec go deep : 'a folded = match deep with

      | TVar {node;_} | TInternal node ->
        begin
          let typ_opt, sort =
            try get node with
              Not_found ->
              let sort =
                try TyVar.Env.find node !(Prelude.it).sorts
                with Not_found -> raise (Failure (TyVar.to_string node)) in
              (None, sort) in
          match typ_opt with
          | None -> fold_var node sort
          | Some typ -> go typ
        end

      | TCons {node; _} -> begin match node with

          | Types.Unit | Zero | Top | Bottom | Qual _ as c -> fold (Cons c) []

          | Cons c ->
            let def = def_of_tycons Prelude.it (Cons c) in
            begin match def.content with
              | Defined typ ->
                assert (def.args = []); (* Constructors must be fully applied *)
                go typ 
              | _ -> match def.sort with
                | Base _ | Index _ | Qualifier -> fold (Cons (Cons c)) []
                | Arrow _ -> assert false (* Constructors must be fully applied *)
            end

          | Prod _ | Choice _ | Sum _ | Fun _ | Closure | Fix | Thunk -> assert false
          (* this is treated further down *)
        end

      | TApp {tfun; args = []; _} -> go tfun

      | TApp {tfun = TApp {tfun; args = args'; _}; args; loc} ->
        go (TApp {loc; tfun; args = args' @ args})

      | TApp {tfun = TCons {node;_}; args; _} -> begin
          match node with
          | Prod _ | Sum _ | Choice _ | Fun _ | Thunk | Closure | Fix as c ->
            fold (Cons c) (List.map go args)
          | Cons c -> begin
              let def = def_of_tycons Prelude.it (Cons c) in
              match def.content with
              | Defined typ -> begin
                  List.iter2 (fun (x,so) y -> add x (Some y,so)) def.args args;
                  go typ
                end
              | _ -> fold (Cons (Cons c)) (List.map go args)
            end
          | _ -> assert false (* this is treated above *)
        end
      | TApp {tfun= TVar {node; _}; args; loc} ->
        begin match get node with
          | None, so -> fold (Var (node, so)) (List.map go args)
          | Some typ, _ -> go (TApp {tfun = typ; args; loc})
        end
      | TApp {tfun=_;_} -> assert false (* by sorting rules, this can't happen *)

    in
    go deep

end
