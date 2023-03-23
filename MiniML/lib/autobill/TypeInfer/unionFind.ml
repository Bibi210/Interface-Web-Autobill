open Format

exception Unify of int * int
exception UnifySort of int * int
exception Cycle of int
exception Occured
exception Unbound of int
exception BadArity of int * int
exception Undefined of int
exception SortConflict of string * string
exception InvalidSort of string

type uvar = int

type ('a, 'b) _shallow = Var of uvar | Shallow of 'a * ('b list)

type 'a shallow = ('a, uvar) _shallow

type 'a folded = Fold of ('a, 'a folded) _shallow

module type Unifier_params = sig
  type node
  type deep
  type sort
  type var
  type rel
  val sort_of_rel : rel -> sort list
  val sort_of_cons : node -> (sort list * sort)
  val is_valid_sort : sort -> bool
  val is_syntactic_sort : sort -> bool
  val eq : node -> node -> bool
  val mk_var : unit -> var
  val deep_of_var : var -> deep
  val deep_of_cons : deep list -> node -> deep
  val folded_of_deep : (var -> sort -> node folded) -> deep -> node folded
  val rank_relation : rel -> int list -> int list
  val pp_rel : formatter -> rel -> unit
  val pp_sort : formatter -> sort -> unit
  val pp_node : formatter -> node -> unit
  val pp_var : formatter -> var -> unit
  val pp_deep : formatter -> deep -> unit
end

module Make (P : Unifier_params) = struct


  module UFOL = FirstOrder.FOL(struct
      type sort = P.sort
      type var = uvar
      type term = uvar
      type rel = P.rel
      open Format
      let pp_sort = P.pp_sort
      let pp_rel = P.pp_rel
      let pp_var fmt var = pp_print_int fmt var
      let pp_term = pp_var
    end)

  module FFOL = FirstOrder.FOL(struct
      include P
      type term = deep
      let pp_term = P.pp_deep
    end)

  include P
  include UFOL

  let pp_uvar fmt u = pp_print_int fmt u

  let pp__shallow pp fmt = function
    | Var u -> pp_uvar fmt u
    | Shallow (node, args) ->
      fprintf fmt "(%a %a)"
        pp_node node
        (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp) args

  let pp_shallow = pp__shallow pp_uvar

  let rec pp_folded fmt (Fold sh) = pp__shallow pp_folded fmt sh


  module S = Map.Make(struct
      type t = uvar
      let compare = compare
    end)

  type rank = int

  type cell =
    | Redirect of uvar
    | Trivial of rank
    | Cell of P.node shallow * rank

  type subst = cell S.t

  type scheme = uvar list * uvar

  let _state = ref S.empty

  let _var_env : (P.var * uvar) list ref = ref []

  let _sorts = ref S.empty

  let add_sort u so =
    let s = pp_sort str_formatter so; flush_str_formatter () in
    if not (is_valid_sort so) then
      raise (InvalidSort s);
    if S.mem u !_sorts then
      raise (Failure ("double sort declaration for " ^ string_of_int u));
    _sorts := S.add u so ! _sorts

  let get_sort u =
    try S.find u !_sorts with _ -> raise (Undefined u)

  let sort_check u v =
    if get_sort u <> get_sort u then raise (UnifySort (u,v))

  let fail_bad_cons_sort folded sort =
    let sort = pp_sort str_formatter sort; flush_str_formatter () in
    let folded = pp_folded str_formatter folded; flush_str_formatter () in
    raise (SortConflict (folded, sort))

  let pp_cell fmt (u,c) =
    let sort = get_sort u in
    match c with
    | Redirect v -> fprintf fmt "%d -> %d : %a" u v pp_sort sort
    | Trivial r -> fprintf fmt "%d -> r%d free : %a" u r pp_sort sort
    | Cell (sh,r) -> fprintf fmt "%d -> r%d %a : %a" u r pp_shallow sh pp_sort sort

  let pp_subst fmt s =
    pp_print_list ~pp_sep:pp_print_newline pp_cell fmt (S.bindings s)

  let _rank : rank ref = ref 0

  let enter () = incr _rank

  let leave () = decr _rank


  let set k v = _state := S.add k v !_state

  let fresh_u so =
    let u = Global_counter.fresh_int () in
    set u (Trivial !_rank);
    add_sort u so;
    u

  let shallow ?rank:r ~sort:so sh =
    let r = Option.value r ~default:!_rank in
    let u = fresh_u so in
    set u (Cell (sh, r));
    u

  let rec of_folded ~sort rank sh = match sh with
    | Fold (Var x) -> x, [x]
    | Fold (Shallow (k, xs)) ->
      let (sos, rets) = sort_of_cons k in
      if sort <> rets then fail_bad_cons_sort sh sort;
      let xs, fvs =
        List.split @@ List.map2 (fun sort x -> of_folded rank ~sort x) sos xs in
      let y = shallow ~rank ~sort:rets (Shallow (k,xs)) in
      y, y::List.concat fvs

  let of_user_var ~sort ~rank a =
    match List.assoc_opt a !_var_env with
    | Some u ->
      if get_sort u <> sort then
        let a = P.pp_var str_formatter a; flush_str_formatter () in
        let sort = pp_sort str_formatter sort; flush_str_formatter () in
        raise (SortConflict (a, sort))
      else
        u
    | None ->
      let u = fresh_u sort in
      _var_env := List.cons (a,u) !_var_env;
      set u (Trivial rank);
      u

  let of_rank1_typ ?rank:(rank=(!_rank)) ~sort deep =
    let of_var v sort = Fold (Var (of_user_var ~sort ~rank v)) in
    let sh = folded_of_deep of_var deep in
    of_folded ~sort rank sh

  let of_tvars vs =
    List.map (fun (v,sort) -> of_user_var ~rank:!_rank ~sort v) vs

  let compress u v =
    sort_check u v;
    if u <> v then set u (Redirect v)

  let traverse u =
    let rec loop v old = try match S.find v !_state with
      | Redirect w | Cell (Var w, _)-> loop w v
      | c -> v, Some c
      with _ -> old, None in
    let v,c = loop u u in
    compress u v; v,c

  let repr u =
    let v,_ = traverse u in v

  let cell u =
    let _, c = traverse u in c

  let rank u = match cell u with
    | None -> -1
    | Some (Trivial r) | Some (Cell (_, r)) -> r
    | Some (Redirect _) -> assert false

  let lower_rank v k =
    match traverse v with
    | v, Some (Trivial k') -> set v (Trivial (min k k'))
    | v, Some (Cell (c,k')) -> set v (Cell (c, min k k'))
    | _, None -> ()
    | _, Some (Redirect _) -> assert false

  let occurs_check u =
    let rec go old u =
      if List.mem (repr u) old then
        raise Occured
      else if is_syntactic_sort (get_sort u) then
        let old = (repr u) :: old in
        match cell u with
        | None ->
          raise (Failure ("Invariant break: variable in scheme is dangling: "
                          ^ string_of_int u))
        | Some (Redirect _) -> assert false
        | Some (Trivial _) -> ()
        | Some (Cell (sh,_)) -> go_sh old sh

    and go_sh old sh = match sh with
      | Var u -> go old u
      | Shallow (_, xs) -> List.iter (go old) xs
    in
    try go [] u; true
    with Occured -> false


  let unify u v =

    let non_syntactic_unifications = ref [] in

    let add u v =
      let r = min (rank u) (rank v) in
      lower_rank u r;
      lower_rank v r;
      non_syntactic_unifications := (u,v)::!non_syntactic_unifications in

    let redirect urep vrep cell =
      set urep cell;
      set vrep (Redirect urep) in

    let rec go u v =
      sort_check u v;
      let (urep, uc), (vrep, vc) = traverse u, traverse v in
      if is_syntactic_sort (get_sort urep) then
        if urep = vrep then ()
        else
          match uc, vc with
          | None, _ -> compress urep vrep
          | _, None -> compress vrep urep
          | Some uc, Some vc -> go_cell uc urep vc vrep
      else
        add u v

    and go_cell uc urep vc vrep =
      match uc, vc with
      | Trivial ur, Trivial vr -> redirect urep vrep (Trivial (min ur vr))
      | Trivial tr, Cell (sh, cr)
      | Cell (sh,cr), Trivial tr ->  redirect urep vrep (Cell (sh, min cr tr))
      | Redirect _, _ | _, Redirect _
      | Cell (Var _, _), _ | _, Cell (Var _, _) -> assert false
      | Cell (Shallow (uk, uxs),ur), Cell (Shallow (vk,vxs),vr) ->
        go_sh (min ur vr) urep uk uxs vrep vk vxs

    and go_sh rank urep uk uxs vrep vk vxs =
      if eq uk vk then
        try
          List.iter2 go uxs vxs;
          redirect urep vrep (Cell (Shallow (uk, uxs), rank))
        with Invalid_argument _ -> raise (BadArity (u,v))
      else
        raise (Unify (urep,vrep)) in

    go u v;
    if not (occurs_check u) then begin
      pp_subst std_formatter !_state;
      pp_print_flush std_formatter ();
      raise (Cycle u);
    end;
    !non_syntactic_unifications

  let freevars_of_type u =
    let rec go fvs u = match cell u with
      | Some (Cell (Var _ ,_))
      | Some (Redirect _) -> assert false
      | None -> raise (Failure "Invariant break: variable in scheme in dangling")
      | Some (Trivial _) -> u::fvs
      | Some (Cell (Shallow (_, xs), _)) ->
        let fvs = if is_syntactic_sort (get_sort u) then fvs else u::fvs in
        List.fold_left go fvs xs
    in
    go [] u

  let freevars_of_eqn = function
    | Eq (a,b,_) -> [a;b]
    | Rel (_,args) -> args

  let ranked_freevars fvs r =
    let a = Array.make (r+1) [] in
    let aux v =
      lower_rank v r;
      let r' = rank v in
      if r' > -1 then (* ignoring primitive types who have rank = -1 *)
        a.(r') <- v :: a.(r') in
    List.iter aux fvs;
    a

  (* HACK: First-Order shouldn't have anything to do here *)
  let refresh_scheme (us, u, eqns) =
    let env = ref [] in
    let us = List.map repr us in
    let copy old c =
      let young = fresh_u (get_sort old) in
      env := (repr old,young) :: !env;
      set young c;
      young in
    let rec go v =
      let vrep, cell = traverse v in
      match List.assoc_opt vrep !env with
      | Some w -> w
      | None -> match cell with
         | Some (Cell (Shallow (k, xs),r)) ->
           copy vrep (Cell (Shallow (k, List.map go xs), r))
         | Some (Trivial r) ->
           if not (List.mem vrep us) then vrep
           else copy vrep (Trivial r)
         | Some (Redirect _)
         | Some (Cell (Var _, _))
         | None -> assert false in
    let go_eq =
      function
      | UFOL.Eq (u,v,so) -> UFOL.Eq (go u, go v, so)
      | Rel (r, us) -> Rel (r, List.map go us) in
    (List.map go us, go u, List.map go_eq eqns)

  (* Assuming all relevant freevars of rank less than [r] are maximally
   * lifted, lift the freevars [vs], of rank [r], at their lowest rank *)
  let lift_freevars r vs : unit =
    let rec aux children =
      List.fold_left (fun acc child -> go child; max acc (rank child))
        (-1) children
    and go v =
      begin
        lower_rank v r;
        match cell v with
        | None -> ()
        | Some c -> match c with
          | Redirect _
          | Trivial _ -> ()
          | Cell (sh, vr) -> go_sh v vr sh
      end
    and go_sh v vr = function
      | Var _ -> ()
      | Shallow (_, []) -> ()
      | Shallow (_, args) -> if vr = r then lower_rank v (aux args)
    in
    List.iter go vs

  (* filters the quantifiers of a scheme that that under a given rank,
   * returns the filtered scheme and the low-ranked variables separately *)
  let extract_old_vars us r  =
    let rec test u =
      match cell u with
      | Some (Trivial r') -> r <= r'
      | Some (Cell (Var u, _)) -> test u
      | Some (Cell (Shallow _, r')) -> r <= r'
      | Some (Redirect _)
      | None -> false in
    let young, old = List.partition test us in
  young, old

  let _nvar_env : (int * scheme) list ref = ref []

  let define x (us, u) =
    let s = (List.map repr us, repr u) in
    _nvar_env := (x, s) :: !_nvar_env

  let pp_env fmt () =
    let pp fmt (v,(us,u)) =
      fprintf fmt "v%d -> ∀%a. %a"
        v
        (pp_print_list ~pp_sep:pp_print_space pp_uvar) us
        pp_uvar u in
    pp_print_list ~pp_sep:pp_print_newline pp fmt !_nvar_env

  type output_env = {
    u : uvar -> deep;
    var : int -> deep;
    get : uvar -> P.var
  }

  let finalize_env () : output_env =

    (* NOTE: Exporting stops registering new vars from type annotations *)

    let ven = ref (List.map (fun (a, u) -> (repr u, a)) !_var_env) in

    let get u : P.var =
      let u = repr u in
      match List.assoc_opt u !ven with
      | Some a -> a
      | None ->
        let a = mk_var () in
        if Option.is_none (cell u) then
          set u (Trivial (-2));
        ven := (u, a) :: !ven;
        a in

    let rec aux u =
      let urep, cell = traverse u in
      match cell with
      | None ->
        raise (Failure "export")
      | Some (Trivial _) -> deep_of_var (get urep)
      | Some (Redirect _) | Some (Cell (Var _, _)) -> assert false
      | Some (Cell (Shallow (k, args), _)) ->
        deep_of_cons (List.map aux args) k
    in

    let aux2 x =
      if occurs_check x then aux x else raise (Cycle x)
    in
    {
      u = aux2;
      (* TODO we can't generalise variables yet *)
      var = (fun x -> match List.assoc_opt x !_nvar_env with
          | None -> raise (Unbound x)
          | Some (_,u) -> aux2 u);
      get
    }

  let reset_unifier () =
    _rank := 0;
    _sorts := S.empty;
    _state := S.empty;
    _var_env := [];

end
