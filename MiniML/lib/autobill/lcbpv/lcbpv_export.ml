open Vars
open Lcbpv
open Cst
open Types
open Primitives
open Misc

exception Invalid_type of string * position

exception Sums_with_many_args of position

exception Many_program_bodies

let fail_sums_with_many_args loc = raise (Sums_with_many_args loc)

let fail_wrong_type (t, loc) =
  let t = Format.(
      Lcbpv_Printer.pp_typ str_formatter (t,loc);
      flush_str_formatter ()) in
  let mess = "When desugaring, found an invalid type annotation. This \
              application is not allowed: " ^ t in
  raise (Invalid_type (mess, loc))

let mk_var s =
  let v = Global_counter.fresh_int () in
  s ^ "__" ^ string_of_int v

let mk_vars n s = List.init n (fun _ -> mk_var s)

let export_bin_primop loc (op : prim_bin_op) =
  let op = match op with
    | Add -> op_add
    | Mult -> op_mul
    | Subs -> op_sub
    | Div -> op_div
    | Mod -> op_mod
    | And -> op_and
    | Or -> op_or
    | Int_Eq -> op_eq
    | Int_Leq -> op_leq
    | Int_Lt -> op_lt in
  V.var ~loc (Var.to_string op)

let export_mon_primop (op : prim_mon_op) =
  let op = match op with
    | Opp -> op_op
    | Not -> op_not in
  V.var (Var.to_string op)

let export_box_kind = function
  | Lin -> Types.Linear
  | Aff -> Types.Affine
  | Exp -> Types.Exponential

 let export_sort = function
    | Pos -> Base Positive
    | Neg -> Base Negative

open Types

let rec export_type (t,loc) = match t with
  | Typ_App((Typ_Int, _), []) | Typ_Int -> Cst.prim_type_int
  | Typ_App((Typ_Bool, _), []) | Typ_Bool -> Cst.prim_type_int
  | Typ_App((Typ_Unit, _), []) | Typ_Unit -> cons ~loc Unit
  | Typ_App((Typ_Zero, _), []) | Typ_Zero -> cons ~loc Zero
  | Typ_App((Typ_Top, _), []) | Typ_Top -> cons ~loc Top
  | Typ_App((Typ_Bottom, _), []) | Typ_Bottom -> cons ~loc Bottom

  | Typ_App((Typ_Tuple, loc2), xs) ->
    app ~loc (cons ~loc:loc2 (Prod (List.length xs))) (List.map export_type xs)
  | Typ_App((Typ_Sum, loc2), xs) ->
    app ~loc (cons ~loc:loc2 (Sum (List.length xs))) (List.map export_type xs)
  | Typ_App((Typ_Fun, loc2), xs) ->
    app ~loc (cons ~loc:loc2 (Fun (List.length xs))) (List.map export_type xs)
  | Typ_App((Typ_LazyPair, loc2), xs) ->
    app ~loc (cons ~loc:loc2 (Choice (List.length xs))) (List.map export_type xs)

  | Typ_App((Typ_Closure q, _), [x]) ->
    boxed ~loc (Some (export_box_kind q)) (export_type x)
  | Typ_App((Typ_Thunk, loc2), [x]) ->
    app ~loc (cons ~loc:loc2 Thunk) [export_type x]

  | Typ_App((Typ_Var v, _), []) | Typ_Var v -> tvar ~loc v
  (* Types in LCBPV are unparametrized, so applied type variables must be constructors *)
  | Typ_App((Typ_Var c, loc2), xs) -> app ~loc (cons ~loc:loc2 (Cons c)) (List.map export_type xs)

  | t -> fail_wrong_type (t,loc)

open Constructors

let rec eval_then ((_,loc) as e) cont =
  let a = mk_var "a" in
  let x = mk_var "x" in
  V.bindcc ~loc a None (go e |~| S.bind ~loc x None (cont x a))

and eval_many_then loc es cont =
  let a = mk_var "a" in
  let xs = mk_vars (List.length es) "x" in
  let aux cmd ((_,loc) as arg) var = (go arg) |~| S.bind ~loc var None cmd in
  let cmd = List.fold_left2 aux (cont xs a) es xs in
  V.bindcc ~loc a None cmd

and go (e, loc) = match e with

  | Expr_Var (x, _) -> V.var ~loc x

  | Expr_Int n -> V.cons ~loc (Constructors.cons (Int n) [] [])

  | Expr_Constructor (c, args) -> go_cons loc c args

  | Expr_Method (e, m, args) -> go_method loc e m args 

  | Expr_Closure (q, e) ->
    let a = mk_var "a" in
    V.box ~loc (export_box_kind q) a None (go e |-| S.ret ~loc a)

  | Expr_Thunk e -> V.cons ~loc (thunk (go e))

  | Expr_Get cases ->
    V.case ~loc (List.map (fun (GetPatTag (m, xs, e, _)) -> go_method_patt loc m xs e) cases)

  | Expr_Match (e, cases) ->
    let a = mk_var "a" in
    V.bindcc ~loc a None ((go e) |+| go_matches loc a cases)

  | Expr_Rec ((x, loc2), e) ->
    let self = mk_var "self" in
    let c = mk_var "c" in
    let self_val = V.box ~loc Exponential c None
        (V.var ~loc self |+| S.box ~loc Exponential (S.cofix ~loc (S.ret ~loc c))) in
    let a = mk_var "a" in
    let b = mk_var "b" in
    let cmd = self_val |+| S.bind ~loc:loc2 x None (go e |-| S.ret ~loc b) in
    V.bindcc ~loc a None ((V.fix ~loc (self, None) (b, None) cmd) |-| S.cofix ~loc (S.ret a))

  | Expr_Block (Blk (instrs, ret, loc)) ->
    let a = mk_var "a" in
    V.bindcc ~loc a None (go_block loc instrs ret a)

  | Expr_Bin_Prim (op, a, b) ->
    let call x y a =
      let z = mk_var "z" in
      (export_bin_primop loc op
       |-| S.destr ~loc (call [V.var x; V.var y]
                           (S.case [thunk (z, None) |=> (V.var z |+| S.ret a)]))) in
    eval_then a (fun x a -> eval_then b (fun y b -> call x y b) |~| S.ret a)

  | Expr_Mon_Prim (op, e) ->
    eval_then e (fun x a ->
        let z = mk_var "z" in
        export_mon_primop op
        |-| S.destr (call [V.var x] (S.case [thunk (z, None) |=> (V.var z |+| S.ret a)])))

  | Expr_If (b, e1, e2) ->
    go (Expr_Match (b, [MatchPatTag (True,[],e1, loc); MatchPatTag (False,[],e2, loc)]), loc)

and go_cons loc c es = match c with
  | Cons_Named c ->
    eval_many_then loc es
      (fun xs a -> V.cons ~loc (cons (PosCons c) [] (List.map V.var xs)) |+| S.ret ~loc a)
  | Unit -> V.cons ~loc unit
  | True -> V.cons ~loc (cons (Bool true) [] [])
  | False -> V.cons ~loc (cons (Bool false) [] [])
  | Int_Litt n -> V.cons ~loc (cons (Int n) [] [])
  | Tuple ->
    eval_many_then loc es (fun xs a -> V.cons (tuple (List.map V.var xs)) |+| S.ret a)
  | Inj (i, n) ->
    match es with
    | [e] -> eval_then e (fun x a -> V.cons (inj i n (V.var x)) |+| S.ret a)
    | _ -> fail_sums_with_many_args loc

and go_matches loc a cases =
  let default = ref None in
  let go_case = function
    | MatchPatTag (c, xs, e, loc) -> Some (go_cons_patt loc c xs e a)
    | MatchPatVar ((x, _), e, loc) ->
      default := Some ((x, None), (go e) |~| S.ret ~loc a);
      None in
  S.case ~loc ~default:!default (List.filter_map go_case cases)

and go_method loc e m es  =
  eval_then e (fun x b ->
      eval_many_then loc es (fun ys c ->
          match m with
          | Method_Named m, _ ->
            V.var ~loc x |-| S.destr ~loc (destr (NegCons m) [] (List.map V.var ys) (S.ret c))
          | Call, _ ->
            V.var ~loc x |-| S.destr ~loc (call (List.map V.var ys) (S.ret ~loc c))
          | Proj (i, n), _ -> match ys with
            | [] -> V.var ~loc x |-| S.destr ~loc (proj i n (S.ret ~loc c))
            | _ -> fail_sums_with_many_args loc)
      |~| S.ret ~loc b)

and go_method_patt loc m xs e =
  let xs = List.map (fun (x,_) -> (x, None)) xs in
  let a = mk_var "a", None in
  let patt = match m with
    | Method_Named m, _ -> destr (NegCons m) [] xs a
    | Call, _ -> call xs a
    | Proj (i, n), _ -> match xs with
      | [] -> proj i n a
      | _ -> fail_sums_with_many_args loc
  in
  patt |=> (go e |~| S.ret ~loc (fst a))

and go_cons_patt loc c ys e a =
  let ys = List.map (fun (y, _) -> (y, None)) ys in
  let patt = match c with
    | Cons_Named c -> cons (PosCons c) [] ys
    | Unit -> unit
    | True -> cons (Bool true) [] []
    | False -> cons (Bool true) [] []
    | Int_Litt n -> cons (Int n) [] []
    | Tuple -> tuple ys
    | Inj (i, n) -> match ys with
      | [y] -> inj i n y
      | _ -> fail_sums_with_many_args loc in
  patt |=> (go e |~| S.ret ~loc a)

and go_block loc instrs ret a =
  let cmd = go ret |~| S.ret ~loc a in
  List.fold_left go_instr cmd (List.rev instrs)

and go_instr cmd (instr, loc) = match instr with
  | Ins_Let ((x, _), e) -> (go e) |~| S.bind ~loc x None cmd
  | Ins_Force ((x, _), e) -> (go e) |~| (S.case ~loc [thunk (x, None) |=> cmd])
  | Ins_Open ((x, _), q, e) -> (go e) |~| (S.box ~loc (export_box_kind q) (S.bind ~loc x None cmd))

let go_toplevel (Blk (instrs, ret, loc)) =
  let rec go_instr = function
    | (Ins_Let ((x, _),e), loc) :: t ->
      Term_definition {
        name = x;
        typ = None;
        content = go e;
        loc
      } :: go_instr t
    | (Ins_Force ((x,_),e), loc) :: t ->
      Term_definition {
        name = x;
        typ = None;
        content =
          (let z = mk_var "z" in
          eval_then e (fun y a ->
              V.var ~loc y |-| (S.case ~loc [thunk (z, None) |=> (V.var z |+| S.ret a)])));
        loc;
      }:: go_instr t
    | (Ins_Open ((x,_),q,e), loc) :: t -> Term_definition {
        name = x;
        typ = None;
        content =
          (eval_then e (fun y a ->
              V.var ~loc y |-| (S.box ~loc (export_box_kind q) (S.ret a))));
        loc;
      } :: go_instr t
    | [] ->
      let a = mk_var "a" in
      Cmd_execution {
        name = None;
        typ = None;
        cont = a;
        content =  go ret |~| S.ret ~loc a;
        loc;
      } :: [] in
  go_instr instrs

let go_program_items (Prog p) : Cst.program =

  let rec go p = match p with

    | Typ_Decl ((name, _), args, rets, loc) :: rest ->
      Type_declaration {
        name; loc;
        sort = sort_arrow (List.map export_sort args) (export_sort rets);
      } :: go rest
    | Value_Decl ((x, _), t, loc) :: rest ->
      Term_declaration {
        name = x;
        typ = export_type t;
        loc;
      } :: go rest
    | Typ_Def (x, args, Def_Synonym (t, so), loc) :: rest ->
      Type_definition {
        name = x;
        args = List.map (fun (x,so) -> (x, export_sort so)) args;
        sort = export_sort so;
        content = export_type t;
        loc;
      } :: go rest
    | Typ_Def (x, args, Def_Datatype conses, loc) :: rest ->
      Data_definition {
        name = x;
        args = List.map (fun (x,so) -> (x, export_sort so)) args;
        content = List.map
            (fun (c, args) -> cons (PosCons c) [] (List.map export_type args), [])
            conses;
        loc;
      } :: go rest
    | Typ_Def (x, args, Def_Computation destrs, loc) :: rest ->
      Codata_definition {
        name = x;
        args = List.map (fun (x,so) -> (x, export_sort so)) args;
        content = List.map
            (fun (d, args, ret) ->
               destr (NegCons d) [] (List.map export_type args) (export_type ret), [])
            destrs;
        loc;
      } :: go rest

    | Do blk :: [] -> go_toplevel blk

    | _ -> raise Many_program_bodies

  in

  go p

let export_prog = go_program_items
