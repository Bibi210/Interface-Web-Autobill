open Lcbpv
open Cst
open Types
open Constructors

let mk_var s =
  let v = Global_counter.fresh_int () in
  s ^ "_" ^ string_of_int v

let mk_vars n s = List.init n (fun _ -> mk_var s)

let op_add = mk_var "_add"

let op_sub = mk_var "_sub"

let op_mul = mk_var "_mul"

let op_div = mk_var "_div"

let op_mod = mk_var "_mod"

let op_and = mk_var "_and"

let op_or = mk_var "_or"

let op_leq = mk_var "_leq"

let op_lt = mk_var "_lt"

let op_eq = mk_var "_eq"

let op_not = mk_var "_not"

let op_op = mk_var "_op"

let export_bin_primop (op : prim_bin_op) =
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
  V.var op

let export_mon_primop (op : prim_mon_op) =
  let op = match op with
    | Opp -> op_op
    | Not -> op_not in
  V.var op

let export_box_kind = function
  | Lin -> Types.Linear
  | Aff -> Types.Affine
  | Exp -> Types.Exponential

 let export_sort = function
    | Pos -> Base Positive
    | Neg -> Base Negative

let rec export_type = function
  | Typ_App(Typ_Int, []) -> cons Int
  | Typ_App(Typ_Bool, []) -> cons Bool
  | Typ_App(Typ_Unit, []) -> cons Unit
  | Typ_App(Typ_Zero, []) -> cons Zero
  | Typ_App(Typ_Top, []) -> cons Top
  | Typ_App(Typ_Bottom, []) -> cons Bottom
  | Typ_App(Typ_Tuple, xs) ->
    app (cons (Prod (List.length xs))) (List.map export_type xs)
  | Typ_App(Typ_Sum, xs) ->
    app (cons (Sum (List.length xs))) (List.map export_type xs)
  | Typ_App(Typ_Fun, xs) ->
    app (cons (Fun (List.length xs))) (List.map export_type xs)
  | Typ_App(Typ_LazyPair, xs) ->
    app (cons (Choice (List.length xs))) (List.map export_type xs)
  | Typ_App(Typ_Closure q, [x]) ->
    boxed (export_box_kind q) (export_type x)
  | Typ_App(Typ_Thunk, [x]) ->
    thunk_t (export_type x)
  | Typ_App(Typ_Var v, []) -> tvar v
  | Typ_App(Typ_Var c, xs) -> app (cons (Cons c)) (List.map export_type xs)
  | Typ_Var v -> tvar v
  | _ -> assert false

let rec eval_then e cont =
  let a = mk_var "a" in
  let x = mk_var "x" in
  V.bindcc a None (go e |~| S.bind x None (cont x a))

and eval_many_then es cont =
  let a = mk_var "a" in
  let xs = mk_vars (List.length es) "x" in
  let aux cmd arg var = (go arg) |~| S.bind var None cmd in
  let cmd = List.fold_left2 aux (cont xs a) es xs in
  V.bindcc a None cmd

and go (e : Lcbpv.expression) = match e with

  | Expr_Var x -> V.var x

  | Expr_Int n -> V.cons (Int n)

  | Expr_Constructor (c, args) -> go_cons c args

  | Expr_Method (e, m, args) ->
    let a = mk_var "a" in
    V.bindcc a None (go_method e m args a)

  | Expr_Closure (q, e) ->
    let a = mk_var "a" in
    V.box (export_box_kind q) a None (go e |-| S.ret a)

  | Expr_Thunk e -> V.cons (thunk (go e))

  | Expr_Get cases ->
    V.case (List.map (fun (GetPat (m, xs, cmd)) -> go_method_patt m xs cmd) cases)

  | Expr_Match (e, cases) ->
    let a = mk_var "a" in
    V.bindcc a None ((go e) |+|
                     (S.case (List.map
                                (fun (MatchPat (c, xs , e)) -> go_cons_patt c xs e a)
                                cases)))

  | Expr_Rec (x, e) ->
    let self = mk_var "self" in
    let c = mk_var "c" in
    let self_val = V.box exp c None (V.var self |+| S.box exp (S.cofix (S.ret c))) in
    let a = mk_var "a" in
    let b = mk_var "b" in
    let cmd = self_val |+| S.bind x None (go e |-| S.ret b) in
    V.bindcc a None ((V.fix (self, None) (b, None) cmd) |-| S.cofix (S.ret a))

  | Expr_Block (Blk (instrs, ret)) ->
    let a = mk_var "a" in
    V.bindcc a None (go_block instrs ret a)

  | Expr_Bin_Prim (op, a, b) ->
    let call x y a =
      let z = mk_var "z" in
      (export_bin_primop op
       |-| S.destr (call [V.var x; V.var y]
                      (S.case [thunk (z, None) |=> (V.var z |+| S.ret a)]))) in
    eval_then a (fun x a -> eval_then b (fun y b -> call x y b) |~| S.ret a)

  | Expr_Mon_Prim (op, e) ->
    eval_then e (fun x a ->
        let z = mk_var "z" in
        export_mon_primop op
        |-| S.destr (call [V.var x] (S.case [thunk (z, None) |=> (V.var z |+| S.ret a)])))

  | Expr_If (b, e1, e2) ->
    go (Expr_Match (b, [MatchPat (True,[],e1); MatchPat (False,[],e2)]))

and go_block instrs ret a =
  let cmd = go ret |~| S.ret a in
  List.fold_left go_instr cmd (List.rev instrs)

and go_instr cmd instr = match instr with
  | Ins_Let (x, e) -> (go e) |~| S.bind x None cmd
  | Ins_Force (x, e) -> (go e) |~| (S.case [thunk (x, None) |=> cmd])
  | Ins_Open (x, q, e) -> (go e) |~| (S.box (export_box_kind q) (S.bind x None cmd))


and go_cons c es = match c with
  | Cons_Named c ->
    eval_many_then es (fun xs a -> V.cons (poscons c [] (List.map V.var xs)) |+| S.ret a)
  | Unit -> V.cons unit
  | True -> V.cons (Bool true)
  | False -> V.cons (Bool false)
  | Int_Litt n -> V.cons (Int n)
  | Tuple ->
    eval_many_then es (fun xs a -> V.cons (Tupple (List.map V.var xs)) |+| S.ret a)
  | Inj (i, n) ->
    match es with
    | [e] -> eval_then e (fun x a -> V.cons (Inj (i, n, V.var x)) |+| S.ret a)
    | _ -> assert false


and go_method e m es a =
  eval_then e (fun x b ->
      eval_many_then es (fun ys c ->
          match m with
          | Method_Named m -> V.var x |-| S.destr (negcons m [] (List.map V.var ys) (S.ret c))
          | Call -> V.var x |-| S.destr (call (List.map V.var ys) (S.ret c))
          | Proj (i, n) -> match ys with
            | [] -> V.var x |-| S.destr (Proj (i, n, S.ret c))
            | _ -> assert false)
      |~| S.ret b)
  |~| S.ret a

and go_method_patt m xs e =
  let xs = List.map (fun x -> (x, None)) xs in
  let a = mk_var "a", None in
  let patt = match m with
    | Method_Named m -> negcons m [] xs a
    | Call -> call xs a
    | Proj (i, n) -> match xs with
      | [] -> Proj(i, n, a)
      | _ -> assert false
  in
  patt |=> (go e |~| S.ret (fst a))

and go_cons_patt c ys e a =
  let ys = List.map (fun y -> (y, None)) ys in
  let patt = match c with
    | Cons_Named c -> poscons c [] ys
    | Unit -> unit
    | True -> Bool true
    | False -> Bool false
    | Int_Litt n -> Int n
    | Tuple -> Tupple ys
    | Inj (i, n) -> match ys with
      | [y] -> Inj (i, n, y)
      | _ -> assert false in
  patt |=> (go e |~| S.ret a)


let go_program_item ( i : Lcbpv.program_item ) =
  match i with
  | Typ_Decl (name, args, rets) ->
    Type_declaration {
      name;
      sort = sort_arrow (List.map export_sort args) (export_sort rets);
      loc = Misc.dummy_pos
    }
  | Value_Decl (x, t) ->
    Term_declaration {
      name = x;
      typ = export_type t;
      loc = Misc.dummy_pos
    }
  | Typ_Def (x, args, Def_Synonym (t, so)) ->
    Type_definition {
      name = x;
      args = List.map (fun (x,so) -> (x, export_sort so)) args;
      sort = export_sort so;
      content = export_type t;
      loc = Misc.dummy_pos
    }
  | Typ_Def (x, args, Def_Datatype conses) ->
    Data_definition {
      name = x;
      args = List.map (fun (x,so) -> (x, export_sort so)) args;
      content = List.map
          (fun (cons, args) -> poscons cons [] (List.map export_type args), [])
          conses;
      loc = Misc.dummy_pos
    }
  | Typ_Def (x, args, Def_Computation destrs) ->
     Codata_definition {
      name = x;
      args = List.map (fun (x,so) -> (x, export_sort so)) args;
      content = List.map
          (fun (destr, args, ret) ->
             negcons destr [] (List.map export_type args) (export_type ret), [])
          destrs;
      loc = Misc.dummy_pos
    }
  | Do (Blk (instrs, ret)) ->
    let a = mk_var "a" in
    Cmd_execution {
      name = None;
      typ = None;
      cont = a;
      content = go_block instrs ret a;
      loc = Misc.dummy_pos
    }

let export_prog (Prog p) = List.map go_program_item p
