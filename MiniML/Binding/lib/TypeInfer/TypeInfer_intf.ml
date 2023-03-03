open Vars
open Constructors
open Ast
open FullAst
open Format


let constraint_as_string (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let x,_ = elab_prog_items items in
  (* let x = (compress_cand (float_cexists x)) in *)
  pp_set_geometry str_formatter ~max_indent:130 ~margin:200;
  pp_constraint str_formatter x;
  pp_print_newline str_formatter ();
  pp_print_newline str_formatter ();
  pp_subst str_formatter !_state;
  flush_str_formatter ()

let post_contraint_as_string (prelude, _, post) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let post = FOLNormalize.normalize post in
  FOLNormalize.pp_formula str_formatter post;
  pp_print_newline str_formatter ();
  pp_print_newline str_formatter ();
  pp_subst str_formatter !_state;
  flush_str_formatter ()

let fill_out_types items =

  let vars = ref (Var.Env.empty) in

  let covars = ref (CoVar.Env.empty) in

  let bind_var (v,t) = vars := Var.Env.add v t !vars in

  let bind_covar (a,t) = covars := CoVar.Env.add a t !covars in

  let gocons c k = match c with
    | Unit | Int _ | Bool _ -> ()
    | Thunk x | Inj (_, _, x) ->  k x
    | Tupple xs | PosCons (_, _, xs) -> List.iter k xs in

  let godestr d kx ka = match d with
    | Closure a | Proj (_, _, a) -> ka a
    | Call (xs, a) | NegCons (_, _, xs, a) -> List.iter kx xs; ka a in


  let rec goval (MetaVal v) = gopreval v.node

  and gostk (MetaStack s) = goprestk s.node

  and gocmd (Command c) = goval c.valu; gostk c.stk

  and gopreval = function
    | Var _ | CoTop -> ()
    | Bindcc {bind; cmd; _} | Box {bind; cmd; _} ->
      bind_covar bind; gocmd cmd
    | Fix {self=(x,_); cont=(a,t); cmd} ->
      bind_var (x, t);
      bind_covar (a, t);
      gocmd cmd
    | Cons c -> gocons c goval
    | Destr patts -> patts |> List.iter (fun (patt, cmd) ->
        godestr patt bind_var bind_covar;
        gocmd cmd
      )

  and goprestk = function
    | Ret _  | CoZero -> ()
    | CoBind {bind; cmd; _} -> bind_var bind; gocmd cmd
    | CoBox {stk;_} -> gostk stk
    | CoFix stk -> gostk stk
    | CoDestr d -> godestr d goval gostk
    | CoCons patts -> patts |> List.iter (fun (patt,cmd) ->
        gocons patt bind_var;
        gocmd cmd
      ) in

  let goitem = function
    | Value_declaration {name; typ; _} -> bind_var (name, typ)
    | Value_definition {name; content; typ; _} -> goval content; bind_var (name, typ)
    | Command_execution {content; _} -> gocmd content

  in
  List.iter goitem items;
  !vars, !covars


let type_infer ~trace:trace (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make (P) in
  let items,post = go ~trace items in
  let vars, covars = fill_out_types items in
  prelude := {!prelude with vars; covars};
  let post : FirstOrder.FullFOL.formula = Obj.magic post in
  (prelude, items, post)
