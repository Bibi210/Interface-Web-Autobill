open Vars
open Constructors
open Ast
open FullAst
open Format


let constraint_as_string (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let x,_ = elab_prog_items items in
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

  let rec goval (MetaVal v) = gopreval v.node

  and gostk (MetaStack s) = goprestk s.node

  and gocmd (Command c) = goval c.valu; gostk c.stk

  and gopreval = function
    | Var _ | CoTop -> ()
    | Bindcc {bind; cmd; _} | Box {bind; cmd; _} ->
      bind_covar bind; gocmd cmd
    | Fix {self; cont; cmd} ->
      bind_var self;
      bind_covar cont;
      gocmd cmd
    | Cons (Raw_Cons cons) -> List.iter goval cons.args
    | Destr {default; cases} ->
      List.iter (fun (Raw_Destr patt, cmd) ->
          List.iter bind_var patt.args;
          bind_covar patt.cont;
          gocmd cmd
        ) cases;
      Option.iter (fun (a,cmd) -> bind_covar a; gocmd cmd) default


  and goprestk = function
    | Ret _  | CoZero -> ()
    | CoBind {bind; cmd; _} -> bind_var bind; gocmd cmd
    | CoBox {stk;_} -> gostk stk
    | CoFix stk -> gostk stk
    | CoDestr (Raw_Destr destr) -> List.iter goval destr.args; gostk destr.cont
    | CoCons {cases; default} ->
      List.iter (fun (Raw_Cons patt,cmd) ->
          List.iter bind_var patt.args;
          gocmd cmd
        ) cases;
      Option.iter (fun (a,cmd) -> bind_var a; gocmd cmd) default
  in

  let goitem = function
    | Value_declaration {bind; _} -> bind_var bind
    | Value_definition {bind; content; _} -> goval content; bind_var bind
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
