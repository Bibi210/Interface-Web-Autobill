open Misc
open Types
open Vars
open Prelude
open Preprocess_ast
open Constructors

exception Double_definition of string * position

exception Bad_sort of string * position

exception Undefined_identifier of string * position

exception Invalid_Goal of string

let fail_too_many_cons_parameters num expected (tag : ConsVar.t constructor_tag) loc =
  let open Format in
  pp_constructor_tag Preprocess_ast.pp_cons_val_aux str_formatter tag;
  let tag = flush_str_formatter () in
  let mess = Printf.sprintf "The constructor %s expects at most %d parameters, but was given %d"
      tag expected num in
  raise (Bad_sort (mess, loc))

let fail_too_many_destr_parameters num expected (tag : DestrVar.t destructor_tag) loc =
  let open Format in
  pp_destructor_tag Preprocess_ast.pp_cons_val_aux str_formatter tag;
  let tag = flush_str_formatter () in
  let mess = Printf.sprintf "The destructor %s expects at most %d parameters, but was given %d"
      tag expected num in
  raise (Bad_sort (mess, loc))

let fail_double_def mess loc =
  raise (Double_definition (Printf.sprintf "%s is defined twice" mess, loc))

let fail_bad_sort loc expected actual =
  let mess = Printf.sprintf "expected %s, got %s"
      (string_of_sort SortVar.to_string actual)
      (string_of_sort SortVar.to_string expected) in
  raise (Bad_sort (mess, loc))

let fail_undefined_type name loc =
  raise (Undefined_identifier ("type " ^ name, loc))

let fail_undefined_sort name loc =
  raise (Undefined_identifier ("sort " ^ name, loc))

let fail_cant_apply_type cons loc =
  raise (Bad_sort ("this type cannot be applied: " ^ cons, loc))

let fail_bad_constructor tag loc =
  raise (Double_definition ("constructor " ^ tag, loc))

let fail_bad_destructor tag loc =
  raise (Double_definition (tag, loc))

let fail_undefined_rel rel loc = raise (Undefined_identifier ("relation " ^ rel, loc))

let fail_higher_order_arg name loc =
  raise (Bad_sort ("cannot have a higher-order type here: " ^ name, loc))

let fail_undefined_var var loc = raise (Undefined_identifier ("variable " ^ var, loc))

let fail_undefined_cons cons loc = raise (Undefined_identifier ("constructor " ^ cons, loc))

let fail_undefined_destr destr loc = raise (Undefined_identifier ("destructor " ^ destr, loc))

let fail_invalid_goal_degree () = raise (Invalid_Goal "the degree is not a natural integer")

let fail_not_a_polynomial () = raise
    (Invalid_Goal "the goal must be a type of sort (nat -> nat -> ... -> nat)")

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

type intern_env = {
  sort_vars : SortVar.t StringEnv.t;
  rels : RelVar.t StringEnv.t;
  tycons_vars : TyConsVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : SortVar.t Types.sort TyConsVar.Env.t;
  prelude_typevar_sort : SortVar.t Types.sort TyVar.Env.t;
  prelude : prelude;
  }


let initial_env () = {
  prelude = Prelude.empty_prelude ();

  sort_vars = StringEnv.empty;
  rels = StringEnv.empty;
  tycons_vars = StringEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
  definitions = StringEnv.empty;

  tycons_sort = TyConsVar.Env.empty;
  prelude_typevar_sort = TyVar.Env.empty;
}

let dump_env fmt env =
  let open Format in
  let aux pp_k pp_v k v = Format.fprintf fmt "%a : %a@," pp_k k pp_v v in
  begin
    pp_print_newline fmt ();
    pp_open_vbox fmt 0;
    pp_print_string fmt "####### Internal state";
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of constructor";
    pp_print_cut fmt ();
    TyConsVar.Env.iter (aux TyConsVar.pp pp_sort) env.tycons_sort;
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of type variables";
    pp_print_cut fmt ();
    TyVar.Env.iter (aux TyVar.pp pp_sort) env.prelude_typevar_sort;
    pp_print_cut fmt ();
    pp_print_newline fmt ()
  end

type scope = {
  vars : Var.t StringEnv.t;
  covars : CoVar.t StringEnv.t;
  tyvars : TyVar.t StringEnv.t;
}


let empty_scope = {
  vars = StringEnv.empty;
  covars = StringEnv.empty;
  tyvars = StringEnv.empty;
}

let add_var scope v =
  {scope with vars = StringEnv.add v (Var.of_string v) scope.vars}

let add_covar scope a =
  {scope with covars = StringEnv.add a (CoVar.of_string a) scope.covars}

let add_tyvar scope t =
  {scope with tyvars = StringEnv.add t (TyVar.of_string t) scope.tyvars}

let add_sort scope t =
  {scope with tyvars = StringEnv.add t (TyVar.of_string t) scope.tyvars}

let get_var scope v = StringEnv.find v scope.vars

let get_covar scope a = StringEnv.find a scope.covars

let get_tyvar scope t = StringEnv.find t scope.tyvars

