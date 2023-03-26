open Misc
open Types
open Vars
open Ast
open Prelude

module USortVar = LocalVar (struct
    let default_name = "pol"
  end)

exception Double_definition of string

exception Bad_sort of {
    loc : string;
    actual : string;
    expected : string;
  }

exception Undefined_type of {
    name : string;
    loc : position;
  }

exception Undefined_sort of {
    name : string;
  }

exception Bad_type_cons_arity of {
    cons : string;
    loc : position;
  }

exception Bad_constructor_name of {
    loc : position
  }

exception Higher_order_type_argument of {
    loc : position;
    name : string
  }

exception Ambiguous_polarity of position

exception Undefined_var of string * position

exception Undefined_constructor of string * position

exception Undefined_destructor of string * position

exception Undefined_relation of string * position

exception Sort_mismatch of string * string * position * position

let fail_double_def mess loc =
  raise (Double_definition
           (Printf.sprintf "%s: FATAL the %s is already defined"
              (string_of_position loc)
              mess))

let fail_bad_sort loc expected actual =
  raise (Bad_sort {loc;
                   actual = string_of_sort SortVar.to_string actual;
                   expected = string_of_sort SortVar.to_string expected})

let fail_undefined_type name loc =
  raise (Undefined_type {name; loc})

let fail_undefined_sort name =
  raise (Undefined_sort {name})

let fail_bad_arity cons loc =
  raise (Bad_type_cons_arity {cons; loc})

let fail_bad_constructor loc =
  raise (Bad_constructor_name {loc})

let fail_undefined_rel rel loc = raise (Undefined_relation (loc, rel))

let fail_higher_order_arg name loc =
  raise (Higher_order_type_argument {name; loc})

let fail_ambiguous_sort loc = raise (Ambiguous_polarity loc)

let fail_undefined_var var loc = raise (Undefined_var (var, loc))

let fail_undefined_cons cons loc = raise (Undefined_constructor (cons, loc))

let fail_undefined_destr destr loc = raise (Undefined_destructor (destr, loc))



type usort =
  | Loc of position * usort
  | Litt of SortVar.t Types.sort
  | Redirect of USortVar.t

let rec string_of_usort = function
  | Loc (pos, upol) -> Printf.sprintf "%s@\"%s\"" (string_of_usort upol)  (string_of_position pos)
  | Litt so -> string_of_sort SortVar.to_string so
  | Redirect var -> USortVar.to_string var

let fail_polarity_mismatch upol1 upol2 pos1 pos2 =
  raise (Sort_mismatch (string_of_usort upol1, string_of_usort upol2, pos1, pos2))


module InternAstParams = struct
  include FullAstParams
  type polarity = usort
  type sort = usort
  type type_bind = TyVar.t * usort
end

module InternAst = Ast (InternAstParams)

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

type sort_check_env = {
  prelude : prelude;

  sort_vars : SortVar.t StringEnv.t;
  rels : RelVar.t StringEnv.t;
  tycons_vars : TyConsVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : SortVar.t sort TyConsVar.Env.t;
  prelude_typevar_sort : SortVar.t sort TyVar.Env.t;

  varsorts : USortVar.t Var.Env.t;
  covarsorts : USortVar.t CoVar.Env.t;
  tyvarsorts : usort TyVar.Env.t;
  unifier : usort USortVar.Env.t;
  }


let empty_sortcheck () = {
  prelude = Prelude.empty_prelude ();

  sort_vars = StringEnv.empty;
  rels = StringEnv.empty;
  tycons_vars = StringEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
  definitions = StringEnv.empty;

  tycons_sort = TyConsVar.Env.empty;
  prelude_typevar_sort = TyVar.Env.empty;

  varsorts = Var.Env.empty;
  covarsorts = CoVar.Env.empty;
  tyvarsorts = TyVar.Env.empty;
(* INVARIANT: if a value in 'unifier' as a Loc node, then it is at the root.
   This implies it must be the only one appearing in the value *)
  unifier = USortVar.Env.empty
}

type scope = {
  vars : Var.t StringEnv.t;
  covars : CoVar.t StringEnv.t;
  tyvars : TyVar.t StringEnv.t;
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

let get_covar scope a =
  try StringEnv.find a scope.covars
  with _ -> raise (Failure a)

let get_tyvar scope t = StringEnv.find t scope.tyvars


let empty_scope = {
  vars = StringEnv.empty;
  covars = StringEnv.empty;
  tyvars = StringEnv.empty;
}


let rec intern_sort env = function
  | Base p -> Base p
  | Arrow (s,t) -> Arrow (intern_sort env s, intern_sort env t)
  | Index i ->
    try Index (StringEnv.find i env.sort_vars) with
    | Not_found -> fail_undefined_sort i

let rec unintern_sort = function
  | Base p -> Base p
  | Index i -> Index (SortVar.to_string i)
  | Arrow (s,t) -> Arrow (unintern_sort s, unintern_sort t)

let rec intern_type env scope = function

  | TVar {node; loc} ->
    begin
      try TCons {node = Cons (StringEnv.find node env.tycons_vars); loc}
      with
        Not_found ->
        try TVar {node = get_tyvar scope node; loc}
        with
          Not_found -> fail_undefined_type node loc
    end

  | TCons {node; loc} ->
      Types.cons ~loc (match node with
        | Cons cons ->
          let name =
            try StringEnv.find cons env.tycons_vars
            with _ -> fail_undefined_type cons loc in
          Cons name
        | Unit -> Unit
        | Int -> Int
        | Bool -> Bool
        | Zero -> Zero
        | Top -> Top
        | Bottom -> Bottom
        | Prod n -> Prod n
        | Sum n -> Sum n
        | Choice n -> Choice n
        | Fun n -> Fun n
        | Thunk -> Thunk
        | Closure -> Closure)

  | TInternal var -> intern_type env scope (TVar {node = var; loc = dummy_pos})

  | TApp {tfun; args; loc} ->
    if args = [] then
      intern_type env scope tfun
    else
      TApp {tfun = intern_type env scope tfun;
            args = List.map (intern_type env scope) args;
            loc}

  | TPos t -> intern_type env scope t

  | TNeg t -> intern_type env scope t

  | TBox box -> TBox {box with node=intern_type env scope box.node}

  | TFix t -> TFix (intern_type env scope t)
