open Primitives
open Types
open Cst
open Vars
open Misc

let declare_sort s =
  Sort_declaration {name = SortVar.to_string s; loc = dummy_pos}

let declare_tycons name args ret =
  Type_declaration {
    name = TyConsVar.to_string name;
    sort = sort_arrow args ret;
    loc = dummy_pos
  }

let declare_rel name args =
  Rel_declaration {
    name = RelVar.to_string name;
    args;
    loc = dummy_pos
  }

let declare_value name typ =
  Term_declaration {
    name = Var.to_string name;
    typ = typ;
    loc = dummy_pos
  }

let sort_nat' = sort_idx (SortVar.to_string sort_nat)
let declare_int_binop op =
  declare_value op (func [prim_type_int; prim_type_int] (thunk_t prim_type_int))
let declare_int_monop op =
  declare_value op (func [prim_type_int] (thunk_t prim_type_int))
let declare_int_binpred op =
  declare_value op (func [prim_type_int] (thunk_t prim_type_bool))
let declare_bool_binop op =
  declare_value op (func [prim_type_bool; prim_type_bool] (thunk_t prim_type_bool))
let declare_bool_monop op =
  declare_value op (func [prim_type_bool] (thunk_t prim_type_bool))

let primitives_prelude = [

  declare_sort sort_nat;
  declare_tycons nat_zero [] sort_nat';
  declare_tycons nat_one [] sort_nat';
  declare_tycons nat_add [sort_nat'; sort_nat'] sort_nat';
  (* declare_rel nat_leq [sort_nat'; sort_nat']; *)

  declare_tycons tycons_int [] sort_postype;
  declare_int_binop op_add;
  declare_int_binop op_mul;
  declare_int_binop op_sub;
  declare_int_binop op_div;
  declare_int_binop op_mod;
  declare_bool_monop op_op;
  declare_int_binpred op_eq;
  declare_int_binpred op_leq;
  declare_int_binpred op_lt;

  declare_tycons tycons_bool [] sort_postype;
  declare_bool_binop op_and;
  declare_bool_binop op_or;
  declare_bool_monop op_not

]

let with_primitives prog = primitives_prelude @ prog
