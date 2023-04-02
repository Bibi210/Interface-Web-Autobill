open Vars

(* sorts tycons rel vals *)

let sort_nat = SortVar.of_primitive "nat"

let nat_zero = TyConsVar.of_primitive "Z"

let nat_one = TyConsVar.of_primitive "One"

let nat_large = TyConsVar.of_primitive "Large"

let nat_add = TyConsVar.of_primitive "Add"

let nat_mult = TyConsVar.of_primitive "Mult"

let nat_leq = RelVar.of_primitive "leq"

let tycons_int = TyConsVar.of_primitive "Int"

let op_add = Var.of_primitive "add"

let op_sub = Var.of_primitive "sub"

let op_mul = Var.of_primitive "mul"

let op_div = Var.of_primitive "div"

let op_mod = Var.of_primitive "mod"

let op_op = Var.of_primitive "op"

let op_eq = Var.of_primitive "eq"

let op_leq = Var.of_primitive "leq"

let op_lt = Var.of_primitive "lt"


let tycons_bool = TyConsVar.of_primitive "Bool"

let op_and = Var.of_primitive "and"

let op_or = Var.of_primitive "or"

let op_not = Var.of_primitive "not"

