open Ast

let list_sprintf list fmt_func separator =
  String.concat separator (List.map fmt_func list)
;;

let array_sprintf array fmt_func separator =
  list_sprintf (List.of_seq (Array.to_seq array)) fmt_func separator
;;

let rec fmt_variable x =
  Printf.sprintf "(VarName = %s ; Type = %s)" x.basic_ident (fmt_type x.expected_type)

and fmt_prog p = list_sprintf p fmt_prog_node " ;;\n"

and fmt_prog_node = function
  | Def def -> Printf.sprintf "Def(%s)" (fmt_def def)
  | Expr expr -> Printf.sprintf "Expr(%s)" (fmt_expr expr)

and fmt_def d =
  match d.dnode with
  | VariableDef { var; init } ->
    Printf.sprintf "%s = %s" (fmt_variable var) (fmt_expr init)
  | FunctionDef { basic_ident; args; body } ->
    Printf.sprintf "Func %s [%s] -> %s" basic_ident (fmt_variable_ls args) (fmt_expr body)
  | FunctionRecDef { basic_ident; args; body } ->
    Printf.sprintf
      "RecFunc %s [%s] -> %s"
      basic_ident
      (fmt_variable_ls args)
      (fmt_expr body)
  | TypeDef { basic_ident; parameters; constructors } ->
    Printf.sprintf
      "NewType %s Parameters=[%s] Constructors(\n | %s)"
      basic_ident
      (String.concat " " parameters)
      (fmt_newconstructor_case_ls constructors)

and fmt_newconstructor_case case =
  Printf.sprintf "ConstructorCase(%s of %s)\n" case.constructor_ident (fmt_type case.c_of)

and fmt_litteral = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
  | Unit -> "Unit"

and fmt_expr exp =
  match exp.enode with
  | Litteral litteral -> fmt_litteral litteral
  | Variable variable -> Printf.sprintf "Variable %s is %s" variable (fmt_type exp.etype)
  | Call { func; arg } ->
    Printf.sprintf "Apply(\n%s with\n (%s))" (fmt_expr func) (fmt_expr arg)
  | Sequence expr_ls -> Printf.sprintf "Seq(%s)" (list_sprintf expr_ls fmt_expr ";")
  | Binding { var; init; content } ->
    Printf.sprintf
      "Binding(%s = (%s) in (%s))"
      (fmt_variable var)
      (fmt_expr init)
      (fmt_expr content)
  | Lambda { arg; body } ->
    Printf.sprintf "Lambda %s -> (%s)" (fmt_variable arg) (fmt_expr body)
  | Tuple { first; second } ->
    Printf.sprintf "Tuple(%s,%s)" (fmt_expr first) (fmt_expr second)
  | Construct { constructor_name; to_group } ->
    Printf.sprintf "%s(%s)" constructor_name (fmt_expr to_group)
  | FunctionRec { basic_ident; args; body } ->
    Printf.sprintf
      "RecFunc %s( [%s] -> (%s))"
      basic_ident
      (fmt_variable_ls args)
      (fmt_expr body)
  | Function { basic_ident; args; body } ->
    Printf.sprintf
      "Func %s( [%s] -> (%s))"
      basic_ident
      (fmt_variable_ls args)
      (fmt_expr body)
  | Match { to_match; cases } ->
    Printf.sprintf "Match (%s) with (%s)" (fmt_expr to_match) (fmt_match_case_ls cases)

and fmt_match_case case =
  Printf.sprintf
    "MatchCase (%s -> %s) "
    (fmt_pattern case.pattern)
    (fmt_expr case.consequence)

and fmt_pattern = function
  | LitteralPattern litteral -> fmt_litteral litteral
  | VarPattern string -> string
  | WildcardPattern -> "_"
  | ConstructorPattern { constructor_ident : string; content : pattern list } ->
    Printf.sprintf "%s(%s)" constructor_ident (fmt_pattern_ls content)

and fmt_type = function
  | TypeInt -> "Int_t"
  | TypeBool -> "Bool_t"
  | TypeUnit -> "Unit_t"
  | TypeTuple { first; second } ->
    Printf.sprintf "Tuple_t of (%s, %s)" (fmt_type first) (fmt_type second)
  | TypeLambda { arg; return_type } ->
    Printf.sprintf "Function_t [%s] -> (%s)" (fmt_type arg) (fmt_type return_type)
  | TypeVar vartype -> Printf.sprintf "%s_t" vartype
  | TypeConstructor construct ->
    Printf.sprintf "(ParametredType %s)" (fmt_type_ls construct)

and fmt_match_case_ls ls = list_sprintf ls fmt_match_case " | "
and fmt_variable_ls x = list_sprintf x fmt_variable " "
and fmt_newconstructor_case_ls ls = list_sprintf ls fmt_newconstructor_case " | "
and fmt_pattern_ls ls = list_sprintf ls fmt_pattern " "
and fmt_type_ls x = list_sprintf x fmt_type " "
