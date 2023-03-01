open Ast

let list_sprintf list fmt_func separator =
  String.concat separator (List.map fmt_func list)
;;

let array_sprintf array fmt_func separator =
  list_sprintf (List.of_seq (Array.to_seq array)) fmt_func separator
;;

let rec fmt_variable x = Printf.sprintf "(VarName = %s)" x.basic_ident
and fmt_prog p = list_sprintf p fmt_prog_node " ;;\n"

and fmt_prog_node = function
  | Def def -> Printf.sprintf "Def(%s)" (fmt_def def)
  | Expr expr -> Printf.sprintf "Expr(%s)" (fmt_expr expr)

and fmt_def d =
  match d.dnode with
  | VariableDef { var; init } ->
    Printf.sprintf "%s = %s" (fmt_variable var) (fmt_expr init)
  | FunctionRecDef { var; args; body } ->
    Printf.sprintf
      "RecFunc %s [%s] -> %s"
      var.basic_ident
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

and fmt_callable = function
  | ApplyExpr e -> fmt_expr e
  | Add -> "%_+"
  | Sub -> "%_-"
  | Mult -> "%_*"
  | Div -> "%_/"
  | Modulo -> "%_%"
  | Or -> "%_||"
  | BitAnd -> "%_&"
  | And -> "%_&&"

and fmt_expr exp =
  match exp.enode with
  | Litteral litteral -> fmt_litteral litteral
  | Variable variable -> Printf.sprintf "Variable %s" (fmt_variable variable)
  | Call { func; args } ->
    Printf.sprintf "Apply(%s with %s)" (fmt_callable func) (fmt_expr_ls args)
  | Sequence expr_ls -> Printf.sprintf "\nSequence(\n  %s)" (fmt_expr_ls expr_ls)
  | Binding { var; init; content } ->
    Printf.sprintf
      "Binding(%s = (%s) in\n (%s))"
      (fmt_variable var)
      (fmt_expr init)
      (fmt_expr content)
  | Lambda { args; body } ->
    Printf.sprintf "Lambda %s\n -> (%s)" (fmt_variable_ls args) (fmt_expr body)
  | Tuple expr_ls -> Printf.sprintf "\nTuple(\n  %s )" (fmt_expr_ls expr_ls)
  | Construct { constructor_ident; to_group } ->
    Printf.sprintf "%s(%s)" constructor_ident (fmt_expr to_group)
  | FunctionRec { var; args; body } ->
    Printf.sprintf
      "RecFunc %s( [%s] -> (%s))"
      var.basic_ident
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
  | TuplePattern pattern_ls ->
    Printf.sprintf "TuplePattern(%s)" (fmt_pattern_ls pattern_ls)
  | ConstructorPattern { constructor_ident; content } ->
    Printf.sprintf "%s(%s)" constructor_ident (fmt_pattern content)

and fmt_type_opt = function
  | None -> "None"
  | Some x -> fmt_type x

and fmt_type t =
  match t.etype with
  | TypeInt -> "Int_t"
  | TypeBool -> "Bool_t"
  | TypeUnit -> "Unit_t"
  | TypeTuple type_ls -> Printf.sprintf "Tuple_t of (%s)" (fmt_type_ls type_ls)
  | TypeLambda { args; return_type } ->
    Printf.sprintf "Function_t [%s] -> (%s)" (fmt_type_ls args) (fmt_type return_type)
  | TypeVar vartype -> Printf.sprintf "%s_t" vartype
  | TypeConstructor construct ->
    Printf.sprintf "(ParametredType %s)" (fmt_type_ls construct)

and fmt_match_case_ls ls = list_sprintf ls fmt_match_case " | "
and fmt_variable_ls x = list_sprintf x fmt_variable " , "
and fmt_newconstructor_case_ls ls = list_sprintf ls fmt_newconstructor_case " | "
and fmt_pattern_ls ls = list_sprintf ls fmt_pattern " , "
and fmt_type_ls x = list_sprintf x fmt_type " , "
and fmt_expr_ls x = list_sprintf x fmt_expr " \n, "
