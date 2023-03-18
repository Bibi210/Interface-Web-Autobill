open AstML

let list_sprintf list fmt_func separator =
  String.concat separator (List.map fmt_func list)
;;

let array_sprintf array fmt_func separator =
  list_sprintf (List.of_seq (Array.to_seq array)) fmt_func separator
;;

let rec fmt_variable x = Printf.sprintf "(VarName = %s)" x.basic_ident
and fmt_prog p = list_sprintf p fmt_prog_node " ;;\n****************\n\n"

and fmt_prog_node = function
  | Def def -> Printf.sprintf "Def(%s)" (fmt_def def)
  | Expr expr -> Printf.sprintf "Expr(%s)" (fmt_expr expr)

and fmt_def d =
  match d.dnode with
  | VariableDef { var; init } ->
    Printf.sprintf "%s = %s" (fmt_variable var) (fmt_expr init)
  | TypeDef { basic_ident; parameters; constructors } ->
    Printf.sprintf
      "NewType %s Parameters=[%s] Constructors(\n | %s)"
      basic_ident
      (String.concat " " parameters)
      (fmt_newconstructor_case_ls constructors)

and fmt_newconstructor_case case =
  Printf.sprintf
    "ConstructorCase(%s of %s)\n"
    case.constructor_ident
    (fmt_type_ls case.c_of)

and fmt_litteral = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
  | Unit -> "Unit"

and fmt_unary_op = function
  | Autobill.Lcbpv.Not -> Printf.sprintf "!"
  | Autobill.Lcbpv.Opp -> Printf.sprintf "opp"

and fmt_binary_op = function
  | Autobill.Lcbpv.Add -> "+"
  | Autobill.Lcbpv.Mult -> "*"
  | Autobill.Lcbpv.Subs -> "-"
  | Autobill.Lcbpv.Div -> "/"
  | Autobill.Lcbpv.Mod -> "%"
  | Autobill.Lcbpv.And -> "and"
  | Autobill.Lcbpv.Or -> "or"
  | Autobill.Lcbpv.Int_Eq -> "=="
  | Autobill.Lcbpv.Int_Leq -> "<="
  | Autobill.Lcbpv.Int_Lt -> "<"

and fmt_expr exp =
  match exp.enode with
  | Litteral litteral -> fmt_litteral litteral
  | Variable variable -> Printf.sprintf "Variable %s" (fmt_variable variable)
  | Call { func; arg } -> Printf.sprintf "Apply(%s on %s)" (fmt_expr func) (fmt_expr arg)
  | CallUnary { op; arg = Some arg } ->
    Printf.sprintf "(%s%s)" (fmt_unary_op op) (fmt_expr arg)
  | CallUnary { op; arg = None } -> Printf.sprintf "(%s)" (fmt_unary_op op)
  | CallBinary { op; args } ->
    Printf.sprintf "(%s)" (list_sprintf args fmt_expr (fmt_binary_op op))
  | Sequence expr_ls -> Printf.sprintf "\nSequence(\n  %s)" (fmt_expr_ls expr_ls)
  | Binding { var; init; content } ->
    Printf.sprintf
      "Binding(%s = (%s) in\n (%s))"
      (fmt_variable var)
      (fmt_expr init)
      (fmt_expr content)
  | Lambda { arg; body } ->
    Printf.sprintf "Lambda %s\n -> (%s)" (fmt_variable arg) (fmt_expr body)
  | Tuple expr_ls -> Printf.sprintf "\nTuple(\n  %s )" (fmt_expr_ls expr_ls)
  | Construct { constructor_ident; to_group } ->
    Printf.sprintf "%s(%s)" constructor_ident (fmt_expr_ls to_group)
  | FunctionRec { var; arg; body } ->
    Printf.sprintf
      "RecFunc %s( %s -> (%s))"
      var.basic_ident
      arg.basic_ident
      (fmt_expr body)
  | Match { to_match; cases } ->
    Printf.sprintf "Match (%s) with (%s)" (fmt_expr to_match) (fmt_match_case_ls cases)

and fmt_match_case case =
  Printf.sprintf
    "MatchCase (%s -> %s) "
    (fmt_pattern case.pattern)
    (fmt_expr case.consequence)

and fmt_pattern ptt =
  match ptt.pnode with
  | LitteralPattern litteral -> fmt_litteral litteral
  | VarPattern string -> string
  | WildcardPattern -> "_"
  | TuplePattern pattern_ls ->
    Printf.sprintf "TuplePattern(%s)" (fmt_pattern_ls pattern_ls)
  | ConstructorPattern { constructor_ident; content } ->
    Printf.sprintf "%s(%s)" constructor_ident (fmt_pattern_ls content)

and fmt_type_opt = function
  | None -> "None"
  | Some x -> fmt_type x

and fmt_type t =
  match t.etype with
  | TypeInt -> "Int_t"
  | TypeBool -> "Bool_t"
  | TypeUnit -> "Unit_t"
  | TypeTuple type_ls -> Printf.sprintf "Tuple_t of (%s)" (fmt_type_ls type_ls)
  | TypeLambda { arg; return_type } ->
    Printf.sprintf "Function_t [%s] -> (%s)" (fmt_type arg) (fmt_type return_type)
  | TypeVar vartype -> Printf.sprintf "%s_t" vartype
  | TypeConstructor construct ->
    Printf.sprintf
      "(ParametredType %s with %s)"
      (fmt_type construct.to_build)
      (fmt_type_ls construct.parameters)

and fmt_match_case_ls ls = list_sprintf ls fmt_match_case "\n | "
and fmt_variable_ls x = list_sprintf x fmt_variable " , "
and fmt_newconstructor_case_ls ls = list_sprintf ls fmt_newconstructor_case " | "
and fmt_pattern_ls ls = list_sprintf ls fmt_pattern " , "
and fmt_type_ls x = list_sprintf x fmt_type " , "
and fmt_expr_ls x = list_sprintf x fmt_expr " \n, "
