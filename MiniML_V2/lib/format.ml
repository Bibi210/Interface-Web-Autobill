open Ast

let list_sprintf list fmt_func separator =
  String.concat separator (List.map fmt_func list)
;;

let array_sprintf array fmt_func separator =
  list_sprintf (List.of_seq (Array.to_seq array)) fmt_func separator
;;

let fmt_const = function
  | Integer i -> Printf.sprintf "Int(%d)" i
  | Boolean b -> Printf.sprintf "Bool(%s)" (string_of_bool b)
  | Unit -> "Unit"
;;

let rec fmt_pattern = function
  | LitteralPattern c -> fmt_const c
  | VarPattern str -> str
  | WildcardPattern -> "_"
  | ConstructorPattern construct ->
    Printf.sprintf
      "%s(%s)"
      construct.constructor_name
      (list_sprintf construct.content fmt_pattern ",")
;;

let rec fmt_type = function
  | TypeInt -> "Int_t"
  | TypeBool -> "Bool_t"
  | TypeUnit -> "Unit_t"
  | TypeTuple { first; second } ->
    Printf.sprintf "Tuple_t of (%s, %s)" (fmt_type first) (fmt_type second)
  | TypeLambda { args_types; return_type } ->
    Printf.sprintf
      "Function [%s] -> %s"
      (list_sprintf args_types fmt_type " ")
      (fmt_type return_type)
  | TypeCustom name -> Printf.sprintf "User_%s_t" name
;;

let rec fmt_expr e =
  Printf.sprintf
    "Expr(Node:%s Type:%s Pos:%s)"
    (match e.node with
    | Litteral a -> fmt_const a
    | Tuple tpl -> Printf.sprintf "Tuple(%s,%s)" (fmt_expr tpl.first) (fmt_expr tpl.second)
    | Binding bind ->
      Printf.sprintf
        "Binding(%s = %s in %s)"
        bind.ident
        (fmt_expr bind.init)
        (fmt_expr bind.content)
    | Variable var -> Printf.sprintf "Var(%s)" var
    | Sequence content -> Printf.sprintf "Seq(%s)" (list_sprintf content fmt_expr ";")
    | Lambda func ->
      Printf.sprintf
        "Lambda %s -> (%s)"
        (list_sprintf func.args (fun a -> a) " ")
        (fmt_expr func.body)
    | Call funcall ->
      Printf.sprintf
        "Apply %s with (%s)"
        (fmt_expr funcall.func)
        (list_sprintf funcall.args fmt_expr " ")
    | Construct construct ->
      Printf.sprintf
        "%s(%s)"
        construct.constructor_name
        (list_sprintf construct.to_group fmt_expr ";")
    | Match m ->
      Printf.sprintf
        "Match(%s,Cases(%s))"
        (fmt_expr m.to_match)
        (list_sprintf m.cases fmt_case "\n"))
    (match e.etype with
    | None -> "ToInfer"
    | Some t -> fmt_type t)
    (Helpers.string_of_position e.loc)

and fmt_case case =
  Printf.sprintf
    "| Case(Pattern = %s , Consequence = %s)"
    (fmt_pattern case.pattern)
    (fmt_expr case.consequence)
;;

let fmt_def_constructor new_constructor =
  Printf.sprintf
    "%s of (%s)"
    new_constructor.name
    (list_sprintf new_constructor.etype fmt_type " *")
;;

let fmt_def = function
  | TypeDef new_type ->
    Printf.sprintf
      "TypeDef(%s = (%s))"
      new_type.name
      (list_sprintf new_type.constructors fmt_def_constructor "\n")
  | VariableDef new_var ->
    Printf.sprintf "VariableDef(%s = %s)" new_var.ident (fmt_expr new_var.init)
;;

let fmt_prog_node = function
  | Def x -> fmt_def x
  | Expr e -> fmt_expr e
;;

let fmt_prog p = list_sprintf p fmt_prog_node " ;;"
