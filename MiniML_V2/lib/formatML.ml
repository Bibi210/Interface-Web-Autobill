(* Should make a pp_printer ? *)
open AstML
open Format

let fmt_string = pp_print_string
let fmt_variable fmt { basic_ident; _ } = fmt_string fmt basic_ident
let fmt_with_string str = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt str)
let fmt_with_space pp fmt l = fmt_with_string " " pp fmt l
let fmt_with_comma pp fmt l = fmt_with_string ", " pp fmt l
let fmt_with_semicolon pp fmt l = fmt_with_string "; " pp fmt l
let fmt_with_mult pp fmt l = fmt_with_string "* " pp fmt l

let rec fmt_type fmt t =
  let fmt_string = fmt_string fmt in
  match t.etype with
  | TypeInt -> fmt_string "int"
  | TypeBool -> fmt_string "bool"
  | TypeUnit -> fmt_string "()"
  | TypeTuple type_ls -> fprintf fmt "(%a)" (fmt_with_mult fmt_type) type_ls
  | TypeLambda { arg; return_type } ->
    fprintf fmt "(%a -> %a)" fmt_type arg fmt_type return_type
  | TypeVar vartype -> fmt_string ("'" ^ vartype)
  | TypeDefined t -> fmt_string t
  | TypeConstructor construct ->
    fprintf
      fmt
      "(%a %a)"
      (fmt_with_space fmt_type)
      construct.parameters
      fmt_type
      construct.to_build
;;

let fmt_unary_op fmt op =
  pp_print_string
    fmt
    (match op with
    | Autobill.Lcbpv.Not -> "!"
    | Autobill.Lcbpv.Opp -> "-")
;;

let fmt_binary_op fmt op =
  pp_print_string
    fmt
    (match op with
    | Autobill.Lcbpv.Add -> "+"
    | Autobill.Lcbpv.Mult -> "*"
    | Autobill.Lcbpv.Subs -> "-"
    | Autobill.Lcbpv.Div -> "/"
    | Autobill.Lcbpv.Mod -> "%"
    | Autobill.Lcbpv.And -> "and"
    | Autobill.Lcbpv.Or -> "or"
    | Autobill.Lcbpv.Int_Eq -> "=="
    | Autobill.Lcbpv.Int_Leq -> "<="
    | Autobill.Lcbpv.Int_Lt -> "<")
;;

let fmt_litteral fmt = function
  | Integer i -> pp_print_int fmt i
  | Boolean b -> pp_print_bool fmt b
  | Unit -> pp_print_string fmt "()"
;;

let rec fmt_pattern fmt ptt =
  match ptt.pnode with
  | LitteralPattern litteral -> fmt_litteral fmt litteral
  | VarPattern string -> pp_print_string fmt string
  | WildcardPattern -> pp_print_char fmt '_'
  | TuplePattern pattern_ls -> fprintf fmt "(%a)" (fmt_with_comma fmt_pattern) pattern_ls
  | ConstructorPattern { constructor_ident; content } ->
    fprintf
      fmt
      "(%a(%a))"
      fmt_string
      constructor_ident
      (fmt_with_comma fmt_pattern)
      content
;;

let rec fmt_expr fmt exp =
  match exp.enode with
  | Litteral litteral -> fmt_litteral fmt litteral
  | Variable variable -> fmt_variable fmt variable
  | Call { func; arg } -> fprintf fmt "(%a %a)" fmt_expr func fmt_expr arg
  | CallUnary { op; arg = Some arg } -> fprintf fmt "(%a%a)" fmt_unary_op op fmt_expr arg
  | CallUnary { op; arg = None } -> fprintf fmt "(%a)" fmt_unary_op op
  | CallBinary { op; args = first :: second :: _ } ->
    fprintf fmt "(%a %a %a)" fmt_expr first fmt_binary_op op fmt_expr second
  | CallBinary { op; args = [] } -> fprintf fmt "(%a)" fmt_binary_op op
  | CallBinary { op; args = [ arg ] } ->
    fprintf fmt "(%a %a)" fmt_binary_op op fmt_expr arg
  | Sequence expr_ls -> fprintf fmt "(%a)" (fmt_with_semicolon fmt_expr) expr_ls
  | Binding { var; init; content } ->
    fprintf
      fmt
      "@[let %a = @[%a@] in @[%a@] @]"
      fmt_variable
      var
      fmt_expr
      init
      fmt_expr
      content
  | Lambda { arg; body } -> fprintf fmt "fun %a -> @[%a@]" fmt_variable arg fmt_expr body
  | Tuple expr_ls -> fprintf fmt "(%a)" (fmt_with_comma fmt_expr) expr_ls
  | Construct { constructor_ident; to_group } ->
    fprintf fmt "(%a(%a))" fmt_string constructor_ident (fmt_with_comma fmt_expr) to_group
  | FunctionRec { var; arg; body } ->
    fprintf
      fmt
      "@[let rec %a = (fun %a -> @[%a@]) in @[%a@] @]"
      fmt_variable
      var
      fmt_variable
      arg
      fmt_expr
      body
      fmt_variable
      arg
  | Match { to_match; cases } ->
    fprintf
      fmt
      "(match %a with @[%a@])"
      fmt_expr
      to_match
      (pp_print_list ~pp_sep:pp_print_cut fmt_case)
      cases

and fmt_case fmt case =
  fprintf fmt "@[| %a -> %a @]" fmt_pattern case.pattern fmt_expr case.consequence
;;

let fmt_construtors fmt newConstr =
  fprintf
    fmt
    "@[| %a of %a @]"
    fmt_string
    newConstr.constructor_ident
    (fmt_with_mult fmt_type)
    newConstr.c_of
;;

let fmt_def fmt d =
  match d.dnode with
  | VariableDef { var; init } -> fprintf fmt "let %a = %a" fmt_variable var fmt_expr init
  | TypeDef { basic_ident; parameters; constructors } ->
    fprintf
      fmt
      "type %a %a = @ %a"
      (fmt_with_space fmt_string)
      (List.map (fun x -> "'" ^ x) parameters)
      fmt_string
      basic_ident
      (pp_print_list ~pp_sep:pp_print_cut fmt_construtors)
      constructors
;;

let fmt_prog_node fmt = function
  | Def def -> fmt_def fmt def
  | Expr expr -> fmt_expr fmt expr
;;

let fmt_with_double_semicolon pp fmt l = fmt_with_string ";;@ " pp fmt l

let fmp_prog fmt prog =
  pp_open_vbox fmt 0;
  fmt_with_double_semicolon fmt_prog_node fmt prog;
  pp_print_cut fmt ();
  pp_close_box fmt ()
;;
