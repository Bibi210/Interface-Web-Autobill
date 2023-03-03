open Format
open Lcbpv

let pp_var = pp_print_string

let pp_with_string str = pp_print_list ~pp_sep:(fun fmt () ->fprintf fmt str)

let pp_with_space pp fmt l = pp_with_string " " pp fmt l

let pp_with_comma pp fmt l = pp_with_string ", " pp fmt l


let pp_sort fmt so =
  pp_print_string fmt (match so with Pos -> "+" | Neg -> "-")

let pp_qual fmt q = pp_print_string fmt (match q with
    |Exp -> "exp"
    | Aff -> "aff"
    | Lin -> "closure")

let rec pp_typ fmt = function
  | Typ_Var v -> pp_var fmt v
  | Typ_Unit -> pp_print_string fmt "Unit"
  | Typ_Zero -> pp_print_string fmt "Zero"
  | Typ_Top -> pp_print_string fmt "Top"
  | Typ_Int -> pp_print_string fmt "Int"
  | Typ_Bool -> pp_print_string fmt "Bool"
  | Typ_Bottom -> pp_print_string fmt "Bottom"
  | Typ_Tuple -> fprintf fmt "Tuple"
  | Typ_Sum -> fprintf fmt "Sum"
  | Typ_Fun -> fprintf fmt "Fun"
  | Typ_LazyPair -> fprintf fmt "Choice"
  | Typ_Closure Lin -> fprintf fmt "Closure"
  | Typ_Closure Exp -> fprintf fmt "Exp"
  | Typ_Closure Aff -> fprintf fmt "Aff"
  | Typ_Thunk -> fprintf fmt "Thunk"
  | Typ_App (Typ_Fun, ret_typ :: args_typs) ->
    fprintf fmt "Fun(%a) -> %a" (pp_with_comma pp_typ) args_typs pp_typ ret_typ
  | Typ_App (c, args) ->
    fprintf fmt "%a(%a)" pp_typ c (pp_with_comma pp_typ) args


let pp_constructor fmt = function
  | Cons_Named c -> pp_var fmt c
  | Unit -> pp_print_string fmt "unit"
  | Tuple -> fprintf fmt "tuple"
  | Inj (i,n) -> fprintf fmt "inj{%d,%d}" i n
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | Int_Litt n -> pp_print_int fmt n

let pp_method fmt = function
  | Method_Named m -> pp_var fmt m
  | Call -> fprintf fmt "call"
  | Proj (i,n) -> fprintf fmt "proj{%d,%d}" i n

let pp_mon_prim fmt op = pp_print_string fmt (match op with
    | Opp -> "-"
    | Not -> "!")

let pp_bin_prim fmt op = pp_print_string fmt (match op with
    | Add -> "+"
    | Mult -> "*"
    | Subs -> "-"
    | Div -> "/"
    | Mod -> "%"
    | And -> "&&"
    | Or -> "||"
    | Int_Eq -> "=="
    | Int_Lt -> "<"
    | Int_Leq -> "<=")

let rec pp_val fmt = function
  | Val_Var v -> pp_var fmt v
  | Val_Int n -> pp_print_int fmt n
  | Val_Constructor (c, []) -> pp_constructor fmt c
  | Val_Constructor (c, args) -> fprintf fmt "%a(%a)" pp_constructor c (pp_with_comma pp_val) args
  | Val_Closure (q, arg) -> fprintf fmt "%a(%a)" pp_qual q pp_expr arg
  | Val_Thunk arg -> fprintf fmt "thunk(%a)" pp_expr arg
  | Val_Get methods ->
    fprintf fmt "get {%a}" (pp_print_list ~pp_sep:pp_print_cut pp_get_pattern) methods

and pp_expr fmt = function
  | Expr_Int n -> pp_print_int fmt n
  | Expr_Var v -> pp_var fmt v
  | Expr_Constructor (c, args) ->
    fprintf fmt "%a(%a)" pp_constructor c (pp_with_comma pp_expr) args
  | Expr_Closure (q, arg) -> fprintf fmt "%a(%a)" pp_qual q pp_expr arg
  | Expr_Thunk arg -> fprintf fmt "thunk(%a)" pp_expr arg
  | Expr_Get methods ->
    fprintf fmt "@[<v 0>get@ %a@ end@]"
      (pp_print_list ~pp_sep:pp_print_cut  pp_get_pattern) methods
  | Expr_Block b -> pp_block_expr fmt b
  | Expr_Method (v,m,args) ->
    fprintf fmt "(%a).%a(%a)" pp_expr v pp_method m (pp_with_comma pp_expr) args
  | Expr_Match (scrut, patts) ->
    fprintf fmt "@[<v 0>match %a with@ %a@ end@]"
      pp_expr scrut
      (pp_print_list ~pp_sep:pp_print_cut  pp_match_pattern) patts
  | Expr_Bin_Prim (op, a, b) ->
    fprintf fmt "%a %a %a" pp_expr a pp_bin_prim op pp_expr b
  | Expr_Mon_Prim (op, a) ->
    fprintf fmt "%a%a" pp_mon_prim op pp_expr a
  | Expr_If (b, x, y) ->
    fprintf fmt "if %a then %a else %a" pp_expr b pp_expr x pp_expr y
  | Expr_Rec (x, e) ->
    fprintf fmt "rec %a is %a" pp_var x pp_expr e


and pp_match_pattern fmt (MatchPat (c, binds, e)) =
  fprintf fmt "@[| %a(%a) -> %a@]" pp_constructor c (pp_with_comma pp_var) binds pp_expr e

and pp_get_pattern fmt (GetPat (m, binds, e)) =
  fprintf fmt "@[| %a(%a) -> %a@]" pp_method m (pp_with_comma pp_var) binds pp_expr e

and pp_block_expr fmt blk = fprintf fmt "@[<v 2>{@ %a@ }@]" pp_block blk

and pp_block fmt (Blk (instrs, ret)) =
  let pp_with_semicol fmt () = fprintf fmt ";@ " in
  if instrs = [] then
    fprintf fmt "return %a" pp_expr ret
  else
    fprintf fmt "%a;@ return %a"
      (pp_print_list ~pp_sep:pp_with_semicol pp_instr) instrs
      pp_expr ret

and pp_instr fmt = function
  | Ins_Let (v, e) -> fprintf fmt "@[let %a = %a@]" pp_var v pp_expr e
  | Ins_Force (v, e) -> fprintf fmt "@[force thunk(%a) = %a@]" pp_var v pp_expr e
  | Ins_Open (v, q, e) -> fprintf fmt "@[open %a(%a)= %a@]" pp_qual q pp_var v pp_expr e


let pp_typdef_args = pp_with_space
    (fun fmt (arg, so) -> fprintf fmt "(%a : %a)" pp_var arg pp_sort so)

let pp_prog_items fmt = function
  | Typ_Def (x, args, Def_Synonym (t, sort)) ->
    fprintf fmt "type %a %a : %a = %a;"
    pp_var x
    pp_typdef_args args
    pp_sort sort
    pp_typ t
  | Typ_Def (x, args, Def_Datatype (conses)) ->
    let pp_consdef fmt (cons, args) =
      fprintf fmt "| %a(%a)" pp_var cons (pp_with_comma pp_typ) args in
    fprintf fmt "data %a %a =@ %a;"
      pp_var x
      pp_typdef_args args
      (pp_print_list ~pp_sep:pp_print_cut  pp_consdef) conses
  | Typ_Def (x, args, Def_Computation (methods)) ->
    let pp_consdef fmt (met, args, ret) =
      fprintf fmt "| %a(%a) -> %a" pp_var met (pp_with_comma pp_typ) args pp_typ ret in
    fprintf fmt "comput %a %a =@ %a;"
      pp_var x
      pp_typdef_args args
      (pp_print_list ~pp_sep:pp_print_cut pp_consdef) methods
  | Typ_Decl (t, sorts, rets) ->
    let pp_args fmt = function
      | [] -> ()
      | args -> fprintf fmt "(%a) -> " (pp_with_comma pp_sort) args in
    fprintf fmt "decl type %a : %a%a;"
      pp_var t
      pp_args sorts
      pp_sort rets
  | Value_Decl (x, t) ->
    fprintf fmt "decl %a : %a;" pp_var x pp_typ t
  | Do b -> fprintf fmt "@[<v 0>%a@]" pp_block b

let pp_program fmt (Prog blk) =
  pp_open_vbox fmt 0;
  pp_print_list ~pp_sep:pp_print_cut pp_prog_items fmt blk;
  pp_print_cut fmt ();
  pp_close_box fmt ()
