open Vars
open Types
open Constructors
open Ast
open Format
open Prelude
open FirstOrder.FullFOL

let pp_comma_sep fmt () =
  fprintf fmt ",@ "

let pp_sort = Types.pp_sort SortVar.to_string

let pp_typ = Types.pp_typ TyConsVar.pp TyVar.pp

let pp_custom_binding ~prefix ~suffix pp_v pp_t fmt (v, t) =
  fprintf fmt "@[<hov 2>%s%a@ : %a%s@]" prefix pp_v v pp_t t suffix

let pp_bind_def =
  pp_custom_binding ~prefix:"" ~suffix:"" Var.pp pp_typ

let pp_bind_def_with_cont =
  pp_custom_binding ~prefix:"" ~suffix:"" Var.pp pp_typ

let pp_type_bind =
  pp_custom_binding  ~prefix:"(" ~suffix:")" pp_tyvar pp_sort

module type PP_Params = sig

  type polarity

  type val_bind

  type cont_bind

  type type_bind

  val pp_pol : formatter -> polarity -> unit

  val pp_bind : formatter -> val_bind -> unit

  val pp_type_bind : formatter -> type_bind -> unit

  val pp_bind_paren : formatter -> val_bind -> unit

  val pp_bind_cc : formatter -> cont_bind -> unit

end

module PP_FullAst = struct

  type polarity = FullAstParams.polarity

  type val_bind = FullAstParams.val_bind

  type type_bind = FullAstParams.type_bind

  type cont_bind = FullAstParams.cont_bind

  let pp_pol fmt = function
    | Positive -> pp_print_string fmt "+"
    | Negative -> pp_print_string fmt "-"

  let pp_bind =
    pp_custom_binding ~prefix:"" ~suffix:"" Var.pp pp_typ

  let pp_type_bind =
    pp_custom_binding ~prefix:"" ~suffix:"" TyVar.pp pp_sort

  let pp_bind_cc =
      pp_custom_binding ~prefix:"" ~suffix:"" CoVar.pp pp_typ

  let pp_bind_paren =
  pp_custom_binding ~prefix:"(" ~suffix:")" Var.pp pp_typ

end

module Make
    (PP_Params : PP_Params)
    (AstParams : AstParams with type polarity = PP_Params.polarity
                            and type val_bind = PP_Params.val_bind
                            and type cont_bind = PP_Params.cont_bind
                            and type type_bind = PP_Params.type_bind)
= struct

  include PP_Params
  include Ast(AstParams)

  let pp_bind_cc_ret fmt bind =
    fprintf fmt ".ret(%a)" pp_bind_cc bind

  let pp_pattern fmt p =
    pp_constructor ConsVar.pp pp_bind pp_type_bind fmt p

  let pp_copattern fmt p =
    pp_destructor DestrVar.pp pp_bind pp_type_bind pp_bind_cc_ret fmt p

  let pp_pol_annot fmt pol =
    fprintf fmt "<<%a>>" pp_pol pol

  let rec pp_value fmt = function
    | MetaVal {node; _} -> pp_pre_value fmt node

  and pp_pre_value fmt = function

    | Var v -> Var.pp fmt v

    | CoTop -> fprintf fmt "GOT_TOP"

    | Bindcc {pol; bind; cmd; _} ->
      fprintf fmt "@[<hov 2>bind/cc%a %a ->@ %a@]"
        pp_pol_annot pol
        pp_bind_cc bind
        pp_cmd cmd

    | Box {kind; bind; cmd; _} ->
      fprintf fmt "@[<hov 2>box(%a) %a ->@ %a@]"
        pp_print_string (string_of_box_kind kind)
        pp_bind_cc bind
        pp_cmd cmd

    | Cons c -> pp_constructor ConsVar.pp pp_value pp_typ fmt c

    | Destr patts ->
      let pp_case fmt (p,c) =
        fprintf fmt "@[<hov 2>| this%a ->@ %a@]" pp_copattern p pp_cmd c in
      fprintf fmt "@[<v 0>@[<v 2>match@,%a@]@,end@]"
        (pp_print_list ~pp_sep:pp_print_space pp_case) patts

    | Fix {self; cmd; cont} ->
      fprintf fmt "@[<hov 2>match this.fix(%a)%a ->@ %a@]"
        pp_bind self
        pp_bind_cc_ret cont
        pp_cmd cmd

  and pp_stack fmt (MetaStack s) = pp_pre_stack fmt s.node

  and pp_pre_stack fmt s =
    pp_print_string fmt "this";
    pp_open_hbox fmt ();
    pp_pre_stack_trail fmt s;
    pp_close_box fmt ()

  and pp_stack_trail fmt (MetaStack s) = pp_pre_stack_trail fmt s.node

  and pp_pre_stack_trail fmt s =
    match s with

    | Ret a -> fprintf fmt "@,.ret(%a)" CoVar.pp a

    | CoZero -> fprintf fmt "@,.GOT_ZERO()"

    | CoBind {pol; bind; cmd; _} ->
      fprintf fmt "@,@[<hov 2>.bind%a %a ->@ %a@]"
        pp_pol_annot pol
        pp_bind_paren bind
        pp_cmd cmd

    | CoBox {kind; stk; _} ->
      fprintf fmt "@,.unbox(%a)%a"
        pp_print_string (string_of_box_kind kind)
        pp_stack_trail stk

    | CoDestr d ->
      pp_print_cut fmt ();
      pp_destructor DestrVar.pp pp_value pp_typ pp_stack_trail fmt d

    | CoCons patts ->
      let pp_case fmt (p,c) =
        fprintf fmt "@[<hov 2>| %a ->@ %a@]" pp_pattern p pp_cmd c in
      fprintf fmt "@[<v 0>@[<v 2>.match@,%a@]@,end@]"
        (pp_print_list ~pp_sep:pp_print_space pp_case) patts

    | CoFix stk ->
      fprintf fmt "@,.fix()%a" pp_stack_trail stk

  and pp_cmd fmt cmd =
    let Command {pol; valu; mid_typ; stk; _} = cmd in
    let pp_annot fmt typ =
      fprintf fmt " : %a" pp_typ typ in
    let MetaVal {node = pre_valu; _} = valu in
    match pre_valu with
    | Var _ | Cons _ ->
      fprintf fmt "%a%a"
        pp_value valu
        pp_stack_trail stk
    | _ ->
      fprintf fmt "@[<v 0>cmd%a%a@ val =@;<1 2>%a@ stk =@;<1 2>%a@ end@]"
        pp_pol_annot pol
        pp_annot mid_typ
        pp_value valu
        pp_stack stk



  let pp_type_bind_def fmt (t,so) =
    fprintf fmt "%a : %a" pp_tyvar t pp_sort so

  let pp_type_bind_def_paren fmt bind = fprintf fmt "(%a)" pp_type_bind_def bind

  let pp_typ_lhs ?sort () fmt (name, args) =
    if args = [] then
      TyConsVar.pp fmt name
    else
      fprintf fmt "@[<hov 2>%a %a@]"
        TyConsVar.pp name
        (pp_print_list ~pp_sep:pp_print_space pp_type_bind_def_paren) args;
    match sort with
    | Some sort -> fprintf fmt " : %a" pp_sort sort
    | None -> ()

  let pp_eqns_def fmt eqns =
    if eqns != [] then
      fprintf fmt " with %a" pp_eqns eqns

let pp_data_decl_item fmt (item,eqns) =
  fprintf fmt "@[<hov 2>| %a%a@]"
    (pp_constructor ConsVar.pp pp_typ pp_type_bind_def) item
    pp_eqns_def eqns

let pp_codata_decl_item fmt (item,eqns) =
  let pp_ret fmt typ = fprintf fmt ".ret(%a)" pp_typ typ in
  fprintf fmt "@[<hov 2>| this%a%a@]"
    (pp_destructor DestrVar.pp pp_typ pp_type_bind_def pp_ret) item
    pp_eqns_def eqns

  let pp_tycons_def fmt (name, def) =
    let {sort; args; content; _} = def in
    match content with
    | Declared ->
      fprintf fmt "decl type %a : %a"
        TyConsVar.pp name
        pp_sort sort
    | Defined content ->
      fprintf fmt "@[<hov 2>type %a =@ %a@]"
        (pp_typ_lhs ~sort:sort ()) (name, args)
        pp_typ content
    | Data content ->
      fprintf fmt "@[<v 2>data %a =@,%a@]"
        (pp_typ_lhs ()) (name, args)
        (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content
    | Codata content ->
      fprintf fmt "@[<v 2>comput %a =@,%a@]"
        (pp_typ_lhs ()) (name, args)
        (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

  let pp_quantified_cons_args pp_k fmt args =
    if List.length args = 0 then
      ()
    else
      fprintf fmt "forall %a. " (pp_print_list ~pp_sep:pp_print_space pp_k) args

  let pp_existential_cons_args pp_k fmt args =
    if List.length args = 0 then
      ()
    else
      fprintf fmt "exists %a. " (pp_print_list ~pp_sep:pp_print_space pp_k) args

  let pp_cons_def fmt (cons, def) =
    let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
    let Consdef {private_typs; typ_args;
                 val_args; resulting_type;
                 equations} = def in
    fprintf fmt "@[<hov 4>/* constructor \"%a\" is@ %a%a%a(%a) : %a%a*/@]"
      ConsVar.pp cons
      (pp_quantified_cons_args pp_aux) typ_args
      (pp_existential_cons_args pp_aux) private_typs
      ConsVar.pp cons
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) val_args
      pp_typ resulting_type
      pp_eqns_def equations

  let pp_destr_def fmt (cons, def) =
    let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
    let Destrdef {private_typs; val_args;
                  typ_args; ret_arg;
                  resulting_type; equations} = def in
    fprintf fmt "@[<hov 4>/* destructor \"%a\" is %a%a%a(%a).ret(%a) : %a%a*/@]"
      DestrVar.pp cons
      (pp_quantified_cons_args pp_aux) typ_args
      (pp_existential_cons_args pp_aux) private_typs
      DestrVar.pp cons
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) val_args
      pp_typ ret_arg
      pp_typ resulting_type
      pp_eqns_def equations

  let pp_var_typ fmt (var, typ) =
    fprintf fmt "@[<hv 2>/* var %a : %a */@]" Var.pp var pp_typ typ

  let pp_tyvar_sort fmt (var, so) =
    fprintf fmt "@[<hv 2>/* tyvar %a : %a */@]" pp_tyvar var pp_sort so

  let pp_sort_def fmt (so,()) =
    fprintf fmt "sort %a" SortVar.pp so

  let pp_rel_def fmt (rel, args) =
    let pp_sep fmt () = pp_print_string fmt " * " in
    fprintf fmt "/*rel %a : %a*/"
      RelVar.pp rel
      (pp_print_list ~pp_sep pp_sort) args

  let pp_definition fmt def =
    pp_open_box fmt 2;
    begin
      match def with
      | Value_declaration {name; typ; pol; _} ->
        fprintf fmt "decl val%a %a"
          pp_pol_annot pol
          pp_bind_def (name, typ)

      | Value_definition {name; typ; pol; content; _} ->
        fprintf fmt "val%a %a =@ %a"
          pp_pol_annot pol
          pp_bind_def (name, typ)
          pp_value content

      | Command_execution {name; pol; content; cont; conttyp; _} ->
        fprintf fmt "cmd%a %a ret %a =@ %a"
          pp_pol_annot pol
          Var.pp name
          (pp_custom_binding ~prefix:"" ~suffix:"" CoVar.pp pp_typ)
          (cont,conttyp)
          pp_cmd content
    end;
    pp_close_box fmt ()

  let pp_program fmt ?debug:(debug=false) (prelude, prog) =
    let is_empty = function [] -> true | _ -> false in
    pp_open_vbox fmt 0;

    let {sort_defs; tycons; cons; destr; sorts;
         vars; covars; relations;
         var_multiplicities; covar_multiplicities} =
      !prelude in

    let sort_defs = SortVar.Env.bindings sort_defs in
    pp_print_list ~pp_sep:pp_print_cut pp_sort_def fmt sort_defs;
    if not (is_empty sort_defs) then pp_print_cut fmt ();

    let relations = RelVar.Env.bindings relations in
    pp_print_list ~pp_sep:pp_print_cut pp_rel_def fmt relations;
    if not (is_empty sort_defs) then pp_print_cut fmt ();

    let typs = TyConsVar.Env.bindings tycons in
    pp_print_list ~pp_sep:pp_print_cut pp_tycons_def fmt typs;
    if not (is_empty typs) then pp_print_cut fmt ();

    if debug then begin
      let conses = ConsVar.Env.bindings cons in
      pp_print_list ~pp_sep:pp_print_cut pp_cons_def fmt conses;
      if not (is_empty conses) then pp_print_cut fmt ();

      let destrs = DestrVar.Env.bindings destr in
      pp_print_list ~pp_sep:pp_print_cut pp_destr_def fmt destrs;
      if not (is_empty destrs) then pp_print_cut fmt ();

      let sorts = TyVar.Env.bindings sorts in
      pp_print_list ~pp_sep:pp_print_cut pp_tyvar_sort fmt sorts;
      if not (is_empty sorts) then pp_print_cut fmt ();

      let vs = Var.Env.to_seq vars |> Seq.map fst in
      let aux fmt v =
        let mult =Var.Env.find_opt v var_multiplicities in
        let typ = Var.Env.find_opt v vars in
        fprintf fmt "/* var %a used %a : %a */"
          Var.pp v
          (pp_print_option ~none:(fun fmt () -> pp_print_string fmt "?") pp_mult) mult
          (pp_print_option ~none:(fun fmt () -> pp_print_string fmt "?") pp_typ) typ in
      pp_print_seq ~pp_sep:pp_print_cut aux fmt vs;
      if not (Seq.is_empty vs) then pp_print_cut fmt ();

      let cos = CoVar.Env.to_seq covars |> Seq.map fst in
      let aux fmt v =
        let mult = CoVar.Env.find_opt v covar_multiplicities in
        let typ = CoVar.Env.find_opt v covars in
        fprintf fmt "/* cont %a used %a : %a */" CoVar.pp v
          (pp_print_option ~none:(fun fmt () -> pp_print_string fmt "?") pp_mult) mult
          (pp_print_option ~none:(fun fmt () -> pp_print_string fmt "?") pp_typ) typ in
      pp_print_seq ~pp_sep:pp_print_cut aux fmt cos ;
      if not (Seq.is_empty cos) then pp_print_cut fmt ();
    end;

    pp_print_list ~pp_sep:pp_print_cut pp_definition fmt prog;

    pp_close_box fmt ()

end

module PP = Make (PP_FullAst) (FullAstParams)
