open Types
open Constructors
open Cst
open Format

let pp_comma_sep fmt () =
  fprintf fmt "@, "

let pp_var fmt v = pp_print_string fmt v

let pp_covar fmt a = pp_print_string fmt a

let pp_sortvar fmt a = pp_print_string fmt a

let pp_tyvar fmt v = pp_print_string fmt v

let pp_consvar fmt v = pp_print_string fmt v

let pp_destrvar fmt v = pp_print_string fmt v

let pp_sort fmt sort = pp_print_string fmt (string_of_sort (fun s -> s) sort)

let pp_rel fmt rel = pp_print_string fmt rel

let pp_typ = Types.pp_typ pp_print_string pp_print_string

let pp_or_underscore pp fmt v = match v with
  | None -> pp_print_string fmt "_"
  | Some v -> pp fmt v

let pp_custom_binding ~prefix ~suffix pp_v pp_t fmt (v, t) =
  match t with
  | None -> pp_v fmt v
  | Some t ->
    fprintf fmt "@[<hov 2>%s%a@ : %a%s@]" prefix pp_v v pp_t t suffix

let pp_bind = pp_custom_binding ~prefix:"" ~suffix:"" pp_var pp_typ

let pp_bind_cc fmt bind =
  fprintf fmt ".ret(%a)"
    (pp_custom_binding ~prefix:"" ~suffix:"" pp_covar pp_typ)
    bind

let pp_type_bind = pp_custom_binding  ~prefix:"" ~suffix:"" pp_tyvar pp_sort

let pp_type_bind_def fmt (t,so) = fprintf fmt "(%a : %a)" pp_tyvar t pp_sort so

let pp_ret fmt typ = fprintf fmt ".ret(%a)" pp_typ typ

let pp_pol_annot fmt pol =
  match pol with
  | Some Positive -> fprintf fmt "+"
  | Some Negative -> fprintf fmt "-"
  | _ -> ()

let rec pp_cons_aux = {
  pp_var = pp_consvar;
  pp_idx = pp_or_underscore pp_typ;
  pp_typ = pp_or_underscore pp_typ;
  pp_arg = pp_value;
  pp_cont = pp_stack_trail
}

and pp_patt_aux = {
  pp_var = pp_consvar;
  pp_typ = pp_or_underscore pp_type_bind;
  pp_idx = pp_or_underscore pp_type_bind;
  pp_arg = pp_bind;
  pp_cont = pp_bind_cc
}

and pp_cons_def_aux = {
  pp_var = pp_consvar;
  pp_idx = pp_type_bind_def;
  pp_typ = pp_type_bind_def;
  pp_arg = pp_typ;
  pp_cont = pp_ret
}


and pp_value fmt = function

  | Var v -> pp_var fmt v.node

  | CoTop _ -> fprintf fmt "GOT_TOP"

  | Bindcc {pol; bind; cmd; _} ->
    fprintf fmt "@[<hov 2>bind/cc%a %a ->@ %a@]"
      pp_pol_annot pol
      (pp_custom_binding ~prefix:"" ~suffix:"" pp_covar pp_typ) bind
      pp_cmd cmd

  | Box {kind; bind; cmd; _} ->
    fprintf fmt "@[<hov 2>box(%a)%a ->@ %a@]"
      pp_print_string (string_of_box_kind kind)
      (pp_custom_binding ~prefix:"" ~suffix:"" pp_covar pp_typ) bind
      pp_cmd cmd

  | Cons c -> pp_constructor pp_cons_aux fmt c.node

  | Destr {cases; default; _} ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>  | this%a ->@ %a@]" (pp_destructor pp_patt_aux) p pp_cmd c in
    let pp_default fmt = function
      | None -> ()
      | Some (a,c) ->
        fprintf fmt "@ @[<hov 2>| %a ->@ %a@]" pp_bind_cc a pp_cmd c in
    fprintf fmt "@[<v 0>match@,%a%a@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) cases
      pp_default default

  | Macro_box {kind; valu; _} ->
    fprintf fmt "box(%s, %a)" (string_of_box_kind kind) pp_value valu

  | Macro_fun {args; valu; _} ->
    fprintf fmt "fun (%a) -> %a"
      (pp_print_list ~pp_sep:pp_comma_sep pp_bind) args
      pp_value valu

  | Fix {self; cont; cmd; _} ->
    fprintf fmt "@[<hov 2>match this.fix(%a)%a->@ %a@]"
      pp_bind self
      pp_bind_cc cont
      pp_cmd cmd


and pp_stack fmt s =
  pp_print_string fmt "this";
  pp_open_hbox fmt ();
  pp_stack_trail fmt s;
  pp_close_box fmt ()

and pp_stack_trail fmt s =
  match s with

  | Ret {var; _} -> fprintf fmt "@,.ret(%a)" pp_covar var

  | CoZero _ -> fprintf fmt "@,.GOT_ZERO()"

  | CoBind {pol; bind; cmd; _} ->
    fprintf fmt "@,@[<hov 2>.bind%a (%a) ->@ %a@]"
      pp_pol_annot pol
      pp_bind bind
      pp_cmd cmd

  | CoBox {kind; stk; _} ->
    fprintf fmt "@,.unbox(%a)%a"
      pp_print_string (string_of_box_kind kind)
      pp_stack_trail stk

  | CoDestr d ->
    pp_print_cut fmt ();
    pp_destructor pp_cons_aux fmt d.node

  | CoCons {cases; default; _} ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>  | %a ->@ %a@]" (pp_constructor pp_patt_aux) p pp_cmd c in
     let pp_default fmt = function
        | None -> ()
        | Some (x,c) ->
          fprintf fmt "@ @[<hov 2>  | %a ->@ %a@]" pp_bind x pp_cmd c in
      fprintf fmt "@[<v 0>.match@ %a%a@,end@]"
        (pp_print_list ~pp_sep:pp_print_space pp_case) cases
        pp_default default

  | CoFix {stk; _} ->
    fprintf fmt "@,.fix()%a"
      pp_stack_trail stk

and pp_cmd fmt cmd =
  let pp_annot fmt typ =
    match typ with
    | None -> ()
    | Some typ -> fprintf fmt "@ : %a" pp_typ typ in

  match cmd with

  | Macro_term {name; valu; typ; cmd; _} ->
    fprintf fmt "@[<hov 2>val %a%a =@ %a in@ %a@]"
      pp_var name
      pp_annot typ
      pp_value valu
      pp_cmd cmd

  | Macro_env {stk; typ; cmd; name; _} ->
    fprintf fmt "@[<hov 2>stk %a = %a%a in@ %a@]"
      pp_covar name
      pp_stack stk
      pp_annot typ
      pp_cmd cmd

  | Macro_match_val {patt; valu; cmd; _} ->
    fprintf fmt "@[<hov 2>match %a =@ %a in@ %a@]"
      (pp_constructor pp_patt_aux) patt
      pp_value valu
      pp_cmd cmd

  | Macro_match_stk {copatt; cmd; stk; _} ->
    fprintf fmt "@[<hov 2>match this%a = %a in@ %a@]"
      (pp_destructor pp_patt_aux) copatt
      pp_stack stk
      pp_cmd cmd

  | Command {pol; valu; typ; stk; _} ->
    match valu with
    | Var _ | Cons _ ->
      fprintf fmt "%a%a"
        pp_value valu
        pp_stack_trail stk
    | _ ->
      fprintf fmt "@[<v 0>cmd%a%a val =@;<1 2>%a@ stk =@;<1 2>%a@ end@]"
        pp_pol_annot pol
        pp_annot typ
        pp_value valu
        pp_stack stk

let pp_typ_lhs fmt (name, args, sort) =
  if args = [] then
    pp_tyvar fmt name
  else
    fprintf fmt "@[<hov 2>%a %a@]"
      pp_tyvar name
      (pp_print_list ~pp_sep:pp_print_space pp_type_bind_def) args;
  match sort with
  | Some sort ->  fprintf fmt " : %a" pp_sort sort
  | None -> ()

let pp_eqn fmt = function
  | Eq (t,u,()) -> fprintf fmt "%a = %a" pp_typ t pp_typ u
  | Rel (rel, args) -> fprintf fmt "%a(%a)"
                         pp_rel rel
                         (pp_print_list ~pp_sep:pp_comma_sep pp_typ) args


let pp_eqns fmt eqns =
  if eqns != [] then
    fprintf fmt " with %a" (pp_print_list ~pp_sep:pp_comma_sep pp_eqn) eqns

let pp_data_decl_item fmt (item,eqns) =
  fprintf fmt "@[<hov 2>| %a%a@]"
    (pp_constructor pp_cons_def_aux) item
    pp_eqns eqns

let pp_codata_decl_item fmt (item,eqns) =

  fprintf fmt "@[<hov 2>| this%a%a@]"
    (pp_destructor pp_cons_def_aux) item
    pp_eqns eqns

let pp_prog_item fmt item =
  pp_open_box fmt 2;
  begin
    match item with
    | Sort_declaration {name; _} ->
      fprintf fmt "decl sort %a" pp_sortvar name

    | Rel_declaration {name; args; _} ->
      let pp_sep fmt () = pp_print_string fmt " * " in
      fprintf fmt "decl rel %a : %a"
        pp_rel name
        (pp_print_list ~pp_sep pp_sort) args

    | Type_declaration {name; sort; _} ->
      fprintf fmt "decl type %a : %a" pp_tyvar name pp_sort sort

    | Type_definition {name; sort; args; content; _} ->
      fprintf fmt "type %a =@ %a" pp_typ_lhs (name, args, Some sort) pp_typ content

    | Data_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>data %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content

    | Codata_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>comput %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

    | Term_definition {name; typ; content; _} ->
      fprintf fmt "val %a =@ %a"
        pp_bind (name, typ)
        pp_value content

    | Term_declaration {name; typ; _} ->
      fprintf fmt "decl val %a"
        pp_bind (name, Some typ)

    | Cmd_execution {name; content; typ; cont; _} ->
      match name with
      | Some name ->
        fprintf fmt "cmd %a ret %a =@ %a"
          pp_var name
          (pp_custom_binding ~prefix:"" ~suffix:"" pp_covar pp_typ) (cont,typ)
          pp_cmd content
      | _ ->
        fprintf fmt "cmd ret %a do@ %a"
          pp_covar cont
          pp_cmd content
  end;
  pp_close_box fmt ()

let pp_program fmt prog =
  pp_open_vbox fmt 0;
  pp_print_list ~pp_sep:pp_print_cut pp_prog_item fmt prog;
  pp_close_box fmt ()
