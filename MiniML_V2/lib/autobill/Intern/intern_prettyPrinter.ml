open Vars
open Format
open PrettyPrinter
open Intern_common


let pp_polvar fmt v = pp_print_string fmt (USortVar.to_string v)

module PP_InternAst = struct
  include InternAstParams

  let rec pp_pol fmt upol =
    match upol with
    | Redirect v -> fprintf fmt "<<%a>>" pp_polvar v
    | Loc (_, upol) -> pp_pol fmt upol
    | Litt p -> pp_sort fmt p

  let pp_bind = PrettyPrinter.PP.pp_bind

  let pp_meta_tyvar fmt v = fprintf fmt "<<%a>>" TyVar.pp v

  let pp_meta_typ fmt t = fprintf fmt "<<%a>>" pp_typ t

  let pp_type_bind =
    PrettyPrinter.pp_custom_binding ~prefix:"" ~suffix:"" pp_meta_tyvar pp_pol

  let pp_bind_paren =
    PrettyPrinter.pp_custom_binding ~prefix:"(" ~suffix:")" Var.pp pp_meta_typ

  let pp_bind_cc =
    PrettyPrinter.pp_custom_binding ~prefix:"(" ~suffix:")" CoVar.pp pp_meta_typ

end

include PrettyPrinter
include PrettyPrinter.Make(PP_InternAst)(InternAstParams)

let dump_env fmt env =
  let aux pp_k pp_v k v = Format.fprintf fmt "%a : %a@," pp_k k pp_v v in
  let rec pp_upol fmt = function
    | Litt p -> pp_sort fmt p
    | Loc (loc, upol) -> pp_print_string fmt (Misc.string_of_position loc); pp_upol fmt upol
    | Redirect var -> pp_polvar fmt var in
  begin
    pp_print_newline fmt ();
    pp_open_vbox fmt 0;
    pp_print_string fmt "####### Internal state";
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of constructor";
    pp_print_cut fmt ();
    TyConsVar.Env.iter (aux TyConsVar.pp pp_sort) env.tycons_sort;
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of type variables";
    pp_print_cut fmt ();
    TyVar.Env.iter (aux TyVar.pp pp_sort) env.prelude_typevar_sort;
    TyVar.Env.iter (aux TyVar.pp pp_upol) env.tyvarsorts;
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of variables";
    pp_print_cut fmt ();
    Var.Env.iter (aux Var.pp pp_polvar) env.varsorts;
    pp_print_cut fmt ();
    pp_print_string fmt "### Sorts of continuations";
    pp_print_cut fmt ();
    CoVar.Env.iter (aux CoVar.pp pp_polvar) env.covarsorts;
    pp_print_cut fmt ();
    pp_print_string fmt "### Unifier";
    pp_print_cut fmt ();
    USortVar.Env.iter (aux pp_polvar pp_upol) env.unifier;
    pp_close_box fmt ();
    pp_print_newline fmt ()
  end
