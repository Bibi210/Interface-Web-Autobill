open Vars
open Ast
open Misc
open Types
open Format

module USortVar = LocalVar (struct
    let default_name = "pol"
  end)

type usort =
  | Loc of position * usort
  | Litt of SortVar.t Types.sort
  | Redirect of USortVar.t

let rec string_of_usort = function
  | Loc (pos, upol) -> Printf.sprintf "%s@\"%s\"" (string_of_usort upol)  (string_of_position pos)
  | Litt so -> string_of_sort SortVar.to_string so
  | Redirect var -> USortVar.to_string var

module InternAstParams = struct
  include FullAstParams
  type polarity = usort
  type sort = usort
  type type_bind = TyVar.t * usort
  let print_debug_names = true
end

module InternAst = Ast (InternAstParams)

let pp_polvar fmt v = pp_print_string fmt (USortVar.to_string v)

module PP_InternAst = struct
  include InternAstParams

  let rec pp_pol fmt upol =
    match upol with
    | Redirect v -> fprintf fmt "<<%a>>" pp_polvar v
    | Loc (_, upol) -> pp_pol fmt upol
    | Litt p -> PrettyPrinter.pp_sort fmt p

  let pp_bind = PrettyPrinter.PP.pp_bind

  let pp_meta_tyvar fmt v = fprintf fmt "<<%a>>" (TyVar.pp ~debug:true) v

  let pp_meta_typ fmt t = fprintf fmt "<<%a>>" PrettyPrinter.pp_typ t

  let pp_type_bind =
    PrettyPrinter.pp_custom_binding ~prefix:"" ~suffix:"" pp_meta_tyvar pp_pol

  let pp_bind_paren =
    PrettyPrinter.pp_custom_binding ~prefix:"(" ~suffix:")" Var.pp pp_meta_typ

  let pp_bind_cc =
    PrettyPrinter.pp_custom_binding ~prefix:"(" ~suffix:")" CoVar.pp pp_meta_typ

  let pp_toplevel_bind_annot = PrettyPrinter.pp_typ
end

include PrettyPrinter
include PrettyPrinter.Make(PP_InternAst)(InternAstParams)
