open Misc
open Vars
open Format

type polarity = Positive | Negative
type 'var sort =
  | Base of polarity
  | Index of 'var
  | Arrow of 'var sort * 'var sort

let positive = Positive
let negative = Negative
let sort_postype = Base Positive
let sort_negtype = Base Negative
let sort_base p = Base p
let sort_idx i = Index i
let sort_arrow arg ret = List.fold_right (fun arg so -> Arrow (arg,so)) arg ret

let rec unmk_arrow sort = match sort with
  | Arrow (s,t) -> let (args,ret) = unmk_arrow t in (s::args,ret)
  | _ -> ([], sort)

let is_base_sort = function
  | Base _ -> true
  | _ -> false
let is_base_index_sort = function
  | Index _ -> true
  | _ -> false
let rec is_index_sort = function
  | Base _ -> false
  | Index _ -> true
  | Arrow (s,t) -> is_index_sort s && is_index_sort t
let rec is_monotype_sort = function
  | Base _ -> true
  | Index _ -> false
  | Arrow (s,t) -> is_index_sort s && is_monotype_sort t
let is_monotype_sort_with_base_indices = function
  | Base _ -> true
  | Index _ -> false
  | Arrow (s,t) -> is_base_index_sort s && is_monotype_sort t
let rec is_polytype_sort = function
  | Base _ -> true
  | Index _ -> false
  | Arrow _ as so when is_monotype_sort so -> true
  | Arrow (s,t) -> is_monotype_sort s && is_polytype_sort t
let is_valid_sort so = is_index_sort so || is_polytype_sort so

let string_of_polarity  = function
  | Positive -> "+"
  | Negative -> "-"
let rec string_of_sort kv = function
  | Base p -> string_of_polarity p
  | Index i -> kv i
  | Arrow (s,t) -> "(" ^ (string_of_sort kv s) ^ " -> " ^ (string_of_sort kv t) ^")"
let pp_sort kv fmt sort = pp_print_string fmt (string_of_sort kv sort)



type box_kind = Linear | Affine | Exponential

let linear = Linear
let affine = Affine
let exp = Exponential

let string_of_box_kind = function
  | Linear -> "Lin"
  | Affine -> "Aff"
  | Exponential -> "Exp"



type var_multiplicity =
  | MulZero
  | MulOne
  | MulMany

let pp_mult fmt = function
  | MulZero -> pp_print_string fmt "0"
  | MulOne -> pp_print_string fmt "1"
  | MulMany -> pp_print_string fmt "*"


let mult a b = match a,b with
  | MulZero, x | x, MulZero -> x
  | MulMany, _ | _, MulMany -> MulMany
  | MulOne, MulOne -> MulMany

let update a b = match b with
  | None -> Some a
  | Some b -> Some (mult a b)


let type_cons_names =
  ["Unit"; "Zero"; "Top"; "Bottom"; "Prod"; "Sum"; "Fun"; "Choice"; "Thunk"; "Closure"]

type 'tycons type_cons =
  | Int
  | Bool
  | Unit
  | Zero
  | Top
  | Bottom
  | Thunk
  | Closure
  | Prod of int
  | Sum of int
  | Fun of int
  | Choice of int
  | Cons of 'tycons

let pp_type_cons kvar fmt cons =
  match cons with
  | Int -> pp_print_string fmt "Int"
  | Bool -> pp_print_string fmt "Bool"
  | Unit -> pp_print_string fmt "Unit"
  | Zero -> pp_print_string fmt "Zero"
  | Top -> pp_print_string fmt "Top"
  | Bottom -> pp_print_string fmt "Bottom"
  | Thunk -> pp_print_string fmt "Thunk"
  | Closure -> pp_print_string fmt "Closure"
  | Prod _ -> pp_print_string fmt "Prod"
  | Sum _ -> pp_print_string fmt "Sum"
  | Fun _ -> pp_print_string fmt "Fun"
  | Choice _ -> pp_print_string fmt "Choice"
  | Cons var -> kvar fmt var

type ('tycons, 'var) pre_typ =
  | TCons of {node : 'tycons type_cons;
              loc : position}
  | TApp of {tfun : ('tycons, 'var) pre_typ;
             args : ('tycons, 'var) pre_typ list;
             loc : position}
  | TBox of {kind : box_kind;
             node : ('tycons, 'var) pre_typ;
             loc : position}
  | TVar of {node : 'var;
             loc : position}
  | TFix of ('tycons, 'var) pre_typ
  | TPos of ('tycons, 'var) pre_typ
  | TNeg of ('tycons, 'var) pre_typ
  | TInternal of 'var

type typ = (TyConsVar.t, TyVar.t) pre_typ


let pos t = TPos t
let neg t = TNeg t
let tvar ?loc:(loc = dummy_pos) node = TVar {node; loc}
let posvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let negvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let boxed ?loc:(loc = dummy_pos) kind node = TBox {kind; node; loc}
let fix t = TFix t
let cons ?loc:(loc = dummy_pos) node = TCons {node; loc}
let app ?loc:(loc = dummy_pos) tfun args = TApp {tfun; args; loc}

let unit_t = cons Unit
let int = cons Int
let bool = cons Bool
let zero = cons Zero
let top = cons Top
let bottom = cons Bottom
let prod ts = app (cons (Prod (List.length ts))) ts
let sum ts = app (cons (Sum (List.length ts))) ts
let func ts = app (cons (Fun (List.length ts - 1))) ts
let choice ts = app (cons (Choice (List.length ts))) ts
let typecons v args = app (cons (Cons v)) args
let thunk_t t = app (cons Thunk) [t]
let closure_t t = app (cons Closure) [t]

let pp_tyvar fmt v = pp_print_string fmt (TyVar.to_string v)

let pp_typ pp_tycons pp_tyvar fmt t =
  let rec go fmt t = match t with
  | TPos t -> fprintf fmt "+%a" go t
  | TNeg t -> fprintf fmt "-%a" go t
  | TVar v -> pp_tyvar fmt v.node
  | TInternal v -> pp_tyvar fmt v
  | TBox b -> fprintf fmt "@[<hov 2>(%s@ %a)@]" (string_of_box_kind b.kind) go b.node
  | TFix t -> fprintf fmt "@[<hov 2>(fix@ %a)@]" go t
  | TCons {node;_} -> pp_type_cons pp_tycons fmt node
  | TApp {tfun;args;_} ->
    match tfun with
    | TCons {node=Prod _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ * ") go)  args
    | TCons {node=Sum _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ + ") go) args
    | TCons {node=Choice _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ & ") go) args
    | TCons{node=Fun _;_} when args != [] ->
      let[@warning "-partial-match"] ret::args = args in
      fprintf fmt "(Fun (%a) -> %a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,, ") go) args
        go ret
    | _ ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]"
        go tfun
        (pp_print_list ~pp_sep:pp_print_space go) args

  in go fmt t