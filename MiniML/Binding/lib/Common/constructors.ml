open Format

let cons_names = ["unit"; "pair"; "left"; "right"; "thunk"]

type ('var, 'typ, 'x) constructor =
  | Unit
  | Thunk of 'x
  | Bool of bool
  | Int of int
  | Tupple of 'x list
  | Inj of int * int * 'x
  | PosCons of 'var * 'typ list * 'x list

let unit = Unit
let pair a b = Tupple [a;b]
let left a = Inj (0, 2, a)
let right b = Inj (1, 2, b)
let thunk a = Thunk a
let poscons c typs args = PosCons (c, typs, args)

let consvar_of_constructor = function
  | PosCons (name, _, _) -> Some name
  | _ -> None

let destr_names = ["call"; "yes"; "no"; "closure"]

type ('var, 'typ, 'x ,'a) destructor =
  | Call of 'x list * 'a
  | Proj of int * int * 'a
  | Closure of 'a
  | NegCons of 'var * 'typ list * 'x list * 'a

let call x a = Call (x,a)
let yes a = Proj (0, 2, a)
let no a = Proj (1, 2, a)
let closure a = Closure a
let negcons c typs args cont = NegCons (c,typs,args,cont)

let destrvar_of_destructor = function
  | NegCons (name, _, _, _) -> Some name
  | _ -> None

let pp_comma_sep fmt () = fprintf fmt ",@, "

let pp_constructor pp_cons pp_kx pp_kt fmt cons =
  match cons with
  | Bool b -> pp_print_bool fmt b
  | Int n -> fprintf fmt "int(%n)" n
  | Unit -> pp_print_string fmt "unit()"
  | Tupple vs ->
    fprintf fmt "@[<hov 2>tupple(%a)@]" (pp_print_list ~pp_sep:pp_comma_sep pp_kx) vs
  | Inj (i,n,x) -> fprintf fmt "inj(%n/%n, %a)" i n pp_kx x
  | Thunk x -> fprintf fmt "thunk(%a)" pp_kx x
  | PosCons (c, typs, args) ->
    pp_cons fmt c;
    if typs != [] then
      fprintf fmt "<@[<hov 2>%a@]>" (pp_print_list ~pp_sep:pp_comma_sep pp_kt) typs;
    fprintf fmt "(@[<hov 2>%a@])" (pp_print_list ~pp_sep:pp_comma_sep pp_kx) args

let pp_destructor pp_destr pp_kx pp_kt pp_ka fmt destr =
  match destr with
  | Call (x,a) -> fprintf fmt ".call(%a)%a"
                    (pp_print_list ~pp_sep:pp_comma_sep pp_kx) x pp_ka a
  | Proj (i,n,a) -> fprintf fmt ".proj(%n/%n)%a" i n pp_ka a
  | Closure a -> fprintf fmt ".closure()%a" pp_ka a
  | NegCons (c, typs, args, a) ->
    pp_print_string fmt ".";
    pp_destr fmt c;
     if typs != [] then
      fprintf fmt "<@[<hov 2>%a@]>" (pp_print_list ~pp_sep:pp_comma_sep pp_kt) typs;
    if args != [] then
      fprintf fmt "(@[<hov 2>%a@])" (pp_print_list ~pp_sep:pp_comma_sep pp_kx) args
    else
      fprintf fmt "()";
    pp_ka fmt a
