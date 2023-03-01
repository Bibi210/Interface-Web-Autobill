open Format
open Types

let cons_names = ["true"; "false"; "int"; "unit"; "pair"; "left"; "right"; "thunk"]


  type 'var constructor_tag =
    | Unit
    | Thunk
    | Bool of bool
    | Int of int
    | Tupple of int
    | Inj of int * int
    | PosCons of 'var

  type ('var, 'idx, 'typ, 'arg) constructor = Raw_Cons of {
    tag : 'var constructor_tag;
    idxs : 'idx list;
    typs : 'typ list;
    args : 'arg list
  }

  let cons tag idxs typs args = Raw_Cons {tag; idxs; typs; args}
  let unit = cons Unit [] [] []
  let tuple xs = cons (Tupple (List.length xs)) [] [] xs
  let inj i n a = cons (Inj (i,n)) [] [] [a]
  let thunk a = cons Thunk [] [] [a]

  let destr_names = ["call"; "yes"; "no"; "closure"]

  type 'var destructor_tag =
    | Call of int
    | Proj of int * int
    | Closure of box_kind option
    | NegCons of 'var

  type ('var, 'idx, 'typ, 'arg, 'cont) destructor = Raw_Destr of {
    tag : 'var destructor_tag;
    idxs : 'idx list;
    typs : 'typ list;
    args : 'arg list;
    cont : 'cont
  }

  let destr tag idxs typs args cont = Raw_Destr {tag; idxs; typs; args; cont}
  let call xs a = destr (Call (List.length xs)) [] [] xs a
  let proj i n a = destr (Proj (i,n)) [] [] [] a
  let closure ?q a = destr (Closure q) [] [] [] a


  let pp_comma_sep fmt () = fprintf fmt ",@, "

type ('var, 'idx, 'typ, 'arg, 'cont) pp_cons_aux = {
  pp_var : Format.formatter -> 'var -> unit;
  pp_idx : Format.formatter -> 'idx -> unit;
  pp_typ : Format.formatter -> 'typ -> unit;
  pp_arg : Format.formatter -> 'arg -> unit;
  pp_cont : Format.formatter -> 'cont -> unit;
}


  let pp_constructor_tag aux fmt cons =
    match cons with
    | Bool b -> pp_print_bool fmt b
    | Int n -> fprintf fmt "int{%n}" n
    | Unit -> pp_print_string fmt "unit"
    | Tupple _ -> fprintf fmt "tuple"
    | Inj (i,n) -> fprintf fmt "inj{%n,%n}" i n
    | Thunk -> fprintf fmt "thunk"
    | PosCons c -> aux.pp_var fmt c

  let pp_destructor_tag aux fmt destr =
    match destr with
    | Call _ -> fprintf fmt "call"
    | Proj (i,n) -> fprintf fmt "proj{%n,%n}" i n
    | Closure q -> (match q with
      | Some q -> pp_print_string fmt (string_of_box_kind q)
      | None -> pp_print_string fmt "closure")
    | NegCons c -> aux.pp_var fmt c

  let pp_idxs_and_typs aux fmt idxs typs =
    fprintf fmt "<%a" (pp_print_list ~pp_sep:pp_comma_sep aux.pp_typ) typs;
    if idxs <> [] then
      fprintf fmt ",%a"(pp_print_list ~pp_sep:pp_comma_sep aux.pp_idx) idxs;
    fprintf fmt ">"

let pp_constructor aux fmt (Raw_Cons {tag; idxs; typs; args}) = begin
  pp_open_hbox fmt ();
  pp_constructor_tag aux fmt tag;
  if idxs <> [] || typs <> [] then
    pp_idxs_and_typs aux fmt idxs typs;
  fprintf fmt "(%a)" (pp_print_list ~pp_sep:pp_comma_sep aux.pp_arg) args;
  pp_close_box fmt ()
end

let pp_destructor aux fmt (Raw_Destr {tag; idxs; typs; args; cont}) = begin
  pp_print_string fmt ".";
  pp_destructor_tag aux fmt tag;
  if idxs <> [] || typs <> [] then
    pp_idxs_and_typs aux fmt idxs typs;
  fprintf fmt "(%a)" (pp_print_list ~pp_sep:pp_comma_sep aux.pp_arg) args;
  aux.pp_cont fmt cont
end
