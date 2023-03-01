exception Undefined_variable of string

module IntM = Map.Make (struct
    type t = int
    let compare = compare
  end)

  module StrM = Map.Make (struct
    type t = string
    let compare = compare
  end)

module type LocalVarParam = sig
  val default_name :  string
end

module type LocalVar = sig
  type t
  module Env : Map.S with type key = t
  val of_string : string -> t
  val fresh : unit -> t
  val _debug_of_int : int -> t
  val _debug_to_int : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module LocalVar (Param : LocalVarParam) : LocalVar = struct

  open Param
  type t = int

  module Env = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let names = ref IntM.empty

  let of_string s =
    let v = Global_counter.fresh_int () in
    names := IntM.add v s !names;
    v

  let fresh () =
    let v = Global_counter.fresh_int () in
    names := IntM.add v (default_name ^ "__" ^ string_of_int v) !names;
    v

  let _debug_of_int n =
    names := IntM.add n (default_name ^ "__" ^ string_of_int n) !names;
    n

  let _debug_to_int v = v

  let to_string v =
    try
      IntM.find v !names
    with
    | Not_found -> raise (Undefined_variable (string_of_int v))

  let pp fmt v = Format.pp_print_string fmt (to_string v)

end

module Var = LocalVar (struct
    let default_name = "x"
  end)

module CoVar = LocalVar (struct
    let default_name = "a"
  end)

module TyVar = LocalVar (struct
    let default_name = "T"
  end)

module DefVar = LocalVar (struct
    let default_name = "def"
  end)

module ConsVar = LocalVar (struct
    let default_name = "cons"
  end)

module DestrVar = ConsVar

module SortVar = LocalVar (struct
    let default_name = "sort"
  end)

module TyConsVar = LocalVar (struct
    let default_name = "Tycons"
  end)

module RelVar = LocalVar (struct
    let default_name = "rel"
  end)
