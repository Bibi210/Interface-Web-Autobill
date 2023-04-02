
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
  val of_primitive : string -> t
  val is_primitive : t -> bool
  val fresh : unit -> t
  val to_int : t -> int
  val to_string : ?debug:bool -> t -> string
  val pp : ?debug:bool -> Format.formatter -> t -> unit
end

module LocalVar (Param : LocalVarParam) : LocalVar = struct

  (* Creating variables with of_string is idempotent, and is the identity with
     of_primitive, as in: *)
  (* to_string (of_string s) != s *)
  (* to_string (of_primitive s) == s *)
  (* to_string (of_string s) == to_string (of_string (to_string (of_string s))) *)

  open Param
  type t = int

  module Env = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let names : string IntM.t ref = ref IntM.empty
  let prim_strs : int StrM.t ref = ref StrM.empty
  let prims : unit IntM.t ref = ref IntM.empty
  let defaults : bool IntM.t ref = ref IntM.empty

  let is_primitive v = IntM.mem v !prims

  let is_default v = Option.value ~default:false (IntM.find_opt v !defaults)

  let to_string ?(debug = true) v =
    match IntM.find_opt v !names with
    | None ->
      let mess = "Unregistered variable: " ^ string_of_int v in
      Misc.fail_invariant_break mess
    | Some s -> if (debug && not (is_primitive v)) || is_default v then
        s ^ "__" ^ (string_of_int v)
      else
        s

  let to_int v = v

  let _of_string ~is_default s =
    let v = Global_counter.fresh_int () in
    names := IntM.add v s !names;
    defaults := IntM.add v is_default !defaults;
    v

  let of_string s = match StrM.find_opt s !prim_strs with
    | None -> _of_string ~is_default:false s
    | Some v -> v

  let of_primitive s =
    let v = Global_counter.fresh_int () in
    names := IntM.add v s !names;
    prim_strs := StrM.add s v !prim_strs;
    prims := IntM.add v () !prims;
    defaults := IntM.add v false !defaults;
    v


  let fresh () = _of_string ~is_default:true default_name

  let pp ?(debug = true) fmt v = Format.pp_print_string fmt (to_string ~debug v)

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
