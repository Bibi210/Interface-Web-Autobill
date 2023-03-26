open Types
open Constructors
open Misc

type sovar = string
type tyvar = string
type var = string
type covar = string
type consvar = string
type destrvar = string
type relvar = string

type typ = (string, string) pre_typ
type sort = string Types.sort

type bind = var * typ option
type type_bind = tyvar * sort option
type cont_bind = covar * typ option

type pattern = (consvar, type_bind option, bind) constructor
type copattern = (destrvar, type_bind option, bind, cont_bind) destructor

type cst_eqn =
  | Eq of typ * typ * unit
  | Rel of relvar * typ list

type value =

  | Var of {
        node : var;
        loc : position
    }

  | CoTop of {loc : position}

  | Bindcc of {
      bind : cont_bind;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | Box of {
      kind : box_kind;
      bind : cont_bind;
      cmd : command;
      loc : position
    }

  | Fix of {
      self : bind;
      cont : cont_bind;
      cmd : command;
      loc : position
    }

  | Cons of {
      node : (consvar, typ option, value) constructor;
      loc : position
    }

  | Destr of {
      node : (copattern * command) list;
      loc : position;
    }

  | Macro_box of {
      kind : box_kind;
      valu : value;
      loc : position
    }

  | Macro_fun of {
      args : (var * typ option) list;
      valu : value;
      loc : position;
    }


and stack =

  | Ret of { var : var; loc : position }

  | CoZero of {loc : position}

  | CoBind of {
      bind : bind;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | CoBox of {
      kind : box_kind;
      stk : stack;
      loc : position
    }

  | CoFix of {
      stk : stack;
      loc : position
    }

  | CoDestr of {
      node : (destrvar, typ option, value, stack) destructor;
      loc : position
    }

  | CoCons of {
      node : (pattern * command) list;
      loc : position
    }

and command =
  | Command of {
    pol : polarity option;
    valu : value;
    stk : stack;
    typ : typ option;
    loc : position
  }
  | Macro_term of {
      name : string;
      pol : polarity option;
      typ : typ option;
      valu : value;
      cmd : command;
      loc : position
    }
  | Macro_env of {
      typ : typ option;
      pol : polarity option;
      name : string;
      stk : stack;
      cmd : command;
      loc : position
    }
  | Macro_match_val of {
      patt : pattern;
      pol : polarity option;
      valu : value;
      cmd : command;
      loc : position
    }
  | Macro_match_stk of {
      copatt : copattern;
      pol : polarity option;
      stk : stack;
      cmd : command;
      loc : position
    }

type program_item =

  | Sort_declaration of {
      name : sovar;
      loc : position
    }

  | Rel_declaration of {
      name : relvar;
      loc : position;
      args :sort list
    }

  | Type_declaration of {
      name : tyvar;
      sort : sort;
      loc : position
    }

  | Type_definition of {
      name : tyvar;
      sort : sort;
      args : (tyvar * sort) list;
      content : typ;
      loc : position
    }

  | Data_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : ((consvar, tyvar * sort, typ) constructor * cst_eqn list) list;
      loc : position
    }

  | Codata_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : ((destrvar, tyvar * sort, typ, typ) destructor * cst_eqn list) list;
      loc : position
    }

  | Term_definition of {
      name : var;
      typ : typ option;
      content : value;
      loc : position
    }

  | Term_declaration of {
      name : var;
      typ : typ;
      loc : position
    }

  | Cmd_execution of {
      name : var option;
      typ : typ option;
      cont : var;
      content : command;
      loc : position
    }

type program = program_item list

let loc_of_value = function
  | Var {loc;_} | Bindcc {loc;_} | Box {loc; _} | Cons {loc;_} | Destr {loc;_}
  | Macro_box {loc; _} | Macro_fun {loc; _} | CoTop {loc} | Fix {loc;_} -> loc

let loc_of_stack = function
  | Ret {loc;_} | CoBind {loc;_} | CoBox {loc;_} | CoCons {loc;_} | CoZero {loc}
  | CoDestr {loc;_} | CoFix {loc;_} -> loc

let loc_of_cmd = function
  | Command {loc;_} | Macro_term {loc;_} | Macro_env {loc;_} | Macro_match_val {loc;_}
  | Macro_match_stk {loc;_} -> loc

let loc_of_item = function
  | Type_declaration {loc;_} | Type_definition {loc;_}
  | Data_definition {loc;_} | Codata_definition {loc;_}
  | Term_definition {loc;_} | Term_declaration {loc;_}
  | Cmd_execution {loc;_} | Sort_declaration {loc;_}
  | Rel_declaration {loc;_} ->
    loc

module V = struct
  type t = value
  let cotop ?loc:(loc = dummy_pos) () = CoTop {loc}
  let var ?loc:(loc = dummy_pos) x = Var {node = x; loc}
  let bindcc ?loc:(loc = dummy_pos) ?pol:pol a typ cmd = Bindcc {pol; bind=(a,typ); cmd; loc}
  let box ?loc:(loc = dummy_pos) kind a typ cmd = Box {kind; bind=(a,typ); cmd; loc}
  let cons ?loc:(loc = dummy_pos) c = Cons {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = Destr {node = l; loc}
  let macro_fun ?loc:(loc = dummy_pos) args valu = Macro_fun {loc; args; valu}
  let macro_box ?loc:(loc = dummy_pos) kind valu = Macro_box {loc; kind; valu}
  let fix ?loc:(loc = dummy_pos) self cont cmd = Fix {loc; self; cont; cmd}
end

module S = struct
  type t = stack
  let cozero ?loc:(loc = dummy_pos) () = CoZero {loc}
  let ret ?loc:(loc = dummy_pos) a = Ret {var = a; loc}
  let bind ?loc:(loc = dummy_pos) ?pol:pol name typ cmd = CoBind {pol; bind =(name,typ); cmd; loc}
  let box ?loc:(loc = dummy_pos) kind stk = CoBox {kind; stk; loc}
  let destr ?loc:(loc = dummy_pos) c = CoDestr {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = CoCons {node = l; loc}
  let cofix ?loc:(loc = dummy_pos) stk = CoFix {stk; loc}
end

type t = command
let cmd ?loc:(loc = dummy_pos) ?pol:pol typ valu stk =
  Command {pol; typ; valu; stk; loc}
let (|+|) (t : V.t) (s : S.t) = cmd ~pol:Positive None t s
let (|-|) (v : V.t) (e : S.t) = cmd ~pol:Negative None v e
let (|~|) (t : V.t) (e : S.t) = cmd None t e
let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)

let cmd_let_val ?loc:(loc = dummy_pos) ?pol:pol name typ valu cmd =
  Macro_term {pol; loc; name; typ; valu; cmd}
let cmd_let_env ?loc:(loc = dummy_pos) ?pol:pol name typ stk cmd =
  Macro_env {pol; loc; name; typ; stk; cmd}
let cmd_match_val ?loc:(loc = dummy_pos) ?pol:pol valu patt cmd =
  Macro_match_val {pol; loc; patt; valu; cmd}
let cmd_match_env ?loc:(loc = dummy_pos) ?pol:pol stk copatt cmd =
  Macro_match_stk {pol; loc; copatt; stk; cmd}
