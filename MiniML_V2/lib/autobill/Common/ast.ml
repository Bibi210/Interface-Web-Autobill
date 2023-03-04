open Types
open Vars
open Misc
open Prelude

module type AstParams = sig
  type val_bind
  type cont_bind
  type type_bind
  type toplevel_bind_annot
  type polarity
  type sort
end

module FullAstParams = struct
  type sort = SortVar.t Types.sort
  type val_bind = Var.t * typ
  type toplevel_bind_annot = typ
  type cont_bind = CoVar.t * typ
  type type_bind = TyVar.t * sort
  type polarity = Types.polarity
end

module Ast (Params : AstParams) = struct

  include Params

  type typ = (TyConsVar.t, TyVar.t) pre_typ
  and constructor = (ConsVar.t, typ, meta_value) Constructors.constructor
  and destructor = (ConsVar.t, typ, meta_value, meta_stack) Constructors.destructor
  and pattern = (ConsVar.t, type_bind, val_bind) Constructors.constructor
  and copattern = (DestrVar.t, type_bind, val_bind, cont_bind) Constructors.destructor

  and meta_value =
      MetaVal of {
        node : pre_value;
        val_typ : typ;
        loc : position;
      }
  and pre_value =
    | Var of Var.t
    | CoTop
    | Bindcc of {
        bind : cont_bind;
        pol : polarity;
        cmd : command;
      }
    | Box of {
        kind : box_kind;
        bind : cont_bind;
        cmd : command;
      }
    | Fix of {
        self : val_bind;
        cont : cont_bind;
        cmd : command
      }
    | Cons of constructor
    | Destr of {
        cases : (copattern * command) list;
        default : (cont_bind * command) option;
      }

  and meta_stack =
      MetaStack of {
        node : pre_stack;
        cont_typ : typ;
        loc : position;
      }
  and pre_stack =
    | Ret of CoVar.t
    | CoZero
    | CoBind of {
        bind : val_bind;
        pol : polarity;
        cmd : command;
      }
    | CoBox of {
        kind : box_kind;
        stk : meta_stack;
      }
    | CoFix of meta_stack
    | CoDestr of destructor
    | CoCons of {
        cases : (pattern * command) list;
        default : (val_bind * command) option;
      }
  and command =
      Command of {
        pol : polarity;
        valu : meta_value;
        stk : meta_stack;
        mid_typ : typ;
        loc : position;
      }

  type prog_item =
    | Value_declaration of {
        bind : Var.t * toplevel_bind_annot;
        pol : polarity;
        loc : position
      }
    | Value_definition  of {
        bind : val_bind;
        pol : polarity;
        loc : position;
        content : meta_value
      }
    | Command_execution of {
        name : Var.t;
        pol : polarity;
        conttyp : toplevel_bind_annot;
        cont : CoVar.t;
        loc : position;
        content : command;
      }

  type program = prelude * prog_item list

  let val_meta ?loc ?typ node =
    let loc = match loc with Some loc -> loc | None -> dummy_pos in
    let val_typ = match typ with Some typ -> typ | None -> tvar (TyVar.fresh ()) in
    MetaVal {node;loc;val_typ;}

  let stack_meta ?loc ?typ node =
    let loc = match loc with Some loc -> loc | None -> dummy_pos in
    let cont_typ = match typ with Some typ -> typ | None -> tvar (TyVar.fresh ()) in
    MetaStack {node;loc;cont_typ}

  module V = struct
    type t = meta_value
    let cotop ?loc ?typ () = val_meta ?loc ?typ CoTop
    let var ?loc ?typ x = val_meta ?loc ?typ (Var x)
    let bindcc ?loc ?typ pol bind cmd = val_meta ?loc ?typ (Bindcc {pol;cmd;bind})
    let box ?loc ?typ kind bind cmd = val_meta ?loc ?typ (Box {kind; bind; cmd})
    let cons ?loc ?typ c = val_meta ?loc ?typ (Cons c)
    let case ?loc ?typ ?default cases = val_meta ?loc ?typ (Destr {cases; default})
  end

  module S = struct
    type t = meta_stack
    let cozero ?loc ?typ () = stack_meta ?loc ?typ CoZero
    let ret ?loc ?typ a = stack_meta ?loc ?typ (Ret a)
    let bind ?loc ?typ pol bind cmd = stack_meta ?loc ?typ (CoBind {pol; bind; cmd})
    let box ?loc ?typ kind stk = stack_meta ?loc ?typ (CoBox {kind; stk})
    let destr ?loc ?typ c = stack_meta ?loc ?typ (CoDestr c)
    let case ?loc ?typ ?default cases  = stack_meta ?loc ?typ (CoCons {default; cases})
  end

end

module FullAst = Ast (FullAstParams)
