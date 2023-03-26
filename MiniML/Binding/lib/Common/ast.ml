open Types
open Vars
open Constructors
open Misc
open Prelude

module type AstParams = sig
  type val_bind
  type cont_bind
  type type_bind
  type polarity
  type sort
end

module FullAstParams = struct
  type sort = SortVar.t Types.sort
  type val_bind = Var.t * typ
  type cont_bind = CoVar.t * typ
  type type_bind = TyVar.t * sort
  type polarity = Types.polarity
end

module Ast (Params : AstParams) = struct

  include Params

  type typ = (TyConsVar.t, TyVar.t) pre_typ
  type pattern = (ConsVar.t, type_bind, val_bind) constructor
  type copattern = (DestrVar.t, type_bind, val_bind, cont_bind) destructor

  type meta_value =
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
    | Cons of (ConsVar.t, typ, meta_value) constructor
    | Destr of (copattern * command) list

  and meta_stack =
      MetaStack of {
        node : pre_stack;
        cont_typ : typ;
        final_typ : typ;
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
    | CoDestr of (DestrVar.t, typ, meta_value, meta_stack) destructor
    | CoCons of (pattern * command) list
  and command =
      Command of {
        pol : polarity;
        valu : meta_value;
        stk : meta_stack;
        mid_typ : typ;
        final_typ : typ;
        loc : position;
      }

  type prog_item =
    | Value_declaration of {
      name : Var.t;
      typ : typ;
      pol : polarity;
      loc : position
    }
    | Value_definition  of {
      name : Var.t;
      typ : typ;
      pol : polarity;
      loc : position;
      content : meta_value
    }
    | Command_execution of {
      name : Var.t;
      pol : polarity;
      conttyp : typ;
      cont : CoVar.t;
      loc : position;
      content : command;
    }

  type program = prelude * prog_item list

  let dummy_val_meta v = MetaVal {
      node = v;
      loc = dummy_pos;
      val_typ = tvar (TyVar.fresh ())
    }

  let dummy_stack_meta s = MetaStack {
      node = s;
      loc = dummy_pos;
      cont_typ = tvar (TyVar.fresh ());
      final_typ = tvar (TyVar.fresh ());
    }

  module V = struct
    type t = meta_value
    let cotop = dummy_val_meta CoTop
    let var x = dummy_val_meta (Var x)
    let bindcc pol bind cmd = dummy_val_meta (Bindcc {pol;cmd;bind})
    let box kind bind cmd = dummy_val_meta (Box {kind; bind; cmd})
    let cons c = dummy_val_meta (Cons c)
    let case l = dummy_val_meta (Destr l)
  end

  module S = struct
    type t = meta_stack
    let cozero = dummy_stack_meta CoZero
    let ret a = dummy_stack_meta (Ret a)
    let bind pol bind cmd = dummy_stack_meta (CoBind {pol; bind; cmd})
    let box kind stk = dummy_stack_meta (CoBox {kind; stk})
    let destr c = dummy_stack_meta (CoDestr c)
    let case l = dummy_stack_meta (CoCons l)
  end

end

module FullAst = Ast (FullAstParams)
