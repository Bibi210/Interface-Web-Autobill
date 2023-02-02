(* Ne pas finir le full typage *)
type types =
  | Int_t
  | Bool_t
  | Tuple_t of types array
  | Weak_t
  | List_t of types
  | Lambda_t of types list * types

type identifier =
  { name : string
  ; type_t : types option
  }

type const =
  | Integer of int
  | Boolean of bool

module VerifiedTree = struct
  type expr =
    | Const of const
    | Tuple of expr array
    | Seq of expr array
    | Cons of
        { hd : expr
        ; tail : expr
        }
    | Nil
    | Binding of
        { var_name : string
        ; init : expr
        ; content : expr
        }
    | Var of { var_name : string }
    | Lambda of
        { args : string list
        ; body : expr
        }
    | Call of
        { func : expr
        ; args : expr list
        }

  type prog = expr
end

module Syntax = struct
  type expr =
    | Const of
        { const : const
        ; loc : Helpers.position
        }
    | Tuple of
        { content : expr array
        ; loc : Helpers.position
        }
    | Binding of
        { ident : identifier
        ; init : expr
        ; content : expr
        ; loc : Helpers.position
        }
    | Var of
        { ident : identifier
        ; loc : Helpers.position
        }
    | Seq of
        { content : expr array
        ; loc : Helpers.position
        }
    | Cons of
        { hd : expr (* Use of OCAML List ?? *)
        ; tail : expr
        ; loc : Helpers.position
        }
    | Nil of { loc : Helpers.position }
    | Lambda of
        { args : identifier list
        ; body : expr
        ; loc : Helpers.position
        }
    | Call of
        { func : expr
        ; args : expr list
        ; loc : Helpers.position
        }

  type prog = expr
end

(*TODO: Flow de Control *)
(*TODO: Operateurs de Base *)
