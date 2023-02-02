type etype =
  | TypeInt
  | TypeBool
  | TypeLambda of etype list * etype
  | TypeConstructor of etype list

type expr =
  { etype : etype
  ; node : pre_expr
  ; loc : Helpers.position
  }

and pre_expr =
  | Integer of int
  | Boolean of bool
  | Binding of
      { ident : string
      ; init : expr
      ; content : expr
      }
  | Variable of string
  | Sequence of expr array
  | Lambda of
      { args : string list
      ; body : expr
      }
  | Call of
      { func : expr
      ; args : expr list
      }
  | Construct of
      { name : string
      ; to_group : expr list
      }
  | Match of
      { (* Unsure about this *)
        to_match : expr
      ; cases : match_case list
      }

and match_case =
  { (* Unsure about this *)
    case : expr
  ; consequence : expr
  }

type def =
  | Type_Def of
      { name : string
      ; to_group : type_case list
      }
  | Variable_Def of
      { ident : string
      ; init : expr
      }

and type_case =
  { constructor_name : string
  ; etype : etype
  }

type prog_node =
  | Def of def
  | Expr of expr

type prog = prog_node list
