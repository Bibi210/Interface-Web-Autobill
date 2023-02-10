type etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeLambda of
      { args_types : etype list
      ; return_type : etype
      }
  | TypeTuple of
      { first : etype
      ; second : etype
      }
  | TypeCustom of string

type literal =
  | Integer of int
  | Boolean of bool
  | Unit

type patt =
  | LitteralPattern of literal
  | VarPattern of string
  | WildcardPattern
  | ConstructorPattern of
      { constructor_name : string
      ; content : patt list
      }

type expr =
  { etype : etype option
  ; node : pre_expr
  ; loc : Helpers.position
  }

and pre_expr =
  | Litteral of literal
  | Tuple of
      { first : expr
      ; second : expr
      }
  | Binding of
      { ident : string
      ; init : expr
      ; content : expr
      }
  | Variable of string
  | Sequence of expr list
  | Lambda of
      { args : string list
      ; body : expr
      }
  | Call of
      { func : expr
      ; args : expr list
      }
  | Construct of
      { constructor_name : string
      ; to_group : expr list
      }
  | Match of
      { to_match : expr
      ; cases : match_case list
      }

and match_case =
  { pattern : patt
  ; consequence : expr
  }

type def =
  | TypeDef of
      { name : string
      ; constructors : def_constructor list
      }
  | VariableDef of
      { ident : string
      ; init : expr
      }

and def_constructor =
  { name : string
  ; etype : etype list
  }

type prog_node =
  | Def of def
  | Expr of expr

type prog = prog_node list
