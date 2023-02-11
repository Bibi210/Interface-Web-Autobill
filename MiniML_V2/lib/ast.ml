type vartype = string

type variable =
  { basic_ident : string
  ; expected_type : etype
  }

and prog = prog_node list

and prog_node =
  | Def of def
  | Expr of expr

and def =
  { dnode : pre_def
  ; dloc : Helpers.position
  }

and pre_def =
  | VariableDef of
      { var : variable
      ; init : expr
      }
  | FunctionDef of
      { basic_ident : string
      ; args : variable list
      ; body : expr
      }
  | FunctionRecDef of
      { basic_ident : string
      ; args : variable list
      ; body : expr
      }
  | TypeDef of
      { basic_ident : string
      ; parameters : vartype list
      ; constructors : newconstructor_case list
      }

and newconstructor_case =
  { constructor_ident : string
  ; c_of : etype
  ; loc : Helpers.position
  }

and litteral =
  | Integer of int
  | Boolean of bool
  | Unit

and expr =
  { etype : etype
  ; enode : pre_expr
  ; eloc : Helpers.position
  }

and pre_expr =
  | Litteral of litteral
  | Variable of string
  | Call of
      { func : expr
      ; arg : expr
      }
  | Sequence of expr list
  | Binding of
      { var : variable
      ; init : expr
      ; content : expr
      }
  | Lambda of
      { arg : variable
      ; body : expr
      }
  | Tuple of
      { first : expr
      ; second : expr
      }
  | Construct of
      { constructor_name : string
      ; to_group : expr
      }
  | FunctionRec of
      { basic_ident : string
      ; args : variable list
      ; body : expr
      }
  | Function of
      { basic_ident : string
      ; args : variable list
      ; body : expr
      }
  | Match of
      { to_match : expr
      ; cases : match_case list
      }

and match_case =
  { pattern : pattern
  ; consequence : expr
  ; cloc : Helpers.position
  }

and pattern =
  | LitteralPattern of litteral
  | VarPattern of string
  | WildcardPattern
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern list
      }

and etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeTuple of
      { first : etype
      ; second : etype
      }
  | TypeLambda of
      { arg : etype
      ; return_type : etype
      }
  | TypeVar of vartype
  | TypeConstructor of etype list
