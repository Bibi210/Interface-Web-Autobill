type vartype = string

type pre_etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeTuple of etype list
  | TypeLambda of
      { args : etype list
      ; return_type : etype
      }
  | TypeVar of vartype
  | TypeConstructor of
      { to_build : etype
      ; parameters : etype list
      }

and etype =
  { etype : pre_etype
  ; tloc : HelpersML.position
  }

type variable =
  { basic_ident : string
  ; vloc : HelpersML.position
  }

and prog = prog_node list

and prog_node =
  | Def of def
  | Expr of expr

and def =
  { dnode : pre_def
  ; dloc : HelpersML.position
  }

and pre_def =
  | VariableDef of
      { var : variable
      ; init : expr
      }
  | FunctionRecDef of
      { var : variable
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
  ; c_of : etype list
  ; loc : HelpersML.position
  }

and litteral =
  | Integer of int
  | Boolean of bool
  | Unit

and expr =
  { enode : pre_expr
  ; eloc : HelpersML.position
  }

and callable =
  | ApplyExpr of expr
  | Add
  | Sub
  | Mult
  | Or
  | And
  | Div
  | Modulo
  | BitAnd

and pre_expr =
  | Litteral of litteral
  | Variable of variable
  | Call of
      { func : callable
      ; args : expr list
      }
  | Sequence of expr list
  | Binding of
      { var : variable
      ; init : expr
      ; content : expr
      }
  | Lambda of
      { args : variable list
      ; body : expr
      }
  | Tuple of expr list
  | Construct of
      { constructor_ident : string
      ; to_group : expr list
      }
  | FunctionRec of
      { var : variable
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
  ; conseq_loc : HelpersML.position
  ; pattern_loc : HelpersML.position
  }

and pattern =
  | LitteralPattern of litteral
  | VarPattern of string
  | WildcardPattern (* Ok *)
  | TuplePattern of pattern list (* Pas Profond *)
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern list (* Pas Profond *)
      }
