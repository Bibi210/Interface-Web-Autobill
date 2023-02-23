(* TODO Sementique de traduction *)

type vartype = string

type etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeTuple of etype list
  | TypeLambda of
      { args : etype list
      ; return_type : etype
      }
  | TypeVar of vartype
  | TypeConstructor of etype list

type variable =
  { basic_ident : string
  ; expected_type : etype option
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
      { (* TODO *)
        var : variable
      ; init : expr
      }
  | FunctionRecDef of
      { basic_ident : string
      ; args : variable list
      ; body : expr
      }
  | TypeDef of
      { (* TODO *)
        basic_ident : string
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
  { etype : etype option
  ; enode : pre_expr
  ; eloc : Helpers.position
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
      ; to_group : expr
      }
  | FunctionRec of
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
  | WildcardPattern (* Ok *)
  | TuplePattern of pattern list (* Pas Profond *)
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern (* Pas Profond *)
      }
