type vartype = string

type pre_etype =
  | TypeInt
  | TypeBool
  | TypeUnit
  | TypeTuple of etype list
  | TypeLambda of
      { arg : etype
      ; return_type : etype
      }
  | TypeVar of vartype
  | TypeConstructor of
      { to_build : etype
      ; parameters : etype list
      }

and etype =
  { etype : pre_etype
  ; tloc : Autobill.Misc.position
  }

type variable =
  { basic_ident : string
  ; vloc : Autobill.Misc.position
  }

and prog = prog_node list

and prog_node =
  | Def of def
  | Expr of expr

and def =
  { dnode : pre_def
  ; dloc : Autobill.Misc.position
  }

and pre_def =
  | VariableDef of
      { var : variable
      ; init : expr
      }
  | TypeDef of
      { basic_ident : string
      ; parameters : vartype list
      ; constructors : newconstructor_case list
      }

and newconstructor_case =
  { constructor_ident : string
  ; c_of : etype list
  ; loc : Autobill.Misc.position
  }

and litteral =
  | Integer of int
  | Boolean of bool
  | Unit

and expr =
  { enode : pre_expr
  ; eloc : Autobill.Misc.position
  }

and pre_expr =
  | Litteral of litteral
  | Variable of variable
  | CallUnary of
      { op : Autobill.Lcbpv.prim_mon_op
      ; arg : expr option
      }
  | CallBinary of
      { op : Autobill.Lcbpv.prim_bin_op
      ; args : expr list
      }
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
  | Tuple of expr list
  | Construct of
      { constructor_ident : string
      ; to_group : expr list
      }
  | FunctionRec of
      { var : variable
      ; body : expr
      }
  | Match of
      { to_match : expr
      ; cases : match_case list
      }

and match_case =
  { pattern : pattern
  ; consequence : expr
  ; cloc : Autobill.Misc.position
  }

and pattern =
  { pnode : pre_pattern
  ; ploc : Autobill.Misc.position
  }

and pre_pattern =
  | LitteralPattern of litteral
  | VarPattern of string
  | WildcardPattern (* Ok *)
  | TuplePattern of pattern list (* Pas Profond *)
  | ConstructorPattern of
      { constructor_ident : string
      ; content : pattern list (* Pas Profond *)
      }
