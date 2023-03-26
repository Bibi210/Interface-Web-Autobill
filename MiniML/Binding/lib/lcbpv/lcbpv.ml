type sort = Pos | Neg

type type_variable = string

type type_constructor_name = string

type qualifier = Exp | Lin | Aff

type typ =
  | Typ_Var of type_variable
  | Typ_App of typ * typ list
  | Typ_Int
  | Typ_Bool
  | Typ_Unit
  | Typ_Zero
  | Typ_Top
  | Typ_Bottom
  | Typ_Tuple
  | Typ_Sum
  | Typ_Fun
  | Typ_LazyPair
  | Typ_Closure of qualifier
  | Typ_Thunk


type variable = string

type constructor_name = string

type constructor =
  | Cons_Named of constructor_name
  | Unit
  | True
  | False
  | Int_Litt of int
  | Tuple
  | Inj of int * int

type method_name = string

type methodd =
  | Method_Named of method_name
  | Call
  | Proj of int * int

type value =
  | Val_Var of variable
  | Val_Int of int
  | Val_Constructor of constructor * value list
  | Val_Closure of qualifier * expression
  | Val_Thunk of expression
  | Val_Get of get_pattern list

and expression =
  | Expr_Var of variable
  | Expr_Int of int
  | Expr_Constructor of constructor * expression list
  | Expr_Closure of qualifier * expression
  | Expr_Thunk of expression
  | Expr_Get of get_pattern list
  | Expr_Block of block
  | Expr_Method of expression * methodd * expression list
  | Expr_Match of expression * match_pattern list
  | Expr_Rec of variable * expression
  | Expr_Bin_Prim of prim_bin_op * expression * expression
  | Expr_Mon_Prim of prim_mon_op * expression
  | Expr_If of expression * expression * expression

and prim_mon_op = Opp | Not

and prim_bin_op =
  | Add | Mult | Subs | Div | Mod
  | And | Or
  | Int_Eq
  | Int_Leq
  | Int_Lt

and block = Blk of instruction list * expression

and instruction =
  | Ins_Let of variable * expression
  | Ins_Force of variable * expression
  | Ins_Open of variable * qualifier * expression

and get_pattern = GetPat of methodd * variable list * expression

and match_pattern = MatchPat of constructor * variable list * expression


type program = Prog of program_item list

and program_item =
  | Typ_Decl of variable * sort list * sort
  | Value_Decl of variable * typ
  | Typ_Def of type_constructor_name * (type_variable * sort) list * type_definition_content
  | Do of block

and type_definition_content =
  | Def_Synonym of typ * sort
  | Def_Datatype of (constructor_name * typ list) list
  | Def_Computation of (method_name * typ list * typ) list
