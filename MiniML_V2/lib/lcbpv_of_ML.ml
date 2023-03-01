open AstML
open Autobill.Lcbpv
open Autobill.Misc

let trans_boolean = function
  | true -> True
  | false -> False
;;

let trans_loc (x : HelpersML.position) =
  { start_pos = x.start_pos; end_pos = x.end_pos; is_dummy = x.is_dummy }
;;

let trans_var x = x.basic_ident, trans_loc x.vloc

let getPatternVariable case =
  let step = function
    | VarPattern x -> x, trans_loc case.pattern_loc
    | _ -> failwith "DeepMatch Pattern Unhandled : Pattern Containig Non Variable"
  in
  match case.pattern with
  | TuplePattern ptt -> List.map step ptt
  | ConstructorPattern ptt -> List.map step ptt.content
  | _ -> failwith "DeepMatch Pattern Unhandled"
;;

let trans_litl = function
  | Integer i -> Expr_Int i
  | Boolean b -> Expr_Constructor (trans_boolean b, [])
  | Unit -> Expr_Constructor (Unit, [])
;;

let rec trans_expr e =
  let e_loc = trans_loc e.eloc in
  ( (match e.enode with
    | Litteral l -> trans_litl l
    | Variable v -> Expr_Var (trans_var v)
    | Tuple tpl -> Expr_Constructor (Tuple, trans_expr_ls tpl)
    | Construct construct ->
      Expr_Constructor
        (Cons_Named construct.constructor_ident, trans_expr_ls construct.to_group)
    | Binding bind ->
      Expr_Block
        (Blk
           ( [ Ins_Let (trans_var bind.var, trans_expr bind.init), trans_loc bind.var.vloc
             ]
           , trans_expr bind.content
           , e_loc ))
    | Match mat -> Expr_Match (trans_expr mat.to_match, trans_match_case_ls mat.cases)
    (* How to translate calls *)
    | _ -> failwith "UnImplemented")
  , e_loc )

and trans_calls pos =
  (* TODO *)
  let fakeVarA = Expr_Var (HelpersML.generate_name (), pos), pos
  and fakeVarB = Expr_Var (HelpersML.generate_name (), pos), pos in
  let calc = Expr_Bin_Prim (Add, fakeVarA, fakeVarB), pos in
  function
  | Add -> Expr_Closure (Exp, calc), pos
  | _ -> failwith ""

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  let conseq_loc = trans_loc case.conseq_loc in
  match case.pattern with
  | LitteralPattern litt ->
    (match litt with
    | Integer x -> MatchPatTag (Int_Litt x, [], conseq, conseq_loc)
    | Boolean x -> MatchPatTag (trans_boolean x, [], conseq, conseq_loc)
    | Unit -> MatchPatTag (Unit, [], conseq, conseq_loc))
  | TuplePattern _ -> MatchPatTag (Tuple, getPatternVariable case, conseq, conseq_loc)
  | ConstructorPattern ptt ->
    MatchPatTag
      (Cons_Named ptt.constructor_ident, getPatternVariable case, conseq, conseq_loc)
  | VarPattern x -> MatchPatVar ((x, trans_loc case.pattern_loc), conseq, conseq_loc)
  | WildcardPattern ->
    MatchPatVar
      ((HelpersML.generate_name (), trans_loc case.pattern_loc), conseq, conseq_loc)

and trans_match_case_ls ls = List.map trans_match_case ls
and trans_expr_ls ls = List.map trans_expr ls

let rec trans_type t =
  ( (match t.etype with
    | TypeInt -> Typ_Int
    | TypeBool -> Typ_Bool
    | TypeUnit -> Typ_Unit
    | TypeTuple x ->
      Typ_App ((Typ_Tuple, trans_loc t.tloc), trans_type_ls x) (*! To check ?  *)
    | TypeVar vartype -> Typ_Var vartype
    | TypeConstructor x ->
      Typ_App (trans_type x.to_build, trans_type_ls x.parameters) (*! To check ?  *)
    | TypeLambda _ -> failwith "How ?")
  , trans_loc t.tloc )

and trans_type_ls ls = List.map trans_type ls

let rec trans_newconstructor_case case = case.constructor_ident, trans_type_ls case.c_of
and trans_newconstructor_case_ls ls = List.map trans_newconstructor_case ls

type temp =
  | NewTypeDef of program_item
  | NewGlobal of instruction

let trans_def def =
  let loc = trans_loc def.dloc in
  match def.dnode with
  | TypeDef newtype ->
    NewTypeDef
      (Typ_Def
         ( newtype.basic_ident
         , List.map (fun elem -> elem, Pos) newtype.parameters
         , Def_Datatype (trans_newconstructor_case_ls newtype.constructors)
         , loc ))
  | VariableDef newglb ->
    NewGlobal (Ins_Let (trans_var newglb.var, trans_expr newglb.init), loc)
  | _ -> failwith ""
;;

let trans_prog_node (glbvarls, program_items, last_expr) node =
  match node with
  | Def d ->
    (match trans_def d with
    | NewTypeDef newtype -> glbvarls, newtype :: program_items, last_expr
    | NewGlobal newglb -> newglb :: glbvarls, program_items, last_expr)
  | Expr e ->
    ( ( Ins_Let ((HelpersML.generate_name (), trans_loc e.eloc), trans_expr e)
      , trans_loc e.eloc )
      :: glbvarls
    , program_items
    , trans_expr e )
;;

let trans_prog p =
  let glbVar, progItemLs, last_expr =
    List.fold_left trans_prog_node ([], [], (Expr_Int 0, dummy_pos)) p
  in
  Prog (List.rev progItemLs @ [ Do (Blk (List.rev glbVar, last_expr, dummy_pos)) ])
;;
