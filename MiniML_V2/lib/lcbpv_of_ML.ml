open AstML
open Autobill.Lcbpv
open Autobill.Misc

let generate_variable pos = HelpersML.generate_name (), pos

let trans_boolean = function
  | true -> True
  | false -> False
;;

let trans_var x = x.basic_ident, x.vloc

let getPatternVariable case =
  let step = function
    | VarPattern x -> x, case.pattern_loc
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
  let e_loc = e.eloc in
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
           ( [ Ins_Let (trans_var bind.var, trans_expr bind.init), bind.var.vloc ]
           , trans_expr bind.content
           , e_loc ))
    | Match mat -> Expr_Match (trans_expr mat.to_match, trans_match_case_ls mat.cases)
    (* How to translate calls *)
    | Sequence expr_ls ->
      let last, rem = HelpersML.list_getlast_rem expr_ls in
      Expr_Block
        (Blk
           ( List.map
               (fun e -> Ins_Let (generate_variable e.eloc, trans_expr e), e.eloc)
               rem
           , trans_expr last
           , last.eloc ))
    | Call _ -> failwith "How ?"
    | Lambda _ -> failwith "How?"
    | FunctionRec _ -> failwith "How?")
  , e_loc )

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  let conseq_loc = case.conseq_loc in
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
  | VarPattern x -> MatchPatVar ((x, case.pattern_loc), conseq, conseq_loc)
  | WildcardPattern ->
    MatchPatVar ((HelpersML.generate_name (), case.pattern_loc), conseq, conseq_loc)

and trans_match_case_ls ls = List.map trans_match_case ls
and trans_expr_ls ls = List.map trans_expr ls

let rec trans_type t =
  ( (match t.etype with
    | TypeInt -> Typ_App ((Typ_Int, t.tloc), [])
    | TypeBool -> Typ_App ((Typ_Bool, t.tloc), [])
    | TypeUnit -> Typ_App ((Typ_Unit, t.tloc), [])
    | TypeTuple x -> Typ_App ((Typ_Tuple, t.tloc), trans_type_ls x)
    | TypeVar vartype -> Typ_Var (String.capitalize_ascii vartype)
    | TypeConstructor x -> Typ_App (trans_type x.to_build, trans_type_ls x.parameters)
    | TypeLambda _ -> failwith "How ?")
    (* TODO *)
  , t.tloc )

and trans_type_ls ls = List.map trans_type ls

let rec trans_newconstructor_case case = case.constructor_ident, trans_type_ls case.c_of
and trans_newconstructor_case_ls ls = List.map trans_newconstructor_case ls

type temp =
  | NewTypeDef of program_item
  | NewGlobal of instruction

let trans_def def =
  let loc = def.dloc in
  match def.dnode with
  | TypeDef newtype ->
    NewTypeDef
      (Typ_Def
         ( String.capitalize_ascii newtype.basic_ident
         , List.map (fun elem -> String.capitalize_ascii elem, Pos) newtype.parameters
         , Def_Datatype (trans_newconstructor_case_ls newtype.constructors)
         , loc ))
  | VariableDef newglb ->
    NewGlobal (Ins_Let (trans_var newglb.var, trans_expr newglb.init), loc)
  | FunctionRecDef _ -> failwith "How ?" (* TODO *)
;;

let trans_prog_node (glbvarls, program_items, last_expr) node =
  match node with
  | Def d ->
    (match trans_def d with
    | NewTypeDef newtype -> glbvarls, newtype :: program_items, last_expr
    | NewGlobal newglb -> newglb :: glbvarls, program_items, last_expr)
  | Expr e ->
    let newVarName = HelpersML.generate_name () in
    let varloc = e.eloc in
    ( (Ins_Let ((newVarName, varloc), trans_expr e), varloc) :: glbvarls
    , program_items
    , (Expr_Var (newVarName, varloc), varloc) )
;;

let trans_prog p =
  let glbVar, progItemLs, last_expr =
    List.fold_left trans_prog_node ([], [], (Expr_Int 0, dummy_pos)) p
  in
  Prog (List.rev progItemLs @ [ Do (Blk (List.rev glbVar, last_expr, dummy_pos)) ])
;;
