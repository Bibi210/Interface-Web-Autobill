open Ast
open Lcbpv

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
  | LitteralPattern _ -> []
  | ConstructorPattern ptt ->
    (match ptt.content with
    | TuplePattern ptt -> List.map step ptt
    | v -> [ step v ])
  | _ -> failwith "DeepMatch Pattern Unhandled"
;;

let trans_litl = function
  | Integer i -> Expr_Int i
  | Boolean b -> Expr_Constructor (trans_boolean b, [])
  | Unit -> Expr_Constructor (Unit, [])
;;

let rec trans_expr e =
  ( (match e.enode with
    | Litteral l -> trans_litl l
    | Variable v -> Expr_Var (trans_var v)
    | Tuple tpl -> Expr_Constructor (Tuple, trans_expr_ls tpl)
    | Construct construct ->
      Expr_Constructor
        (Cons_Named construct.constructor_ident, [ trans_expr construct.to_group ])
    | Binding bind ->
      Expr_Block
        (Blk
           ( [ Ins_Let (trans_var bind.var, trans_expr bind.init), bind.var.vloc ]
           , trans_expr bind.content
           , e.eloc ))
    | Match mat -> Expr_Match (trans_expr mat.to_match, trans_match_case_ls mat.cases)
    (* How to translate calls *)
    | _ -> failwith "UnImplemented")
  , e.eloc )

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  match case.pattern with
  | LitteralPattern litt ->
    (match litt with
    | Integer x -> MatchPatTag (Int_Litt x, [], conseq, case.conseq_loc)
    | Boolean x -> MatchPatTag (trans_boolean x, [], conseq, case.conseq_loc)
    | Unit -> MatchPatTag (Unit, [], conseq, case.conseq_loc))
  | TuplePattern _ -> MatchPatTag (Tuple, getPatternVariable case, conseq, case.conseq_loc)
  | ConstructorPattern ptt ->
    MatchPatTag
      (Cons_Named ptt.constructor_ident, getPatternVariable case, conseq, case.conseq_loc)
  | VarPattern x -> MatchPatVar ((x, case.pattern_loc), conseq, case.conseq_loc)
  | WildcardPattern ->
    MatchPatVar ((Helpers.generate_name (), case.pattern_loc), conseq, case.conseq_loc)

and trans_match_case_ls ls = List.map trans_match_case ls
and trans_expr_ls ls = List.map trans_expr ls

let trans_def def (glbVarLs, progItemLs) =
  match def.dnode with
  | TypeDef newtype ->
    ( glbVarLs
    , Typ_Def
        ( newtype.basic_ident
        , List.map (fun elem -> elem, Pos) newtype.parameters
        , Def_Datatype [] (*!  Comment traduire les def de constructeurs ? *)
        , def.dloc )
      :: progItemLs )
  | VariableDef newglb ->
    ( (Ins_Let (trans_var newglb.var, trans_expr newglb.init), def.dloc) :: glbVarLs
    , progItemLs )
  | _ -> failwith ""
;;

let trans_prog_node node (glbVarLs, progItemLs) =
  match node with
  | Def d -> trans_def d (glbVarLs, progItemLs)
  | Expr e -> glbVarLs, Do (Blk ([], trans_expr e, e.eloc)) :: progItemLs
;;

let trans_prog p =
  let glbVar, progItemLs = List.fold_right trans_prog_node p ([], []) in
  Prog
    (progItemLs
    @ [ Do (Blk (glbVar, (Expr_Int 0, Helpers.dummy_pos), Helpers.dummy_pos)) ])
;;
