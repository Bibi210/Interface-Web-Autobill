open AstML
open Autobill.Lcbpv
open Autobill.Misc

exception Error of string

let generate_variable pos = HelpersML.generate_name (), pos

let trans_boolean = function
  | true -> True
  | false -> False
;;

let trans_var x = x.basic_ident, x.vloc
let trans_var_ls = List.map trans_var

let trans_litl = function
  | Integer i -> Expr_Int i
  | Boolean b -> Expr_Constructor (trans_boolean b, [])
  | Unit -> Expr_Constructor (Unit, [])
;;

let rec make_unary_closure op loc =
  let arg = { basic_ident = HelpersML.generate_name (); vloc = loc } in
  let expr_var = Some { enode = Variable arg; eloc = loc } in
  let e, _ =
    trans_expr
      (HelpersML.func_curryfy
         [ arg ]
         { enode = CallUnary { arg = expr_var; op }; eloc = loc })
  in
  e

and make_binary_closure args op loc =
  let arg1 = { basic_ident = HelpersML.generate_name (); vloc = loc } in
  let expr_var1 = { enode = Variable arg1; eloc = loc } in
  let closure, _ =
    trans_expr
      (match args with
      | [] ->
        let arg2 = { basic_ident = HelpersML.generate_name (); vloc = loc } in
        let expr_var2 = { enode = Variable arg2; eloc = loc } in
        HelpersML.func_curryfy
          [ arg1; arg2 ]
          { enode = CallBinary { args = [ expr_var1; expr_var2 ]; op }; eloc = loc }
      | [ hd ] ->
        HelpersML.func_curryfy
          [ arg1 ]
          { enode = CallBinary { args = [ expr_var1; hd ]; op }; eloc = loc }
      | _ ->
        HelpersML.err "Unexpected number of arguments on binary closure" loc.start_pos)
  in
  closure

and trans_expr e =
  let e_loc = e.eloc in
  ( (match e.enode with
    | Litteral l -> trans_litl l
    | Variable v -> Expr_Var (trans_var v)
    | Tuple tpl -> Expr_Constructor (Tuple, trans_expr_ls tpl)
    | CallUnary { op; arg = Some arg } -> Expr_Mon_Prim (op, trans_expr arg)
    | CallUnary { op; arg = None } -> make_unary_closure op e_loc
    | CallBinary { op; args = first :: second :: _ } ->
      Expr_Bin_Prim (op, trans_expr first, trans_expr second)
    | CallBinary { op; args } -> make_binary_closure args op e_loc
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
    | Sequence expr_ls ->
      let last, rem = HelpersML.list_getlast_rem expr_ls in
      Expr_Block
        (Blk
           ( List.map
               (fun e -> Ins_Let (generate_variable e.eloc, trans_expr e), e.eloc)
               rem
           , trans_expr last
           , last.eloc ))
    | Call { func; arg } ->
      let openvar = generate_variable e_loc in
      let returnvar = generate_variable e_loc in
      let call =
        Expr_Method ((Expr_Var openvar, e_loc), (Call, e_loc), [ trans_expr arg ]), e_loc
      in
      Expr_Block
        (Blk
           ( [ Ins_Open (openvar, Exp, trans_expr func), func.eloc
             ; Ins_Force (returnvar, call), func.eloc
             ]
           , (Expr_Var returnvar, e_loc)
           , e_loc ))
    | Lambda { arg; body } ->
      Expr_Closure
        ( Exp
        , ( Expr_Get
              [ GetPatTag
                  ( (Call, e_loc)
                  , [ trans_var arg ]
                  , (Expr_Thunk (trans_expr body), body.eloc)
                  , body.eloc )
              ]
          , e_loc ) )
          | FunctionRec { var; body } ->
            (match body.enode with
            | Lambda { arg; body } ->
              Expr_Closure
                ( Exp
                , ( Expr_Rec
                      ( trans_var var
                      , ( Expr_Get
                            [ GetPatTag
                                ( (Call, e_loc)
                                , [ trans_var arg ]
                                , (Expr_Thunk (trans_expr body), body.eloc)
                                , body.eloc )
                            ]
                        , e_loc ) )
                  , e_loc ) )
            | _ -> failwith ""))
        , e_loc )

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  let conseq_loc = case.consequence.eloc in
  let ptt_loc = case.pattern.ploc in
  match case.pattern.pnode with
  | LitteralPattern litt ->
    (match litt with
    | Integer x -> MatchPatTag (Int_Litt x, [], conseq, conseq_loc)
    | Boolean x -> MatchPatTag (trans_boolean x, [], conseq, conseq_loc)
    | Unit -> MatchPatTag (Unit, [], conseq, conseq_loc))
  | TuplePattern _ -> MatchPatTag (Tuple, getPatternVariable case, conseq, conseq_loc)
  | ConstructorPattern ptt ->
    MatchPatTag
      (Cons_Named ptt.constructor_ident, getPatternVariable case, conseq, conseq_loc)
  | VarPattern x -> MatchPatVar ((x, ptt_loc), conseq, conseq_loc)
  | WildcardPattern ->
    MatchPatVar ((HelpersML.generate_name (), ptt_loc), conseq, conseq_loc)

and getPatternVariable case =
  let step pt =
    match pt.pnode with
    | VarPattern x -> x, pt.ploc
    | _ ->
      HelpersML.err
        "DeepMatch Pattern Unhandled : Pattern Containig Non Variable"
        pt.ploc.start_pos
  in
  match case.pattern.pnode with
  | TuplePattern ptt -> List.map step ptt
  | ConstructorPattern ptt -> List.map step ptt.content
  | _ -> HelpersML.err "DeepMatch Pattern Unhandled" case.pattern.ploc.start_pos

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
    | TypeLambda { arg; return_type } ->
      Typ_App
        ( (Typ_Closure Exp, t.tloc)
        , [ ( Typ_App
                ( (Typ_Fun, t.tloc)
                , ( Typ_App ((Typ_Thunk, return_type.tloc), [ trans_type return_type ])
                  , return_type.tloc )
                  :: [ trans_type arg ] )
            , t.tloc )
          ] ))
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
