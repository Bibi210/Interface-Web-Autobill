open Ast
open Lcbpv

let trans_boolean = function
  | true -> True
  | false -> False
;;

let rec getPatternVariable =
  let step = function
    | VarPattern x -> x
    | _ -> failwith "DeepMatch Pattern Unhandled : Pattern Containig Non Variable"
  in
  function
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
  match e.enode with
  | Litteral l -> trans_litl l
  | Variable v -> Expr_Var v.basic_ident
  | Tuple tpl -> Expr_Constructor (Tuple, trans_expr_ls tpl)
  | Construct construct ->
    Expr_Constructor
      (Cons_Named construct.constructor_ident, [ trans_expr construct.to_group ])
  | Binding bind ->
    Expr_Block
      (Blk
         ( [ Ins_Let (bind.var.basic_ident, trans_expr bind.init) ]
         , trans_expr bind.content ))
  | Match mat -> Expr_Match (trans_expr mat.to_match, trans_match_case_ls mat.cases)
  (* How to translate calls *)
  | _ -> failwith "UnImplemented"

and trans_match_case case =
  let conseq = trans_expr case.consequence in
  match case.pattern with
  | LitteralPattern litt ->
    (match litt with
    | Integer x -> MatchPat (Int_Litt x, [], conseq)
    | Boolean x -> MatchPat (trans_boolean x, [], conseq)
    | Unit -> MatchPat (Unit, [], conseq))
  | TuplePattern _ -> MatchPat (Tuple, getPatternVariable case.pattern, conseq)
  | ConstructorPattern ptt ->
    MatchPat (Cons_Named ptt.constructor_ident, getPatternVariable case.pattern, conseq)
  | VarPattern _ -> failwith "How To Translate VarPattern?" (* TODO *)
  | WildcardPattern -> failwith "How To Translate ? Wildcard" (* TODO *)

and trans_match_case_ls ls = List.map trans_match_case ls
and trans_expr_ls ls = List.map trans_expr ls

(* let trans_def def =
  match def.dnode with
  | TypeDef t ->
    Typ_Def (t.basic_ident, List.map (fun x -> x, Pos) t.parameters, Def_Datatype)
;; *)
