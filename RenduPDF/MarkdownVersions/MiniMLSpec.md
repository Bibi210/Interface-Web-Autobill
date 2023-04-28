---
title: MiniML Grammar Spec
output: 
  html_document: 
    pandoc_args: ["--lua-filter=color-text.lua"]
  pdf_document: 
    pandoc_args: ["--lua-filter=color-text.lua"]
    keep_tex: true
subtitle: 
author:
  - Fazazi Zeid
  - Luo Yukai 
  - Brahima Dibassi
date: 2 fevrier, 2023
---
 <!--pandoc --lua-filter ./MarkdownVersions/color-text.lua  -N --variable "geometry=margin=1.2in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=12pt --variable version=2.0 ./MarkdownVersions/MiniMLSpec.md  --pdf-engine=xelatex --toc -o MiniMLSpec.pdf -->
\newpage


# Change Log

- 2 fevrier, 2023 Première Version
- 2 fevrier, 2023 Première Correction
  - Ajout de Unit
  - Ajout des patterns
  - Rename Value -> Litteral
  - Retrait Operators/Type de Base
  - Retrait Sucre Syntaxique pour le moment
  
- 7 fevrier, 2023 Deuxième Correction
  - Simplification (des _LS)
  - Ajout des constructeurs infixes 
  - Fix des Match Patterns
  - Fix Definition Globales
  - Reintroduction du Parsing Operators/Type de Base
-  11 fevrier, 2023 Post Reunion
   -  Ajout et compréhension des vartypes
   -  Ajout du keyword rec
   -  Ajout des types parametrer 

# Notes

## Todo
- Crée du Sucre Syntaxique. # Plus Tard


\pagebreak
# Lexing Tokens

## Separators

    { } [ ] ( ) ; , * -> | = 

## Mots-Clefs

    let fun in match with type of rec

## Types

    int bool unit

## Operators

    + - % / & | ~ :: && || *


## Valeurs_Atomiques

    integer := ('-')?['0'-'9']*
    boolean := ("true"|"false")

## Identificateur

    alphanum := ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    basic_ident := ['a'-'z' '_'] alphanum
    vartype  := [’`t’][0..9]*

### Constructeurs

    constructeur_ident := ['A'-'Z'] alphanum
    constructeur_infixes := ["::" ',']

\pagebreak
# Grammaire
    Prog := | Def
            | Expr
            | Prog ;; Prog

## Definitions

    Def :=  | let basic_ident = Expr
            | let basic_ident list = Expr
            | let rec basic_ident list = Expr
            | type vartype list basic_ident =  NewContructor_Case  #TypeDef

    NewContructor_Case :=   | constructeur_ident
                            | constructeur_ident of Type
                            | NewContructor_Case '|' NewContructor_Case

## Expressions

    Litteral  :=    | integer
                    | boolean
                    | ( ) # Unit

    Expr    :=  | ( Expr )
                | Litteral
                | basic_ident
                | UnaryOperator Expr
                | Expr BinaryOperator Expr
                | Expr Expr # Call
                | Expr ; Expr # Sequence
                | let basic_ident = Expr in Expr # Binding
                | fun basic_ident list -> Expr # Lambda
                | Expr constructeur_infixes Expr 
                | constructeur_ident Expr # Built Expr
                | constructeur_ident # Avoid Nil ()
                | let basic_ident list = Expr in Expr
                | let rec basic_ident basic_ident list = Expr in Expr
                | match Expr with Match_Case

    UnaryOperator :=    | ~
                        | -

    BinaryOperator :=   | &
                        | &&
                        | ||
                        | +
                        | -
                        | /
                        | %
                        | *     

## Filtrage et Motifs

    Match_Case  :=  | Pattern -> Expr
                    | Pattern -> Expr '|' Match_Case 

    Pattern :=      | ( Pattern )
                    | Litteral
                    | basic_ident
                    | _
                    | constructeur_ident 
                    | constructeur_ident Pattern

## Types
    Type    :=  | (Type)
                | int
                | bool
                | unit
                | Type * Type # Tuple_Type
                | Type -> Type  # Lambda_Type
                | vartype # 'a
                | basic_ident # defined type
                | Type List  # Parametred Type (EXEMPLE : int list option)

\pagebreak
# Traduction

## Programmes
    (PROG) si  ꜔ pi -> ω
        alors  ꜔ [pi] -> Prog(ω)

## Suites de commandes
    (DEFS) si d ∈ DEF, si ꜔ d -> ω et si ꜔ pi -> ω' 
        alors  ꜔ (Def(d), pi) -> (ω, ω')
    (BLOCK) si b ∈ BLOCK, si ꜔ b -> ω et si ꜔ pi -> ω'
        alors  ꜔ (Expr(b), pi) -> (Do(ω), ω')


## Définitions 
    (VARDEF) si ꜔ v -> v' et si  ꜔ e -> e' 
        alors  ꜔ VariableDef(v, e) -> NewGlobal(Ins_Let(v', e'))
    (TYPDEF) si ꜔ c1 -> c1', ..., si ꜔ cN -> cN'
        alors  ꜔ TypeDef(n, [t1,...,tn], cst)
    -> NewTypeDef(Typ_Def(n, [t1,...,tn], Def_Datatype([c1,...,cN])))

## Types
    (TINT) ꜔ TypeInt -> Typ_App ((Typ_Int), [])
    (TBOOL) ꜔ TypeBool -> Typ_App ((Typ_Bool), [])
    (TUNIT) ꜔ TypeUnit -> Typ_App ((Typ_Unit), [])
    (TTUPLE) si ꜔ t1 -> t_1,... et ꜔ tN -> t_N
    alors ꜔ TypeTuple([t1,...tN]) -> Typ_App((Typ_Tuple, t.tloc), [t_1,...,t_N])
    (TDEF) ꜔ TypeDefined(id) -> Typ_Var(id)
    (TVAR) ꜔ TypeVar(id) -> Typ_Var (id)
    (TCONS) si ꜔ t -> t', si ꜔ p1 -> p_1,... et si ꜔ pN -> p_N  
    alors ꜔ TypeConstructor(t, [p1,...,pN]) -> Typ_App(t, [p_1,...,p_N])
    (TLAMB) si ꜔ a -> a' et ꜔ ret -> ret'
        alors ꜔ TypeLambda(a,ret) 
    -> Typ_App(Typ_Closure(Exp),[(Typ_App(Typ_Fun, [Typ_App(Typ_Thunk, [ret']), a']))])


## Litteraux et Expressions
    (INT) si i ∈ NUM 
        alors ꜔ Integer(i) -> Expr_Int(i)
    (TRUE) si ꜔ b -> true 
        alors ꜔ Boolean(b) -> Expr_Constructor(True,[])
    (FALSE) si ꜔ b -> false
        alors ꜔ Boolean(b) -> Expr_Constructor(False, [])
    (UNIT) ꜔ Unit -> Expr_Constructor(Unit, [])    
    (TUPLE) si ꜔ e1 -> e_1, ..., si ꜔ eN -> e_N
        alors ꜔ Tuple([e1,...,eN]) -> Expr_Constructor(Tuple, [e_1,...,e_N])
    (UNARY) si ꜔ a -> a_1 
        alors ꜔ CallUnary(op, a) -> Expr_Mon_Prim(op, a_1)
    (BINARY) si ꜔ a1 -> a_1 et ꜔ a2 -> a_2
        alors ꜔ CallBinary(op, [a1,a2]) -> Expr_Bin_Prim(op, a_1, a_2) 
    (CONSTR) si ꜔ e -> e' 
        alors ꜔ Construct(c,e) -> Expr_Constructor(Cons_Named(c), e')
    (BIND) si ꜔ i -> i' et si ꜔ c -> c' 
        alors ꜔ Binding(v,i,c) -> Expr_Block(Blk([Ins_Let (v, i')], c'))
    (MATCH) si ꜔ m -> m', si m1 ∈ CASE,..., si mN ∈ CASE, si ꜔ m1 -> m_1, ... et si ꜔ mN -> m_N alors ꜔ Match((m,[m1,...,mN])) -> Expr_Match(m', [m_1,...,m_N])
    (SEQ) si ꜔ e1 -> Ins_Let(x1, e_1), ..., si ꜔ eN-1 -> Ins_Let(xN-1, e_N-1)
        et si ꜔ eN -> e_N, alors ꜔ Sequence([e1,...,eN]) 
            -> Expr_Block(Blk([Ins_Let(x1, e_1),..., Ins_Let(xN-1, e_N-1)], e_N))
    (CALL) si ꜔ a -> a' et si ꜔ f -> f' 
        alors Call(f, a) 
    -> Expr_Block(Blk([Ins_Open(Exp, f'), Ins_Force(Expr_Method(Call, [a']))]))
    (LAMBDA) si ꜔ a -> a' et si ꜔ b -> b'
        alors ꜔ Lambda(a, b)
        -> Expr_Closure(Exp, Expr_Get([GetPatTag(Call, [a'], Expr_Thunk(b'))]))
    (REC) si ꜔ a -> a', si ꜔ v -> v' et si ꜔ b -> b'
        alors ꜔ FunctionRec(v, a, b)
    -> Expr_Closure(Exp,Expr_Rec(v',Expr_Get([GetPatTag(Call, [a'], Expr_Thunk(b'))])))

## Motifs et Filtrage
    (LITPAT1) si l = Integer(l') et ꜔ e -> e'
        alors ꜔ Case(LitteralPattern(l), e) -> 
            -> MatchPatTag(Int_litt l', [], e')
    (LITPAT2) si l = Boolean(_), si ꜔ l -> l' et ꜔ e -> e'
        alors ꜔ Case(LitteralPattern(l), e) 
            -> MatchPatTag(l', [], e')
    (LITPAT3) si l = Unit et ꜔ e -> e'
        alors ꜔ Case(LitteralPattern(l), e)
            -> MatchPatTag(Unit, [], e')
    (TUPAT) si p1 ∈ CASE,..., si pN ∈ CASE,
            si ꜔ p1 -> p_1, ..., si ꜔ pN -> p_N et ꜔ e -> e'
        alors ꜔ Case(TuplePattern([p1,...,pN]), e)
            -> MatchPatTag(Tuple, [p_1,...,p_N], e')
    (CONSPAT)  si c ∈ CASE, si ꜔ c -> c' et ꜔ e -> e'
        alors ꜔ Case(ConstructorPattern((n,c)), e)
            -> MatchPatTag(Cons_Named(n), c', e')
    (VARPAT) si ꜔ e -> e'
        alors ꜔ Case(VarPattern(x), e, l)
            -> MatchPatVar((x, l), e', l)
    (WILDPAT) si ꜔ e -> e' 
        alors ꜔ Case(WildcardPattern(), e, l)
            -> MatchPatVar((n, l), e', l)
    
