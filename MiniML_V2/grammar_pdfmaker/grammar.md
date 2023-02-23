---
title: MiniML Grammar Spec
output: 
  html_document: 
    pandoc_args: ["--lua-filter=color-text.lua"]
  pdf_document: 
    pandoc_args: ["--lua-filter=color-text.lua"]
    keep_tex: true
subtitle: 
author: Brahima,Yukai,Zaid
date: 2 fevrier, 2023
---
 <!-- pandoc --lua-filter ./grammar_pdfmaker/color-text.lua  -N --variable "geometry=margin=1.2in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=12pt --variable version=2.0 ./grammar_pdfmaker/grammar.md  --pdf-engine=xelatex --toc -o MiniMLSpec.pdf -->
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

    { } [ ] ( ) ; : , * -> | = 

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
    # For Type Inference
    Variable :=     | basic_ident
                    | basic_ident : Type

    Prog := | Def
            | Expr
            | Prog ;; Prog

## Definitions

    Def :=  | let Variable = Expr
            | let basic_ident Variable Variable list = Expr
            | let rec basic_ident Variable Variable list = Expr
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
                | Variable
                | UnaryOperator Expr
                | Expr BinaryOperator Expr
                | Expr Expr # Call
                | Expr ; Expr # Sequence
                | let Variable = Expr in Expr # Binding
                | fun Variable list -> Expr # Lambda
                | Expr constructeur_infixes Expr 
                | constructeur_ident Expr # Built Expr
                | constructeur_ident # Avoid Nil ()
                | let basic_ident Variable Variable list = Expr in Expr
                | let rec basic_ident Variable Variable list = Expr in Expr
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
                    | Pattern constructeur_infixes Pattern


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

# Traduction

## Programmes
    (PROG) si  ⊢ pi -> ω
        alors  ⊢ [pi] -> Prog(ω)

## Suites de commandes
    (DEFS) si d ∈ DEF, si ⊢ d -> ω et si ⊢ pi -> ω' 
        alors  ⊢ (Def(d), pi) -> (ω, ω')
    (BLOCK) si b ∈ BLOCK, si ⊢ b -> ω et si ⊢ pi -> ω'
        alors  ⊢ (Expr(b), pi) -> (Do(ω), ω')

## Définitions 
    (VALDEF) si ⊢ v -> v',si  ⊢ e -> e' et si ⊢ pi -> ω'
        alors  ⊢ (VariableDef(d), pi) -> (ω, ω')
    (FUNREC)
    (TYPDEF) si td ∈ CONSTR, si ⊢ td -> ω et si ⊢ pi -> ω'
        alors  ⊢ (TypeDef(n, [t1,...,tn], td), pi) 
                -> (Typ_Def(n, [t1,...,tn], ω), ω')
## Constructeurs
    (SYNON) si 
    (DATYP)
    (COMPUT)
*To be done*

## Litteraux et Expressions
    (INT) si i ∈ NUM 
        alors ⊢ Integer(i) -> Expr_Int(i)
    (TRUE) si ⊢ b -> true 
        alors ⊢ Boolean(b) -> Expr_Constructor(True,[])
    (FALSE) si ⊢ b -> false
        alors ⊢ Boolean(b) -> Expr_Constructor(False,[])
    (TUPLE) si ⊢ e1 -> e_1, ..., si ⊢ eN -> e_N
        alors ⊢ Tuple([e1,...,eN]) 
                -> Expr_Constructor(Tuple, [e_1,...,e_N])
    (CONSTR) si ⊢ e -> e' 
        alors ⊢ Construct((c,e)) 
                -> Expr_Constructor(Cons_Named c, [e'])
    (BIND) si ⊢ i -> i' et si ⊢ c -> c' 
        alors ⊢ Binding((v,i,c)) 
            -> Expr_Block(Blk([Ins_Let (v, i')], c'))
    (MATCH) si ⊢ m -> m', si m1 ∈ CASE,..., si mN ∈ CASE,
            si ⊢ m1 -> m_1, ... et si ⊢ mN -> m_N 
        alors ⊢ Match((m,[m1,...,mN])) 
            -> Expr_Match(m', [m_1,...,m_N])

## Motifs et Filtrage
    (LITPAT1) si l = Integer(l') et ⊢ e -> e'
        alors ⊢ Case(LitteralPattern(l), e) -> 
            -> MatchPat(Int_litt l', [], e')
    (LITPAT2) si l = Boolean(_), si ⊢ l -> l' et ⊢ e -> e'
        alors ⊢ Case(LitteralPattern(l), e) 
            -> MatchPat(l', [], e')
    (LITPAT3) si l = Unit et ⊢ e -> e'
        alors ⊢ Case(LitteralPattern(l), e)
            -> MatchPat(Unit, [], e')
    (TUPAT) si p1 ∈ CASE,..., si pN ∈ CASE,
            si ⊢ p1 -> p_1, ..., si ⊢ pN -> p_N et ⊢ e -> e'
        alors ⊢ Case(TuplePattern([p1,...,pN]), e)
            -> MatchPat(Tuple, [p_1,...,p_N], e')
    (CONSPAT)  si c ∈ CASE, si ⊢ c -> c' et ⊢ e -> e'
        alors ⊢ Case(ConstructorPattern((n,c)), e)
            -> MatchPat(Cons_Named(n), c', e')
    
