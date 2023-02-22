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