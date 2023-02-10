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
  - Types Polymorphiques ?

# Notes

## Todo
- Crée du Sucre Syntaxique. # Plus Tard
- Parsing des types definis par l'utilisateur

# Lexing Tokens

## Separators

    { } [ ] ( ) ; : , * -> | = 

## Mots-Clefs

    let fun in match with type of

## Types

    int bool unit

## Operators

    + - % / & | ~ :: && || *


## Valeurs_Atomiques

    nombre := ('-')?['0'-'9']*
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

## Types

    Type    :=  | vartype
                | int
                | bool
                | unit
                | (Type)
                | Type * Type # Tuple_Type
                | ( Type list -> Type )  # Lambda_Type

## Expressions

    Litteral  :=    | nombre
                    | boolean
                    | ( ) # Unit
                    | [ ] # EmptyList

    Variable :=     | basic_ident
                    | basic_ident : Type

    VarArgs :=    | Variable VarArgs

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
                        
    # Operator are translated to simple calls to STDLib

    Expr    :=  | ( Expr )
                | Litteral
                | Variable
                | UnaryOperator Expr
                | Expr BinaryOperator Expr
                | constructeur_ident Expr # Built Expr
                | constructeur_ident Built Expr
                | Expr constructeur_infixes Expr 
                | Expr ; Expr # Sequence
                | [ Expr ] # List
                | let VarArgs = Expr in Expr # Binding
                | fun VarArgs -> Expr # Lambda
                | Expr Expr # Call
                | match Expr with Match_Case


## Filtrage et Motifs

    Match_Case  :=  | Pattern -> Expr
                    | Pattern -> Expr '|' Match_Case 

    Pattern :=      | Litteral
                    | basic_ident
                    | _
                    | constructeur_ident 
                    | constructeur_ident Pattern
                    | Pattern constructeur_infixes Pattern
                    | ( Pattern )


## Definitions

    Def     :=   | let VarArgs = Expr
                 | type = ident NewContructor_Case # Type Declaration

    NewContructor_Case :=   | constructeur_ident of Type
                            | constructor_ident
                            | NewContructor_Case '|' NewContructor_Case