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

# Notes

## Todo
- Crée du Sucre Syntaxique. # Plus Tard

# Tokens

## Symbols

## Separators

    { } [ ] ( ) ; : , * -> | = 

## Mots-Clefs

    let rec fun in match with type of if then else

## Valeurs_Atomiques

    nombre := ('-')?['0'-'9']*
    boolean := ("true"|"false")

## Identificateur
    alphanum := ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    basic_ident := ['a'-'z' '_'] alphanum
    constructeur_ident := ['A'-'Z'] alphanum
\pagebreak
# Grammaire

    Prog := | Def
            | Expr
            | Prog ;; Prog

## Variables

    Variable := | basic_ident
                | basic_ident : Type
    Variables:= | Variable
                | Variable Variables
## Types

    Type    :=  | basic_ident
                | ( ) # Unit
                | [ ] # EmptyList_Type
                | ( Type_Ls ) # Tuple_Type
                | [ Type ] # List_Type
                | ( Types -> Type ) # Lambda_Type

    Types   :=  | Type
                | Type Types
    Type_Ls :=  | Type
                | Type , Type_Ls
## Expressions

    Litteral   :=  | nombre
                   | boolean

    Expr    :=  | ( Expr )
                | Litteral
                | Variable
                | constructeur_ident Expr # Custom Expr
                | ( ) # Unit
                | [ ] # EmptyList
                | ( Exprs_Ls ) # Tuple
                | [ Exprs_Ls ] # List
                | ( Exprs_Seq ) # Sequence
                | let Variable = Expr in Expr # Binding
                | fun Variables -> Expr # Lambda
                | if Expr then Expr else Expr # Condition
                | Expr (Exprs_Arg) # Call
                | match Expr with Match_Case


    Match_Case  :=  | Patt -> Expr
                    | Match_Case '|' Match_Case 

    Patt :=     | Litteral
                | constructor_ident '(' Basic_Ident_LS ')'
                | ( )
                | ( Basic_Ident_LS )

    Basic_Ident_LS :=   | basic_ident
                        | basic_ident , Basic_Ident_LS

    Exprs_Arg := | Expr
                 | Expr Exprs_Arg
    Exprs_Ls  := | Expr
                 | Expr , Exprs_Ls
    Exprs_Seq := | Expr ; Exprs_Seq

## Definitions

    Def     :=   | let Variable = Expr
                 | type = ident NewContructor_Case # Type Declaration

    NewContructor_Case :=   | constructeur_ident of Type
                            | NewContructor '|' NewContructor_Case