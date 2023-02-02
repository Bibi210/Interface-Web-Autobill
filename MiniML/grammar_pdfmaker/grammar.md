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

# Notes

## Todo
- Fix Match_Case
- Fix NewContructor_Case

# Tokens

## Symbols

## Separators

    { } [ ] ( ) ; : , * -> | = 

## Operators

    + * - / & |

## Mots-Clefs

    let rec fun in match with type of if then else

## Types

    int bool

## Valeurs_Atomiques

    nombre := ('-')?['0'-'9']*
    boolean := ("true"|"false")

## Identificateur
    alphanum := ['a'-'z' 'A'-'Z' '0'-'9' '_']*
    basic_ident := ['a'-'z' '_'] alphanum
    constructeur_ident := ['A'-'Z'] alphanum

# Grammaire

    Prog := | Def
            | Expr
            | Prog ;; Prog

## Variables

    Variable := | basic_ident
                | Variable : Type
    Variables:= | Variable
                | Variable Variables
## Types

    Type    :=  | int
                | bool
                | (Types -> Type) # Lambda_Type
                | ( -> Type) # Lambda_Type
                | ( Type_Ls ) # Tuple_Type
                | [ Type ] # List_Type
                | [ ] # EmptyList_Type ?

    Types   :=  | Type
                | Type Types
    Type_Ls :=  | Type
                | Type , Type_Ls
## Expressions

    Value   :=  | nombre
                | boolean

    Expr    :=  | ( Expr )
                | Value
                | Variable
                | constructeur_ident Expr # Custom Expr
                | ( Expr , Exprs_Ls ) # Tuple
                | [ Exprs_Ls ] # List
                | [ ] # EmptyList
                | ( Exprs_Seq ) # Sequence
                | let Variable = Expr in Expr # Binding
                | fun Variables -> Expr # Lambda
                | let Variables = Expr in Expr # LambdaBinding (Sugar)
                | if Expr then Expr else Expr # Condition
                | Expr (Exprs_Arg) # Call
                | match Expr with Match_Case

    Match_Case := | Expr -> Expr 
                  | Match_Case '|' Match_Case

    Exprs_Arg := | Expr
                 | Expr Exprs_Arg
    Exprs_Ls  := | Expr
                 | Expr , Exprs_Ls
    Exprs_Seq := | Expr ; Exprs_Seq

## Definitions

    Def     :=   | let Variable = Expr
                 | let Variables = Expr (Sugar)
                 | type = ident NewContructor_Case # Type Declaration

    NewContructor_Case :=   | constructeur_ident of Type
                            | NewContructor '|' NewContructor_Case