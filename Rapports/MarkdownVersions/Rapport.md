---
title: "PSTL : Interface Web Autobill"
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
date: 23 mars, 2023
---
 <!--pandoc --lua-filter ./MarkdownVersions/color-text.lua  -N --variable "geometry=margin=1.2in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=12pt --variable version=2.0 ./MarkdownVersions/Rapport.md  --pdf-engine=xelatex --toc -o Rapport.pdf -->
\newpage

# Contexte du projet

## Que-est-ce qu'est autobill ?
  - TODO
  
## Comment on rentre dans ce projet ?
  - TODO


# Avancement
## MiniML
  - Description du language
  - Description traduction vers lcbpv
    - Expliquer lcbpv (Contexte,Fonctionnement,Difficultées)
  - Choix de Conception
  
## Archi Full Client
  - Design du client (Images ? Inspirations ?)
    - Schema
  - React/Js_Of_Ocaml 
    - Interlanguage Pb Rencontrée (Thread Block , Exception ... )
    - Pourquoi React
      - Tenter un lien PC3R ??
    - Description Js_Of_Ocaml 
      - Neccesaire dans l'archi full client (Pourquoi)
  - Solveur (Web Assembly)


## Archi Serveur + Client
  - Design du server (Images ? Inspirations ?)
    -  Schema
        -  Montrer les echanges de données
    - Pourquoi Node ? (Et pas ocaml par exemple)
    - Tenter un lien PC3R ??
  - Solveur

## Client VS Serveur
  - Avantages Inconvénients 

# Projections (Rapport Suivant)
## MiniML
  - Description + Full Spec
  - Lazyness
  - Bibliothéque de tests (Structure de donéees complexes)

## Serveur
  - ?
  
## Client 
  - ?
  
## Tests

  - Comparaison Client vs Servers
  - Comparaison YAML vs Autobill

# Bibliographie 
  - Reprendre le carnet de bord
