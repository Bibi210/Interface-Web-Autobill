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
 <!--pandoc --lua-filter ./MarkdownVersions/color-text.lua  -N --variable "geometry=margin=1.2in" --variable mainfont="Palatino" --variable sansfont="Helvetica" --variable monofont="Menlo" --variable fontsize=12pt --variable version=2.0 ./MarkdownVersions/Rapport/Rapport.md  --pdf-engine=xelatex --toc -o Rapport.pdf  -->
\newpage

# Contexte du projet

## Qu'est-ce que Autobill ?
  **Autobill** est un projet universitaire soutenu par notre tuteur de projet Hector Suzanne, au sein de l'équipe APR du LIP6, dans le cadre de sa thèse sur l'analyse statique de la consommation mémoire d'un programme. 

  L'analyse statique se réfère au domaine en informatique visant à déterminer des métriques, des comportements ou des erreurs dans un code source. Pour un langage et une syntaxe donnée, on peut fixer des sémantiques d'évaluation, du typage ou, dans le cas de notre problématique, autour de la définition de l'occupation en ressources d'un programme.

  Historiquement, ce sujet de recherche a été plusieurs fois abordé dans divers travaux scientifiques, parmi eux, ceux de Hoffmann Jan et Stephen Jost sur l'analyse de consommation de ressources automatisé (AARA) [1]. Des solutions se basant sur ces théories existent, comme RAML [2], Resource Aware ML, un langage ML permettant ce type d'analyse.

  La proposition d'Hector avec Autobill se différencie par un niveau d'analyse plus fin. D'abord, Autobill est codé en Ocaml et prend en entrée des programmes écrits soit en modèle machine propre à Autobill, soit en **Call-By-Push-Value**. 
  
  C'est un langage qui utilise un paradigme déjà éprouvé, décrit dans les papiers de Paul Blain Lévy [3]. CBPV utilise une pile pour stocker les valeurs et les fonctions manipulées dans le programme. Ainsi, on peut suivre de manière explicite et précise les quantités de mémoire pour chaque valeur allouée / libérée ou fonction appelée / terminée. 
  
  Aussi, le langage permet d'exprimer des stratégies d'évaluation dans le code source : on retarde les évaluations jusqu'à ce que les expressions ou les résultats de calculs soient effectivement utilisées et on optimise ainsi la consommation de ressource du programme.

  À partir d'une entrée en CBPV, Autobill l'internalise et traduit le programme en un code machine, exprimant explicitement les contraintes matérielles qui s'appliquent sur l'entrée. Enfin, il retourne en sortie ces contraintes formalisées pour satisfaire le format d'entrée de différents solveurs et assistants de preuve, comme MiniZinc ou Coq, afin de prouver des propriétés de complexité temporelle et spatiale.

## Objectifs du projet

  Notre démarche se rapproche de celle faite pour RAML [2] dans leur site officiel.

  Le sujet de notre projet STL va donc être de soutenir l'effort de développement en proposant une interface sur le Web permettant la libre manipulation de l'outil Autobill par des tiers à travers un environnement de développement sur navigateur. 
  
  On souhaite aussi faciliter l'utilisation de l'outil avec un langage fonctionnel pur en entrée plus accessible, un **MiniML**. Enfin, on se place aussi sur la sortie d'Autobill en traitant les expressions de contraintes qu'il génère avec des solveurs externes, afin d'en tirer des preuves de complexité et les afficher directement sur le client Web.

  Notre charge de travail doit se diviser en plusieurs tâches principales : 
  - L'implémentation du langage MiniML et sa traduction vers LCBPV
  - La mise en place d'un client et d'un serveur Web 
  - La mise en relation entre l'interface Web et la machine Autobill
  - Le traitement des contraintes d'Autobill par un solveur  
  - Les tests de performances et comparaisons avec les solutions exitantes
  
## Processus de Conception
  Lors de la conception de l'interface les contraintes étaient multiples.\
  La première était le langage d'implémentation en effet **Autobill** étant développé en **OCaml** il nous était obligatoire de trouver un moyen pour communiquer avec l'application.\
  La seconde était qu'il fallait développer cette interface en simultané avec **Autobill** et d'ajuster notre travail en fonction des besoins courants de nos encadrants.\
  Mais la plus importante d'entre elles était le souhait de nos encadrants que l'application soit principalement côté client afin de simplifier son déploiement.\ 

  Une fois ses contraintes établies nous avons du tout au long de ce projet effectuer des choix que ce soit en matière de design ou de technologies.

  Nous tenons donc à travers ce rapport à mettre en lumière ces décisions tout en décrivant le travail qu'elles ont engendré.

\newpage
# Interface Web 


  Dans l'optique de ne pas se restreindre dans un choix de conception, le groupe s'est orienté vers deux structures de projets différentes et indépendantes : l'une fonctionnant avec un client unique, la seconde avec un serveur dédié et un client qui expose ce serveur. L'avantage réside dans le fait que, lors du développement, si un nouvel outil est amené à être utilisé mais ne dispose de compatibilité sur navigateur Web, alors le serveur peut répondre à ce problème. C'est aussi un sujet de comparaison intéressant à présenter par la suite, que ce soit au niveau des performances que du déploiement de ces solutions.

## Client uniquement

### Design du client

![](./MarkdownVersions/Rapport/screen.png)

### Outils et Technologies utilisées

- **HTML / CSS / Javascript** : 
Il s'agit de la suite de langages principaux permettant de bâtir l'interface Web souhaitée. On a ainsi la main sur la structure de la page à l'aide des balises HTML, du style souhaité pour l'éditeur de code avec le CSS et on vient apporter l'interactivité et les fonctionnalités en les programmant avec Javascript, complété par la librairie React.


- **React.js** : React s'ajoute par-dessus la stack technique décrite plus haut pour proposer une expérience de programmation orientée composante sur le Web. C'est une librairie Javascript permettant de construire des applications web complexes tournant autour de composants / éléments possédant un état que l'on peut imbriquer entre eux pour former notre interface utilisateur et leur programmer des comportements et des fonctionnalités précises, sans se soucier de la manipulation du DOM de la page Web.

- **CodeMirror** : C'est une librairie Javascript permettant d'intégrer un éditeur de code puissant, incluant le support de la coloration syntaxique, de l'auto-complétion ou encore le surlignage d'erreurs. Les fonctionnalités de l'éditeur sont grandement extensives et permettant même  la compatibilité avec un langage de programmation personnalité comme **MiniML**. Enfin, CodeMirror est disponible sous licence MIT.

- **OCaml + Js_of_OCaml** :  Afin de manipuler la librairie d'**Autobill**, il est nécessaire de passer par du côté OCaml pour traiter le code en entrée et en sortir des équations à résoudre ou des résultats d'interprétations. Pour faire le pont entre Javascript et OCaml, on utilise Js_of_OCaml, une librairie contenant, entre autres, un compilateur qui transpile du bytecode OCaml en Javascript et propose une grande variété de primitive et de type pour manipuler des éléments Javascript depuis OCaml

### Tâches réalisées 
  - Intégration d'une IDE similaire aux Playground de [OCaml](https://OCaml.org/play) et [Rescript](https://rescript-lang.org/try)
  - Implémentation d'un éditeur de code supportant la syntaxe de **MiniML**
  - Liaison entre le code Javascript et OCaml à l'aide de Js_of_OCaml 
  - Implémentation de plusieurs modes d'interprétation du code **MiniML** : 
    - Vers AST 
    - Vers AST de **Call-By-Push-Value**
    - Vers Equation
    - Vers code Machine **Autobill**
  - Remontée d'erreurs et affichage dynamique sur l'interface
  - Implémentation du solveur d'équations MiniZinc côté client
  - Solveur (Web Assembly)

\newpage

## Serveur + Client


### Schéma de Communication


![](./MarkdownVersions/Rapport/communication.png)

### Outils et Technologies utilisées

#### Coté Client

   - **HTML / CSS / Javascript** 
   - **React.js**
   - **CodeMirror** 
   - **OCaml + Js_of_OCaml** 
  
#### Coté Serveur

   - **NodeJS**: NodeJS permet une gestion asynchrone des opérations entrantes, ce qui permet d'avoir une grande efficacité et une utilisation optimale des ressources. En outre, NodeJS est également connu pour son excellent support de la gestion des entrées/sorties et du traitement de données en temps réel. Enfin, la grande quantité de packages disponible sur NPM (le gestionnaire de packages de Node Js) permet de gagner beaucoup de temps de développement et de faciliter notre tâche.

### Tâches réalisées

  - Intégration d'une IDE similaire aux Playground de [OCaml](https://OCaml.org/play) et [Rescript](https://rescript-lang.org/try)
  - Implémentation d'un éditeur de code supportant la syntaxe de **MiniML**
  - Liaison entre le code Javascript et OCaml à l'aide de Js_of_OCaml 
  - Implémentation de plusieurs modes d'interprétation du code **MiniML** : 
    - Vers Equation
  - Implémentation du solveur d'équations MiniZinc côté client
  - Solveur 

\newpage
# MiniML

## Pourquoi MiniML ?


MiniML émerge de la volonté de créer un langage fonctionnel simple, accessible et sans effets de bord pour les utilisateurs d'autobill car celui-ci requiert une connaissance approfondie de la théorie autour des différentes sémantiques d'évaluation a travers le paradigme **Call-By-Push-Value**.


### Call-By-Push-Value
Le paradigme de traitement de langage **Call-By-Push-Value** utilisé par autobill permet à l'aide d'une seule sémantique de traiter deux types de stratégies d'évaluation différentes **Call By Value** utilisée par **OCaml** et **Call By Name** utilisée par **Haskell** pour mettre en place l'évaluation *Lazy*.\
Pour permettre cette double compatibilité, **Call-By-Push-Value** effectue une profonde distinction entre les calculs et les valeurs.\
La différenciation entre ses deux types de stratégies s'effectue lors de la traduction depuis le langage d'origine.


## Description Rapide
**MiniML** dans ce projet dispose d'une implémentation écrite en **OCaml**.\
**MiniML** possède deux types de base (Integer et Boolean).\
Il est possible de créer de nouveaux types à partir de ceux-ci.\
La syntaxe de MiniML est très proche de celle d'OCaml et parfaitement compatible avec un parser OCaml


## Contenu Actuel

- Listes
- Files
- Fonction Recusives
- Opérateurs de Bases
- Construction de Types
- Variables Globales/Locales
  

## Diagramme
![](./MarkdownVersions/Rapport/MiniML.png)

\newpage

# Projections (Rapport Suivant)
## MiniML
  - Ajout de sucre syntaxique.
  - Ajout d'une librairie standard.
  - Spécification complète du langage.
  - Bibliothèque de tests sous la forme de structure de données complexes


## Serveur
  - Affichage des erreurs
  - Réalisation des autres services pour MiniML
  - Réalisation de génération de solution depuis le code Autobill
  
## Client 
  - Retouches esthétiques
  - Affichage des erreurs sur plusieurs lignes
  - Couverture d'erreurs à traiter la plus grande possible, afin d'éviter les blocages du client
  - "Benchmark" la résolution d'équations plus complexes avec le MiniZinc client
  - Proposer des programmes d'exemples à lancer, demandant des lourdes allocations mémoires.
  
## Tests
  - Comparaison d'architectures Full-Client vs Client-Serveur
  - Comparaison **RAML** vs **Autobill**

\newpage

# Bibliographie 

- [1] Hoffmann, Jan, and Steffen Jost. “Two Decades of Automatic Amortized Resource Analysis.” Mathematical structures in computer science 32.6 (2022): 729–759

- [2] Levy, Paul Blain. “Call-by-Push-Value: A Subsuming Paradigm.” Lecture Notes in Computer Science. Berlin, Heidelberg: Springer Berlin Heidelberg, 1999. 228–243
  
- Will Kurt. 2018. Get Programming with Haskell. Simon and Schuster. Chapitre 5,7

- Pierce, Benjamin C. Types and Programming Languages. MIT Press, 2002


- Winskel, Glynn. The Formal Semantics of Programming Languages : an Introduction. Cambridge (Mass.) London: MIT Press, 1993

- Compilers : Principles, Techniques, and Tools. 2nd ed. Boston (Mass.) San Francisco (Calif.) New York [etc: Pearson Addison Wesley, 2007]

- Minsky, Anil Madhavapeddy, and Jason Hickey. 2013. Real World OCaml. O’Reilly Media.

- Martin Avanzini and Ugo Dal Lago. 2017. Automating sized-type inference for complexity analysis. Proceedings of the ACM on Programming Languages 1, 1-29

- Dominic Orchard, Vilem-Benjamin Liepelt, and Harley Eades III. 2019. Quantitative program reasoning with graded modaltypes.Proceedings of the ACM on Programming Languages3, ICFP (2019)

- Xavier Leroy. 2022 OCaml library. OCaml Lazy Doc. Retrieved February 20, 2023 [**Link**](https://v2.OCaml.org/api/index.html)

- Haskell - Wikibooks, open books for an open world. Doc Haskell. Retrieved February 17, 2023 [**Link**](https://en.wikibooks.org/wiki/Haskell)
