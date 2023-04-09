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

## Historique et définitions

  **Autobill** est un projet universitaire développé par notre tuteur de projet Hector Suzanne, au sein de l'équipe APR du LIP6, dans le cadre de sa thèse sur l'analyse statique de la consommation mémoire d'un programme. 

  L'analyse statique se réfère au domaine de l'informatique visant à déterminer des métriques, des comportements ou des erreurs dans un programme par la lecture de son code source. Pour un langage donnée, on peut fixer des sémantiques d'évaluation et du typage. Dans le cas de notre problématique, on s'intéresse à l'occupation en mémoire d'un programme.
  
  Historiquement, ce sujet de recherche a été plusieurs fois abordé dans divers travaux scientifiques, parmi eux, ceux de Jan Hoffmann et Stephen Jost sur l'analyse de consommation de ressources automatisé (AARA) [1]. Des solutions se basant sur ces théories existent, comme RAML [2] (Resource Aware ML), un langage ML permettant ce type d'analyse.

## Qu'est-ce que Autobill ?

  La proposition d'Hector avec Autobill se différencie par un niveau d'analyse plus précis sur les fermetures et les arguments fonctionnelles d'un programme. D'abord, Autobill prend en entrée des programmes écrits soit en modèle machine propre à Autobill, soit en **Call-By-Push-Value** (CBPV), avec ou sans continuation explicite. 
  
  C'est un langage qui utilise un paradigme déjà éprouvé, décrit dans la thèse de Paul Blain Lévy [3]. CBPV utilise une pile pour stocker les valeurs et les fonctions manipulées dans le programme. Ainsi, on peut suivre de manière explicite et précise les quantités de mémoire pour chaque valeur introduite / éliminée ou fonction appelée / terminée. Aussi, le langage permet d'exprimer clairement les stratégies d'évaluation utilisées dans le code source : on fixe quand les évaluations se déroulent et on peut mieux prédire la consommation de mémoire à chaque étape du programme.

  À partir d'une entrée en CBPV, Autobill l'internalise et traduit le programme en un code machine avec continuation, exprimant explicitement les contraintes de taille qui s'appliquent sur l'entrée. Enfin, il retourne en sortie ces contraintes formalisées pour satisfaire le format d'entrée de différents outils de recherche d'optimisations et assistants de preuve, comme [MiniZinc](https://www.minizinc.org/) ou [Coq](https://coq.inria.fr/), afin de prouver des propriétés de complexité temporelle ou spatiale.

  ![](./MarkdownVersions/Rapport/Schema_Autobill.png)

## Objectifs du projet

  Notre démarche se rapproche de celle faite pour RAML [2] dans leur site officiel.

  Le sujet de notre projet STL va donc être de soutenir l'effort de développement en proposant une interface sur le Web permettant la libre manipulation de l'outil Autobill par des utilisateurs à travers un environnement de développement sur navigateur. 
  
  On souhaite aussi faciliter l'utilisation de l'outil avec un langage fonctionnel pur en entrée plus accessible, un **MiniML**. Cela nous contraint donc à adapter cette nouvelle entrée pour qu'elle soit compatible avec Autobill. Enfin, on se place aussi sur la sortie d'Autobill en traitant les expressions de contraintes qu'il génère avec des solutions externes, afin d'en tirer des preuves de complexité et les afficher directement sur le client Web.

  Notre charge de travail doit se diviser en plusieurs tâches principales : 

- L'implémentation du langage MiniML et sa traduction vers LCBPV
- La mise en place d'un client et d'un serveur Web 
- La mise en relation entre l'interface Web et la machine Autobill
- Le traitement des contraintes d'Autobill par un solveur externe
- Les tests de performances et comparaisons avec les solutions existantes

![](./MarkdownVersions/Rapport/Diagramme Haut Niveau PSTL.png)
  
## Processus de Conception
  Lors de la conception de l'interface, les contraintes étaient multiples.\
  La première était l'interopérabilité des technologies du projet. En effet **Autobill** étant développé en **OCaml**, il était nécessaire de trouver des moyens pour l'adapter à un environnement Web.\
  La seconde était qu'il fallait développer cette interface en simultané avec **Autobill** et ajuster notre travail en fonction des besoins courants de nos encadrants.\
  Mais la plus importante d'entre elles était le souhait de nos encadrants que l'application soit principalement côté client afin de simplifier son déploiement dans les infrastructures de la faculté .\ 

  Une fois ces contraintes établies, nous avons dû,tout au long de ce projet, effectuer des choix, que ce soit en matière de design ou de technologies.

  Nous tenons donc à travers ce rapport à mettre en lumière ces décisions, tout en décrivant le travail qu'elles ont engendré.

\newpage
# Interface Web 

  Dans l'optique de ne pas se restreindre dans l'utilisation d'outils notamment au niveau du résolveur de contraintes, le groupe s'est orienté vers deux structures de projets différentes et indépendantes : l'une fonctionnant avec un client unique, la seconde avec un serveur dédié et un client qui expose ce serveur. 
  
  L'avantage réside dans le fait que, lors du développement, si un nouvel outil est amené à être utilisé mais ne dispose de compatibilité sur navigateur Web, alors le serveur peut répondre à ce problème. C'est aussi un sujet de comparaison intéressant à présenter par la suite, que ce soit au niveau des performances que du déploiement de ces solutions.

## Client uniquement

### Design du client

![](./MarkdownVersions/Rapport/screen.png)

\newpage

### Outils et Technologies utilisés

- **HTML / CSS / Javascript** : 
Il s'agit de la suite de langages principaux permettant de bâtir l'interface Web souhaitée. On a ainsi la main sur la structure de la page à l'aide des balises HTML, du style souhaité pour l'éditeur de code avec le CSS et on vient apporter l'interactivité et les fonctionnalités en les programmant avec Javascript, complété par la librairie React.


- **React.js** : React s'ajoute par-dessus les langages décrits plus haut pour proposer une expérience de programmation orientée composante sur le Web. C'est une librairie Javascript permettant de construire des applications web complexes tournant autour de composants / éléments possédant un état que l'on peut imbriquer entre eux pour former notre interface utilisateur et leur programmer des comportements et des fonctionnalités précises, sans se soucier de la manipulation du DOM de la page Web.

- **CodeMirror** : C'est une librairie Javascript permettant d'intégrer un éditeur de code puissant, incluant le support de la coloration syntaxique, de l'auto-complétion ou encore le surlignage d'erreurs. Les fonctionnalités de l'éditeur sont grandement extensives et permettant même la compatibilité avec un langage de programmation personnalité comme **MiniML**. Enfin, CodeMirror est disponible sous licence MIT.

- **OCaml + Js_of_OCaml** :  Afin de manipuler la librairie d'**Autobill**, il est nécessaire de passer par du côté OCaml pour traiter le code en entrée et en sortir des équations à résoudre ou des résultats d'interprétations. Pour faire le pont entre Javascript et OCaml, on utilise Js_of_OCaml, une librairie contenant, entre autres, un compilateur qui transpile du bytecode OCaml en Javascript et propose une grande variété de primitive et de type pour manipuler des éléments Javascript depuis OCaml

### Tâches réalisées 
  - Intégration d'une IDE similaire aux Playground de [OCaml](https://OCaml.org/play) et [Rescript](https://rescript-lang.org/try)
  - Implémentation d'un éditeur de code supportant la syntaxe de **MiniML**
  - Liaison entre le code Javascript et OCaml à l'aide de Js_of_OCaml 
  - Implémentation de plusieurs modes de traitement du code **MiniML** : 
    - Affichage de l'AST MiniML
    - Affichage de l'AST de **Call-By-Push-Value**
    - Affichage de l'Equation résultant de l'anlyse statique
    - Vers Representation Interne **Autobill**
  - Remontée d'erreurs et affichage dynamique sur l'interface
  - Implémentation du solveur d'équations MiniZinc côté client

\newpage

## Serveur + Client


### Schéma de Communication

![](./MarkdownVersions/Rapport/communication.png)

### Outils et Technologies utilisés

#### Coté Client

   - **HTML / CSS / Javascript** 
   - **React.js**
   - **CodeMirror** 
   - **OCaml + Js_of_OCaml** 
  
#### Coté Serveur

   - **NodeJS**: NodeJS permet une gestion asynchrone des opérations entrantes, ce qui permet d'avoir une grande efficacité et une utilisation optimale des ressources. En outre, NodeJS est également connu pour son excellent support de la gestion des entrées/sorties et du traitement de données en temps réel. Enfin, la grande quantité de packages disponible sur NPM (le gestionnaire de packages de Node Js) permet de gagner beaucoup de temps de développement et de faciliter notre tâche. Par example, le module ["Child Processes"](https://nodejs.org/api/child_process.html) nous permet de éxecuter le code MiniZinc en passant les commandes directement. Cela nous permet d'éviter les restrictions en côté full-client au niveau de  du résolveur de contraintes.

#### Solveur
    - **MiniZinc**: Nous utilisons MiniZinc pour calculer les bornes mémoires minimums pour satisfaire les contraintes mémoires du programme. MiniZinc est un langage de modélisation de contraintes de haut niveau utilisé pour la résolution de problèmes de satisfaction de contraintes. Il prend en charge une large gamme de solveurs, permettant aux utilisateurs de choisir le solveur le mieux adapté à leur problème et de passer facilement d'un solveur à l'autre. Ainsi, il dispose d'une grande communauté d'utilisateurs et de contributeurs, ce qui nous permet de trouver nombreuses ressources disponibles pour l'apprentissage et le dépannage.

### Tâches réalisées

  - Intégration d'une IDE similaire aux Playground de [OCaml](https://OCaml.org/play) et [Rescript](https://rescript-lang.org/try)
  - Implémentation d'un éditeur de code supportant la syntaxe de **MiniML**
  - Liaison entre le code Javascript et OCaml à l'aide de Js_of_OCaml 
  - Implémentation de plusieurs modes de traitement du code **MiniML** : 
     - Affichage de l'Equation résultant de l'anlyse statique
  - Implémentation du solveur d'équations MiniZinc côté client et server

\newpage
# MiniML

## Pourquoi MiniML ?

MiniML émerge de la volonté de créer un langage fonctionnel simple, accessible et sans effets de bord pour les utilisateurs d'autobill car celui-ci requiert une connaissance approfondie de la théorie autour des différentes sémantiques d'évaluation afin de pouvoir manipuler son entrée en **Call-By-Push-Value**.


### Call-By-Push-Value
Le paradigme de traitement de langage **Call-By-Push-Value** utilisé par autobill permet à l'aide d'une seule sémantique de traiter deux types de stratégies d'évaluation différentes **Call By Value** utilisée par **OCaml** et **Call By Name** utilisée par **Haskell** pour mettre en place l'évaluation *Lazy*.\
Pour permettre cette double compatibilité, **Call-By-Push-Value** effectue une profonde distinction entre les calculs qui font et les valeurs qui sont.\
La différenciation entre ses deux types de stratégies s'effectue lors de la traduction depuis le langage d'origine.


## Description Rapide
**MiniML** dans ce projet dispose d'une implémentation écrite en **OCaml**.\
**MiniML** possède deux types de base (Integer et Boolean).\
Il est possible de créer de nouveaux types à partir de ceux-ci.\
MiniML est un modeste sous set d'OCaml et parfaitement compatible avec un parseur ou compilateur **OCaml**

### Dépendances

- **Menhir** : *Menhir* est l'unique dépendance de l’implémentation de **MiniML**, Cette librarie permet la génération de parseurs LR(1) en OCaml.\
*Menhir* est disponible sous une licence GNU GENERAL PUBLIC.



## Contenu Actuel

- Listes
- Fonction Recusives
- Opérateurs de Bases
- Construction de Types
- Variables Globales/Locales
- Files

\newpage

## Un exemple de code MiniML

```OCaml
  type 'a option =
  | None
  | Some of 'a
  ;;

  let createFile = ([],[]);;

  let push file elem = 
  (match file with
  | (a,b) -> (a,(elem::b)))
  ;;

  let pop file =
    (match file with
    | (debut, fin) -> 
      (match debut with
      | [] -> (
                match (rev fin) with
                | [] -> (None,debut,fin)
                | (hd :: tail) -> ((Some(hd)), tail ,[])
              )
      | (hd::tail) -> ((Some(hd)),tail,fin ))
    )
  ;;

  let elems = [1;2;3;4;5;6;7];;
  let queue = (fold_left push createFile elems);;
  (pop queue)

```

Dans le prochain rapport, nous allons nous baser sur une variante de cet exemple pour décrire, avec des schémas de traduction comment l'on passe d'un AST **MiniML** a un AST **Call-By-Push-Value** compatible pour **Autobill**.

\newpage

# Conclusion et Tâches à réaliser

## Conclusion
  La réalisation de cette interface a fait intervenir un large panel de sujets en lien avec la formation du Master d'informatique STL et mis à profit les connaissances acquises lors de ce semestre. Le projet est à un stade d'avancement satisfaisant. Autobill étant encore en phase expérimentale, celui-ci ajoute contiuellement des nouveautés et corrections que l'on doit intégrer.

  La suite consistera surtout à consolider les bases établies sur tous les aspects du projet présentés dans ce rapport et les adapter aux changements d'Autobill. Aussi, il serait intéressant à titre de démonstration de comparer notre solution avec celle de Jan Hoffmann et l'interface de RAML [3], cités en section 1.

## MiniML
  - Ajout de sucre syntaxique. (Records, Operateurs Infixes, ...)
  - Ajout d'une librairie standard.
  - Spécification complète du langage.
  - Bibliothèque de structures de données complexes
  - Schemas de compilation d'une structure *FIFO* vers **Autobill**

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

- [3] Hoffman, Jan. Resource Aware ML, Web, [URL](https://www.raml.co/)
  
- Will Kurt. 2018. Get Programming with Haskell. Manning Publications.

- Pierce, Benjamin C. Types and Programming Languages. MIT Press, 2002

- Winskel, Glynn. The Formal Semantics of Programming Languages : an Introduction. Cambridge (Mass.) London: MIT Press, 1993

- Alfred V. Aho, Monica S. Lam, Ravi Sethi, Jeffrey D. Ullman, Compilers : Principles, Techniques, and Tools. 2nd ed. Boston (Mass.) San Francisco (Calif.) New York [etc: Pearson Addison Wesley, 2007]

- Minsky, Anil Madhavapeddy, and Jason Hickey. 2013. Real World OCaml. O’Reilly Media, [**URL**](https://dev.realworldocaml.org/)

- Martin Avanzini and Ugo Dal Lago. 2017. Automating sized-type inference for complexity analysis. Proceedings of the ACM on Programming Languages 1, Volume 1, Issue ICFP, Article No.: 43, pp 1–29 (2017)

- Dominic Orchard, Vilem-Benjamin Liepelt, and Harley Eades III. 2019. Quantitative program reasoning with graded modaltypes.Proceedings of the ACM on Programming Languages3, Volume 3, Issue ICFP, Article No.: 110, pp 1–30 (2019)

- Xavier Leroy. 2022 OCaml library. OCaml Lazy Doc. Retrieved February 20, 2023 [**Link**](https://v2.OCaml.org/api/index.html)

- Haskell - Wikibooks, open books for an open world. Doc Haskell. Retrieved February 17, 2023 [**Link**](https://en.wikibooks.org/wiki/Haskell)

- Emmanuel Chailloux,  Pascal Manoury, Bruno Pagano. Développement d’applications avec Objective Caml. Paris Cambridge [etc: O’Reilly, 2000],[**URL**](https://www-apr.lip6.fr/~chaillou/Public/DA-OCAML/) 

- Hector Suzanne. Autobill, 2023, GitLab, [**URL**](https://gitlab.lip6.fr/suzanneh/autobill)