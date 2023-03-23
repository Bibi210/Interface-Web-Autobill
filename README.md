# Projet PSTL (Interface Web Autobill)

L'objectif de ce projet est de fournir une interface Web pour faciliter l'installation et l'expérimentation d'Autobill

## Fonctionnement
1. Utilisation d'un site type [RAML](https://www.raml.co/interface/)
2. Parsing d'un language Fonctionnel vers un AST
   - Affichage d'Erreurs (A definir)
   - Affichage Coloration Syntaxique ? 
3. Utilisation "en quasi boite noire" d'Autobill
   - Affichage d'Erreurs (A definir) 
4. Utilisation d'un solveur (Lequel ?)
   - Affichage Resultats (A definir)

## Notes Progression
**Présoutenance avant vacances AVRIL**
- [ ] Partie ML
    - // ****
- [ ] Partie Web
    - // ****

- [Discord Link](https://discord.gg/wRNjJjBSQ9)
- [GitLab Link](https://github.com/Bibi210/Interface-Web-Autobill)

## Partie Mini-ML (Ocaml)
**Objectif Text to AST**\
Petit front-end d'un language fonctionnel (Ocaml-Like ou Lisp)
**Technos : Menhir/(Ocamllex?)**

### Fonctionnalités
    **Types**
    - Integer
    - Bool
    - Tuple
    - List (Cons Nil)
    
    **Expressions**
    - Match
    - Func (Recursives)
    - Type_Def (A Spécifier)
    - If

### TODO LIST 
- [X] LexerV1 (Ocamllex)
- [X] Parser (Menhir)
- [X] Traduction AST (Connection AutoBill)
  - [X] Compiler AutoBill
- [ ] LexerV2 (HandMade) ? (Not Needed)
  - Si version full-client
  - Stream Txt -> Stream Token pour menhir
- [ ] Testing (Yaml)
  - How ?

## Partie JavaScript ( React / Js_of_ocaml )

Site en deux colonne.\
**Technos : React**

### Documentation/Infos

[React Framework](https://fr.reactjs.org/)\
[Js_of_ocaml Doc](http://ocsigen.org/js_of_ocaml/latest/manual/overview)\
[Site RAML](https://www.raml.co/interface/)

### Fonctionnalités
    - Affichage D'erreurs
    - Coloration Syntaxique ?

### TODO LIST 
- [ ] Apprendre React
- [ ] Definir le design du site
- [ ] Testing
  - How ? 