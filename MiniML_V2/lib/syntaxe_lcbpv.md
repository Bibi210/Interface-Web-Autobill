# Syntaxe et sémantique informelle de λ-CBPV

## Call-By-Push-Value En Bref

Le langage λ-CBPV ("lambda-call-by-push-value", ou juste LCBPV pour les intimes)
est une réprésentation intermédiaire de haut niveau, dans laquel on peut
compiler les langages à la ML (avec évaluation stricte), ou à la Haskell (avec
évaluation paresseuse), ou mélanger les deux pour compiler des fonctionalités
avancées (comme certaines concept de programmation objets).

Dans LCBPV, on a des valeurs et des calculs. Les valeurs *sont* des choses mais
de font rien, les calculs *font* des choses mais ne contiennent pas de données.
On a donc deux *sortes* de type: si `T` est type de valeur, on le défini comme
`T : +` , et de même, `T : -` est un type de calcul. Pour aider la
compréhension, quand `T` est un type de valeur, on le notera parfois `T+`, et
`T-` si c'est un type de calcul.

## Les types de valeurs 

### les entiers 

Classique. On les écrits en base 10, avec un '-' devant pour les négatifs. On
dispose des opérations classiques: addition, substraction, division euclidienne,
reste, et négation. On peut tester pour l'égalité et l'ordre. Le type de valeur
des entier est `Int : +`
```
0 1 2 3530530899 -2 -4343  (littéraux)
1+1 2-2 3*3 4*4 -5         (opérations)
6==6 7!=7 8<8 9=<9 10>10 11<=11 (tests)
```

### Les booléens 

Encore classique. `true`, `false`, `and`, `or` et `not`. Le type de valeur des
booléens est `Bool : +`
```
true false 
&& || ! == !=
```
On a un if/then/else:
```
if 1>0 then 45 else 422 
```

### Les tuples

On les note `(e_1, ..., e_n)`, et ils ont le type `Tuple(T_1+, ..., T_n+)`.
Chacun des `n` composant est une valeur `e_i : T_i : +`. Le 0-uplet/tuple vide
est noté `()` de type de valeur`Unit`. On accède aux composants `e_1`,...,`e_n`
des tuples par pattern matching:
```
match e_tuple with
 | (x_1, ..., x_n) -> ... 
end
```

### Les types sommes 

Les sommes binaires on le type de valeur `Sum(T+,U+) : +`. Ces deux
constructeurs sont `inj{1,2} : T -> sum(T,U)` et `inj{2,2} : U+ -> Sum(T+,U+)`.
En général, on peut faire des sommes avec *n* types `T_1+, ..., T_n+`. On note
le type de valeurs correspondant `Sum(T_1+, ...,T_n+) : +` et on a des
constructeurs `inj{i,n}` pour tout *i* entre 1 et *n*. On peut pattern matcher
sur les sommes:
```
match e_sum with
  | inj{1,n}(x) -> ...
  | inj{2,n}(x) -> ...
  ...
  | inj{n,n} -> ...
end
```
### Définir des types de valeurs 

On peut, comme en OCaml, définir des types par cas. Par exemple, le type des
listes et le type option sont définis de la manière suivante:
```
data List(A : +) =
  | nil()
  | cons(A, List(A));

data Option(A : +) =
  | none()
  | some(A);
```

On peut alors créer des listes en utilisant les constructeurs `Cons` et `Nil`,
par exemple, la liste [1,2,3] sera encodée par `Cons(1,Cons(3,Cons(3,Nil)))`. On
peut aussi faire du fitrage par motif sur les listes. Par exemple, si on a une
liste d'entiers `l`, l'expression ci-dessous renvoit la tête de la liste dans un
type option.
```
match l with 
  | nil() -> {return none()}
  | cons(h,t) -> {return some(h)}
end 
```

## Les blocs, thunks, et fermetures 

### Blocs 

LCBPV utilise des blocs pour séquencer les opérations. Un bloc est une séquence
d'instruction, séparées par un point-virgule, et terminé par un `return e` qui
évalue `e` et renvoie et renvoie la valeur correspondante. On a trois
instructions: `let open force`. La première est `let x = e;`, elle évalue
`e`, et affecte le résulat à `x`. `x` est en portée dans les instruction
suivantes dans le bloc et dans l'expression de retour. Par exemple:
```
let x = 2+2;
let y = 3+1;
return x + y
```

On peut remplacer une expression par bloc avec des allocades. C'est à dire, les
expressions `e`, `{return e}`, et `{let x = e; return x}` sont interchangeables.
On va maintenant voir `force` et `open`, qui gèrent les *thunks* et les
*fermetures*.

### Les thunks

Quand un calcul renvoie une valeur, il la renvoi encapsulée dans un calcul nommé
*thunk* et noté `thunk(e)`, où est une expression quelqueonque qui renvoie une
valeur. Il faut *forcer* le thunk avec `force x = thunk(e)` pour calculer
effectivement la valeur de `e`, et l'affecter à `x`. Pour l'exemple, comparez
les deux fragments de code suivants, qui calculent différement le même résultat.
Dans le premier, on calcule 2+2 avant de calculer 3+1, et dans le second on
calcule 3+1 en premier et 2+2 après.
```
// fragment 1:
let x = 2+2;
let y = 3+1;
return x + y 
  
//fragment 2:
let x = thunk(2+2);
let y = thunk(3+1);
force y' = y;
force x' = x;
return x'+y'
```

Si une expression `e : T : +` renvoie une valeur de type de valeur `T`, alors le
thunk `thunk(e) : Thunk(T) : -` est une valeur de type de calcul `Thunk(T)`

### Les fermetures 

Comme les thunks qui sont des calculent qui renvoient une valeur après
évaluation, on peut créer des valeurs qui stockent un calcul avant son
évaluation. si on a un calcul `f`, on peut le mettre dans une *fermeture*
`closure(f)` qui bloque son évaluation et stocke les valeurs de ses variables
libres. Quand on *ouvre* la fermeture avec `open g = closure(f)`, on assigne
`g=f` et on remet les valeurs dont `f` dépend sur la pile. On peut alors
accéder à `g` correctment (sans segfault).

Par exemple, dans le code suivant, un créé une fermeture sur un calcul qui
renvoi un entier. Comme les entiers sont des valeurs, on créé une fermeture
autour d'un trunk qui renvoi un entier. On voit que quand on définit un calcul,
on peut évaluer les calculs en plusieurs morceaux: quand ils sont définis, quand
on ouvre leur fermeture, et quand on les force.
```
// Au début, je n'ai pas encore évalué `a=2+2` ni `b=3+1`
let f = {

  let a = 2+2;
  // Ici, j'ai évalué 'a' mais pas `b`
    
  // la fermeture capture la variable `a=4` qui est libre dans son corps 
  return closure({
  
      // ce bloc est bloqué par la fermeture
      let b = 3+1;
      return thunk(a+b)
  })
};
  
// Ici, je n'ai toutjours pas évalué `b`
open f' = f;
// Après l'ouverture de `f`, `b` est évalué et on a assigné `f' = thunk(a+b)` 
  
// Puis on force le thunk pour évaluer `a+b` et récupérer 8
force x = f';
return x
```

Si `e : T : -` est une expression qui renvoie un calcul de type `T`, alors
`closure(e) = Closure(T) : +` est une valeur de type de valeur `Closure(T)`

## Les autres types de calculs

### Les fonctions 

Les fonctions en LCBPV sont des calculs. Comme en Java ou C++, on les réprésente
par des "objets" avec une méthode `call`. Par exemple, la fonction "f : x →
2x+3" sera écrite et appelée de la manière suivante. Notons que les fonctions
qui renvoient des types de valeurs doivent encapsuler leur valeur de retour dans
un thunk, et qu'il faut forcer le thunk pour terminer l'appel.
```
// `y` est défini comme un calcul, avec une méthode `call`:
let f = get 
    | call(x) -> thunk(2 * x + 3)
  end;
  
// `y` est un thunk qui calcule "f(1)". On a pas encore évalué f !
let y = f.call(1);

// Ici on calcule l'appel "f(1)", qui renvoie 2*1+3 = 5
force z = y;

return z // on renvoie z = 5
```

Le type des fonctions est `Fun(T_1+, ..., T_n+) -> T-`, avec chaque argument un
type de valeur et le résultat un type de calcul. Les fonctions en LCBPV ne sont
pas currifiés. Une fontion de type `Fun(A+, B+) -> C-` doit recevoir ces deux
arguments simultanément. Cette fonction, currifiée, serait de type `Fun(A+) ->
Fun(B+) -> C-`, ou encore `Fun(A+) -> Thunk(Closure(Fun(B+) -> C-))`.

### Les "paires paresseuses"

Les paires paresseuses sont des objets avec plusieurs méthodes qui renvoient
chacune un type différent. Au final, seul(s) le(s) élément(s) accédé(s) sont
effectivement calculées, d'où le coté paresseux. Si on a une paire paresseuse
`p` avec *n*, éléments, on appele la méthode `p.proj{i,n}()` pour sélectionner
le *i*-ème. Notons que qu'aucune des méthodes ne prend d'arguments, et qu'il
faut forcer le thunk pour effectivement calculer le *i*-ème élément.

On défini une paire paresseuse en spécifiant chacune des méthodes `proj{i,n}`
dans une clause `get ... end` comme pour les functions:
```
let douzaines = get
  | proj{1,3} -> 12
  | proj{2,3} -> 24
  | proj{3,3} -> 36
end;

force x = douzaine.proj{1,3}();

return x // renvoie 12
```

### Définir des types de calculs 

On peut aussi définir des types de calcul au cas-par-cas, c'est à dire en
définissant chaque méthode. Par exemple, on peut définir le type des itérateurs
sur les listes de la manière suivante: un itérateur est un objet avec deux
méthodes, une qui consome un élément de liste et renoie un nouvel itérateur, et
une second qui termine l'itération quand on croise `nil()`. On définit alors un
type `Iter(A, B)` qui consome des listes de type `A` et renvoi un `B`. La
méthode `use_nil()` renvoie une thunk sur le `B` final, et `use_cons` prend un
`A` et renvoi un `Iter(A,B)`. Notons que comme `Iter(A,B) : -` est un type de
calcul, on a pas besoin de renvoyer un thunk dans `use_cons`. 
```
comput Iter(A : +, B : +) =
  | use_nil() -> Thunk(B)
  | use_cons(A) -> Iter(A,B);
```

On définit souvent les itérateurs par récurrence, et on vera cela plus bas.

## Le reste des fonctionalités 

### Définitions récursives 

On peut définir des calculs récursifs avec le constructeur spécial `rec x is
...`. Par exemple, en OCaml on écrirai la fonction factorielle:
```
let rec fact n = 
  if n = 0 then
    1 
  else 
    n * (fact (n-1))
```

On écrirai globalement la même chose en LCBPV, mais en séparent le `let` et le
`rec`: on définit `let fact = rec self is ...`. `self` est alors une fermeture
sur la fonction récursive, et est en portée dans la définition de `f`.
Formellement, `fact` doit avoir un type de calcul `T-`, et `self` sera de type
`Closure(T-)`. Il est essentiel que `self` soit une fermeture pour des raisons
techniques, et donc doit être d'un type différent de `f`. 

Par exemple, on définit une function récursive `fact` de type
`Fun(Int)->Thunk(Int)`. Dans la définition, `self` est une fermeture
`Closure(Fun(Int) -> Thunk(Int))`, et on l'ouvre pour retrouver `fact`.
```
let fact = rec self is
    get
    | call(n) -> 
         if n==0 then 
           thunk(1) 
         else {
           open f = self;
           force m = f.call(n-1);
           return thunk(m * n)
         }
    end
```

### Déclarations

Pour simuler des bibliothèques, on peut déclarer des types et des valeurs au
toplevel sans les définir. Quand on déclare une valeur, on doit spécifier son
type, et quand on déclare un type, on doit déclarer la sorte (valeur/calcul) de
ces argument et la sort du type. Par exemple, pour déclarer les tableaux:
```
decl type Array : (+) -> +
decl array_init : Fun(Int, A) -> Thunk(Array(A))
decl array_read : Fun(Array(A), Int) -> Thunk(Option(A))
decl array_write : Fun(Array(A), A, Int) -> Thunk(Unit)
```

Notons que la variable de type `A` n'est pas déclarée, c'est implicitement une
variable de type polymorphique. La fonction `array_read` aurait en OCaml le type
`'a. 'a array * int -> 'a option thunk`. 

### Types linéaires (optionels)

Finalement, LCBPV supporte optionellement les types linéaires. C'est à dire que
les variables ne peuvent être utilisées qu'une fois ni plus ni moins dans chaque
execution du programme. Pour pouvoir interpréter les languages normaux (sans
types linéaires), on introduit des *fermetures partageables*. 

Les fermetures partagées fonctionnent exactement comme les fermetures:
- Si `e : T-` est une expression de calcul, `exp(e) : Exp(T-)` est une valeur de
  tpye `Exp(T-)`. Pour les fermetures normales, on a `closure(e) : Closure(T-)`.
- On peut accéder le caclul sous-jaçant avec l'instruction `unexp x = e;`, qui
  évalue le calcul dans la fermeture et l'assigne à `x`.
  
Quand on a pas activé les types linéaires dans LCPBV, on peut remplacer chaque
`exp` par un `closure` et rien ne change. Mais avec les types linéaires, on a
deux différences:
- On doit utiliser toutes les variables une fois, ni plus *ni moins*. 
- On peut néanmoins partager les `exp` librement, c'est à dire les utiliser zéro
  ou plus de une fois.
- On peut créer des `closure(e)` pour n'importe quel `e : T-`, mais pas pour
  `exp`. Si on veut mettre une expression `e : T-` dans une fermeture partagée
  `exp(e)`, il faut que les variable libres dans `e` soient toutes de type
  `Exp(...)`. Donc, les fermetures partagées ne peuvent dépendre que d'autres
  fermetures partagées, ou ne pas avoir de variables libres. 

## Tout les tokens 

- `x` un nom de variable,
- `litt` est un littéral `true`, `false`, ou un des entiers
- `op` est une opération primitive sur les entiers et les booléens (voir plus haut). 
- `k` un nom de constructeur, dont `unit`, `tuple` et `inj{i,n}`. L'expression
  `(e_1, ..., e_n)` signifie `tuple(e_1, ...,e_n)`, et `()` signifie `unit()`.
- `m` un nom de méthode, dont `call` et `proj{i,n}`
- `A` un nom de variable de type, dont: `Int Bool Unit Zero Top Bottom`
- `K` un nom de constructeur de type, dont: `Tupple Sum Fun Choice Thunk Closure Exp`
- Tout les mots-clés dans la syntaxe ci-dessous: `thunk closure exp match get end
  absurd return let letrec force open unexp data comput Int Bool Unit Zero Top Bottom
  Tupple Sum Fun Choice Thunk Closure`
- Les symboles `. , + - * / % : ; | ( ) { } = == != < > >= <= ! && || -> `

## Syntaxe formelle 

Notes:
- On peut aussi écrire `(e, ...)` pour `tuple(e, ...)` et `()` pour `unit()`
- dans `match`, `get`, `data` et `comput`, les cas sont séparés par des `|`, et
  le premier est optionel. On peut donc écrire `match e with k() -> ... | ...
  end` pour `match e with | k() -> ... | ... end`.

```
T ::= T+ | T- | A | K(T_1, ..., T_n)


x ::= x | x : T 


expr ::= 

  x                      (variables)
    
  litt                   (littéraux) 
    
  op(e, ..., e)          (primitives)
  
  e : T                  (expression avec une annotation de type)
  
  k(e_1, ..., e_n)       (constructeur (comme Some, Cons, Nil, etc.))
  
  thunk(e)               (thunks renvoyant une valeur `e`)
  
  closure(e)             (fermeture sur un calcul `e`)
  
  exp(e)                 (fermeture partagée)
  
  (e).m(e_1, ..., e_n)   (appel de méthode sur un calcul (à utiliser avec `get ... end`))
  
  if e then e else e     (conditionelle)
  
  match e with           (pattern matching (à utiliser avec `e` = `k(...)`)) 
    | k_1(x_1, ..., x_n) -> e_1
    | k_2(y_1, ..., y_n) -> e_2
    ...
  end
  
  get                    (définition de calcul)
    | m_1(x_1, ..., x_n) -> e_1
    | m_2(y_1, ..., y_n) -> e_2
    ...
  end
  
  { block }             (exécution de bloc)
  
  rec x is e            (calculs récursifs)
  
  absurd(e)             (indique que le code `e` est mort (jamais accessible au runtime))


block ::= 
  
  return e              (retour)
  
  let x = e; block      (asignement)
  
  force x = e; block    (ouverture de fermeture, à utiliser avec closure(e))
  
  open x = e; block  (forçage de thunk, à utiliser avec thunk(e))
  
  unexp x = e; block    (ouverture de fermeture partagée)
  
decl ::=
 
  data K(A_1 : ±, ..., A_n : ±) = 
    | k_1(T_1+, ..., T_n+)
    | k_2(U_1+, ..., U_n+)
    | ...
  end
     
  comput K(A_1 : ±, ..., A_n : ±) = 
    | m_1(T_1+, ..., T_n+) -> T-
    | m_2(U_1+, ..., U_n+) -> U-
    | ... 
  end
   
  type K(A_1 : ±, ..., A_n : ±) = T
     
  decl type K : (±, ..., ±) -> ±

  decl x : T

program ::=

  decl 
  decl
  ...
  decl
  block 
```
