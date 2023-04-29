---
title: MiniML Spec
author:  
  - Fazazi Zeid
  - Luo Yukai 
  - Dibassi Brahima 
lang: fr
date: 28 avril, 2023
geometry:
  - margin = 1.2in
mainfont: Palatino
sansfont: Helvetica
monofont: Menlo
fontsize: 12pt
urlcolor: NavyBlue
numbersections: true # Numéros de sections
toc: true # Table des matières
tableofcontents: true # Table des matières
include-before: | # Texte avant la table des matières
    \newpage
---
\allowdisplaybreaks
\pagebreak
# Syntaxe MiniML
## Lexing Tokens

### Separators

$$\{\quad\}\qquad[\quad]\qquad(\quad)\qquad;\qquad,\qquad*\qquad\rightarrow\qquad|\qquad=$$

### Mots Clefs

$$let \qquad fun\qquad in \qquad match \qquad with \qquad type \qquad of \qquad rec \qquad if \qquad then \qquad else$$

### Types

$$int\qquad bool\qquad unit$$

### Operateurs

$$ + \qquad - \qquad \% \qquad  /  \qquad :: \qquad  \&\& \qquad  || \qquad  * \qquad < \qquad > \qquad \leq \qquad \geq \qquad = $$


### Valeurs Atomiques

\begin{align*}
integer &= (\;-\;)\;?\;[\;0\;-\;9\;]\* \\
boolean &= (\;true\;|\;false\;)
\end{align*}

### Identificateur

\begin{align*}
alphanum &= [a-z \quad A-Z \quad 0-9 \quad \_]\,* \\
basic\_ident &= [a-z \quad \_] \, alphanum \\
vartype &= [`a] \; [0 \, .. \, 9]\,*\ \\
\\
constructeur\_ident &= [A-Z] \, alphanum \\
constructeur\_infixes &= [:: \quad ,]
\end{align*}

\pagebreak

## Grammaire
  \begin{align*}
    \bf{Prog} := \quad & |\quad \bf{Def} \\
        & |\quad \bf{Expr} \\
        & |\quad \bf{Prog} \; ;; \; \bf{Prog}
  \end{align*}

### Definitions
\begin{align*}
\bf{Def} := \quad &|\quad let \; basic\_ident = \bf{Expr} &&\textit{Def Variable}  \\
    &|\quad let \; basic\_ident_{\bf{list}} = \bf{Expr} && \textit{Def Function} \\ 
    &|\quad let \; rec \; basic\_ident_{\bf{list}} = \bf{Expr} && \textit{Def FunctionRec} \\
    &|\quad type \; vartype_{\bf{list}} \; basic\_ident =  \bf{NewContructor\_Case} &&  \textit{Def Type} 
\end{align*}
\begin{align*}
    \bf{NewContructor\_Case} :=   \quad &|\quad  constructeur\_ident \\
                             &|\quad  constructeur\_ident \;of\; \textbf{Type} \\
                             &|\quad  NewContructor\_Case \;'|'\; NewContructor\_Case \\
\end{align*}

### Expressions
\begin{align*}
\bf{Litteral}  \quad :=  \quad &|\quad integer \\
                    &|\quad boolean \\
                    &|\quad (\;) &&  \textit{ Unit} \\
                    \\
\bf{Expr}  \quad :=  \quad &|\quad ( \textbf{Expr} ) \\
            &|\quad \textbf{Litteral} \\
            &|\quad basic\_ident \\
            &|\quad \textbf{UnaryOperator}\quad \textbf{Expr} \\
            &|\quad \textbf{Expr}\quad \textbf{BinaryOperator}\quad \textbf{Expr} \\
            &|\quad \textbf{Expr}\quad \textbf{Expr} && \textit{ Call} \\
            &|\quad \textbf{Expr};\textbf{Expr} && \textit{ Sequence } \\
            &|\quad let \; basic\_ident = \textbf{Expr}\; in \; \textbf{Expr} && \textit{ Binding} \\
            &|\quad fun \; basic\_ident_{\bf{list}} \rightarrow  \textbf{Expr} && \textit{ Lambda} \\
            &|\quad \textbf{Expr}\; constructeur\_infixes\; \textbf{Expr} \\
            &|\quad constructeur\_ident  \; \textbf{Expr}\;  && \textit{Construction} \\
            &|\quad constructeur\_ident  \\
            &|\quad let \; basic\_ident_{\bf{list}} = \textbf{Expr}\; in \; \textbf{Expr} && \textit{Fonction} \\
            &|\quad let \; rec \; basic\_ident \; basic\_ident_{\bf{list}} = \textbf{Expr}\; in \;\textbf{Expr}\; && \textit{Fonction Recusive} \\
            &|\quad match \; \textbf{Expr}\; with \; \textbf{Match\_Case}\quad \\
            &|\quad if \; \textbf{Expr}\; then \; \textbf{Expr}\; else \; \textbf{Expr}\\
            \\
\bf{UnaryOperator}  \quad :=  \quad &|\quad  not \\
            \\
\bf{BinaryOperator}  \quad :=  \quad &|\quad \&\& \\
        &|\quad || \\
        &|\quad + \\
        &|\quad - \\
        &|\quad / \\
        &|\quad \% \\
        &|\quad * \\
        &|\quad < \\
        &|\quad > \\
        &|\quad \leq \\
        &|\quad \geq \\
        &|\quad = \\
\end{align*}

       
### Filtrage et Motifs
\begin{align*}
    \bf{Match\_Case}  \quad :=  \quad &|\quad  \textbf{Pattern} \rightarrow  \textbf{Expr} \\
                    &|\quad \textbf{Pattern} \rightarrow  \textbf{Expr}\; '|'\; \bf{Match\_Case}  \\
    \\
    \textbf{Pattern} \quad :=  \quad &|\quad   ( \textbf{Pattern} ) \\
                    &|\quad \textbf{Litteral} \\
                    &|\quad basic\_ident \\
                    &|\quad \_ \\
                    &|\quad constructeur\_ident  \\
                    &|\quad constructeur\_ident \; \textbf{Pattern} \\
\end{align*}
\pagebreak

### Types
\begin{align*}
\textbf{Type}    \quad :=  \quad &|\quad  (\textbf{Type}) \\
&|\quad int \\
&|\quad bool \\
&|\quad unit \\
&|\quad \textbf{Type} * \textbf{Type} && \textit{Tuple Type}  \\
&|\quad \textbf{Type} \rightarrow  \textbf{Type} &&  \textit{Lambda Type}  \\
&|\quad vartype && \textit{'a} \\
&|\quad basic\_ident && \textit{Defined Type} \\
&|\quad \textbf{Type}_{\bf{List}} &&  \textit{App Type}  \\
\end{align*}
\newpage

# Semantique de traduction

## Programmes
$$ \vdash_{Prog} Prog[cs] \rightarrow Prog'(x) $$
\begin{align*}
\text{ si }&   \vdash_{\text{Cmds}} cs \rightarrow  (g,\omega,\upsilon)\\
\text{  alors}&   \vdash_{\text{Cmds}} Prog[cs] \rightarrow  Prog'(\omega;Do(g,\upsilon))
\end{align*}

## Suites de commandes
$$ \vdash_{\text{Cmds}} cs \rightarrow (\gamma,\omega,\upsilon) $$
\begin{align*}
\text{(VAR DEFS)  si }& \; d  \in DEF,\\
\text{et si }&  \vdash_{\text{Cmds}} cs \rightarrow (\gamma,\omega,\upsilon)  \\
\text{et si }&  \vdash_{\text{Def}} d \rightarrow  \pi \\
\text{et si }&  \pi \in GLB \\
alors& \vdash_{\text{Cmds}} (Def(d); cs) \rightarrow ((\gamma;\pi),\omega,\upsilon)\\
\\
\text{(TYPE DEFS)  si }& \; d  \in DEF,\\
\text{et si }&  \vdash_{\text{Cmds}} cs \rightarrow (\gamma,\omega,\upsilon)  \\
\text{et si }&  \vdash_{\text{Def}} d \rightarrow  \pi \\
\text{et si }&  \pi \in TYPE \\
alors& \vdash_{\text{Cmds}} (Def(d); cs) \rightarrow (\gamma,(\omega;\pi),\upsilon)\\
\\
\text{(GLB EXPR)} \text{ si }& b  \in EXPR,\\ 
\text{et si }&  \vdash_{\text{Cmds}} cs \rightarrow (\gamma,\omega,\upsilon)  \\
\text{et si }&  \vdash_{\text{Expr}} b \rightarrow  \upsilon' \\
alors&   \vdash_{\text{Cmds}} (Expr(b), cs) \rightarrow  (\gamma,\omega,\upsilon')
\end{align*}

\newpage

## Définitions 

$$ \vdash_{\text{Def}} d \rightarrow \pi $$

\begin{align*}
\text{(VARDEF)} 
\text{ si }&   \vdash_{\text{Expr}} e \rightarrow  e' \\
\text{alors}&   \vdash_{\text{Def}} VariableDef(v, e) \rightarrow  GLB(InsLet(v, e'))\\
\\
\text{(TYPDEF)} \text{ si }&  \vdash c1 \rightarrow  c1' 
\dots \text{ si } \vdash cN \rightarrow  cN' \\
\text{alors}&   \vdash_{\text{Def}} TypeDef(n, [t1,\dots,tn], [c1,\dots,cN]) \\
\rightarrow \; & \text{TYPE}(Typ\_Def(n, [t1,\dots,tn], Def\_Datatype[c1',\dots,cN'])) \\
\end{align*}


## Types
$$ \vdash_{\text{Type}} t \rightarrow t' $$
\begin{align*}
\text{(TINT)}  &\vdash_{Type} TypeInt \rightarrow  TypInt \\
\text{(TBOOL)}  &\vdash_{Type} TypeBool \rightarrow  TypBool \\
\text{(TUNIT)}  &\vdash_{Type} TypeUnit \rightarrow  TypUnit \\
\text{(TDEF)}  &\vdash_{Type} TypeDefined(id) \rightarrow  TypVar(id) \\
\text{(TVAR)}  &\vdash_{Type} TypeVar(id) \rightarrow  TypVar (id)\\
\\
\text{(TTUPLE)} \text{ si }&  \vdash_{Type} t1 \rightarrow  t_1,\dots \text{ et }  \vdash_{Type} tN \rightarrow  t_N \\
\text{alors}&  \vdash_{Type} TypeTuple([t1,\dots, tN]) \rightarrow  TypTuple[t_1,\dots,t_N] \\
\\
\text{(TCONS)} \text{ si }&  \vdash_{Type} t \rightarrow  t'\\ 
\text{ si }&  \vdash_{Type} p1 \rightarrow  p_1,\dots \text{ et } \vdash_{Type} pN \rightarrow  p_N \\  
\text{alors}&  \vdash_{Type} TypeConstructor(t, [p1,\dots,pN]) \rightarrow  TypApp(t, [p_1,\dots,p_N])\\
\\
\\
\text{(TLAMB)} \text{ si }&  \vdash_{Type} a \rightarrow  a' \\ 
\text{et si}&  \vdash_{Type} ret  \rightarrow  ret' \\
\text{alors}&  \vdash_{Type} TypeLambda(a,ret) \\
 \rightarrow \; & TypClosure(Exp, \, (TypFun(TypThunk(ret'), a')))
\end{align*}

## Litteraux et Expressions

$$ \vdash_{\text{Expr}} e \rightarrow e' $$
\begin{align*}
\text{(INT)} \text{ si }& i  \in \text{NUM} \\
\text{alors}&  \vdash_{Expr} Integer(i) \rightarrow  ExprInt(i) \\
\text{(TRUE)} \quad&\vdash_{Expr} Boolean(true) \rightarrow  ExprConstructor(True,[\;]) \\
\text{(FALSE)} \quad&\vdash_{Expr} Boolean(false) \rightarrow  ExprConstructor(False, [\;]) \\
\text{(UNIT)}\quad  & \vdash_{Expr} Unit \rightarrow  ExprConstructor(Unit, [\;])    
\\
\\
\text{(TUPLE)} \text{ si } & \vdash_{Expr} e1 \rightarrow  e_1, \dots \text{ si }  \vdash_{Expr} eN \rightarrow  e_N \\
\text{alors}&  \vdash_{Expr} Tuple([e1,\dots,eN]) \rightarrow  ExprConstructor(Tuple, [e_1,\dots,e_N])
\\
\\
\text{(UNARY1)} \text{ si }&  \vdash_{Expr} a \rightarrow  a'  \\
\text{alors}&  \vdash_{Expr} CallUnary(op, [a]) \rightarrow  ExprMonPrim(op, a') \\
\text{(UNARY0)} \text{ si }&  \vdash_{Expr} Lambda(a,CallUnary(op, [a])) \rightarrow  \omega  \\
\text{alors}&  \vdash_{Expr} CallUnary(op, [\;]) \rightarrow \omega   \\
\\ 
\\
\text{(BINARY2)} \text{ si }&  \vdash_{Expr} a1 \rightarrow  a_1 \text{ et }  \vdash_{Expr} a2 \rightarrow  a_2 \\
\text{alors}&  \vdash_{Expr} CallBinary(op, [a1:a2]) \rightarrow  ExprBinPrim(op, a_1, a_2)\\
\text{(BINARY1)} 
\text{ si }&  \vdash_{Expr} Lambda(b,CallUnary(op, [a,b])) \rightarrow  \omega  \\
\text{alors}&  \vdash_{Expr} CallBinary(op, [a]) \rightarrow \omega \\
\text{(BINARY0)} 
\text{ si }&  \vdash_{Expr} Lambda(a,Lambda(b,CallUnary(op, [a,b]))) \rightarrow  \omega  \\
\text{alors}&  \vdash_{Expr} CallBinary(op, [\;]) \rightarrow \omega \\
\\
\text{(CONSTR)} \text{ si }&  \vdash_{Expr} e \rightarrow  e' \\
\text{alors}&  \vdash_{Expr} Construct(c,e) \rightarrow  ExprConstructor(ConsNamed(c), e') \\
\\
\text{(BIND)} \text{ si }&  \vdash_{Expr} i \rightarrow  i' \\  
\text{et si }&  \vdash_{Expr} c \rightarrow  c'  \\
\text{alors}&  \vdash_{Expr} Binding(v,i,c) \rightarrow  ExprBlock(Blk([InsLet (v, i')], c')) \\
\\
\text{(MATCH)} \text{ si }&  \vdash_{Expr} m \rightarrow  m' \\
\text{ si }& m1  \in CASE \dots \text{ et } mN  \in CASE  \\
\text{ si }&  \vdash_{Case} m1 \rightarrow  m_1, \dots \text{ et } \vdash_{Case} mN \rightarrow  m_N \\
\text{alors}&  \vdash_{Expr} Match(m,[m1,\dots,mN]) \rightarrow  ExprMatch(m', [m_1,\dots,m_N]) \\
\\
\text{(SEQ)} \text{ si }&  \vdash_{Expr} e1 \rightarrow  InsLet(x1, e_1), \dots \text{ et }  \vdash_{Expr} eN-1 \rightarrow  InsLet(xN-1, e_{N-1}) \\
\text{et si }&  \vdash_{Expr} eN \rightarrow  e_N \\
\text{alors}&  \vdash_{Expr} Sequence([e1,\dots,eN]) \\
&\rightarrow  ExprBlock(Blk([InsLet(x1, e_1); \dots ; InsLet(xN-1, e_{N-1})], e_N)) \\
\\
\text{(CALL)} \text{ si }&  \vdash_{Expr} a \rightarrow  a'  \\
\text{et si }&  \vdash_{Expr} f \rightarrow  f'  \\
\text{alors}& \vdash_{Expr} Call(f, a) \\
&\rightarrow  ExprBlock(Blk([InsOpen(Exp, f'), InsForce(ExprMethod(Call, [a']))]))\\
\\
\text{(LAMBDA)} \text{ si }&  \vdash_{Expr} a \rightarrow  a' \\
\text{et si }&  \vdash_{Expr} b \rightarrow  b' \\
\text{alors}&  \vdash_{Expr} Lambda(a, b) \\
&\rightarrow  ExprClosure(Exp, ExprGet([GetPatTag(Call, [a'], ExprThunk(b'))]))\\
\\
\text{(REC)} 
\text{ si }&  \vdash_{Expr} a \rightarrow  a' \\
\text{ si }&  \vdash_{Expr} v \rightarrow  v' \text{ et } \text{ si }  \vdash_{Expr} b \rightarrow  b' \\
\text{alors}&  \vdash_{Expr} FunctionRec(v, a, b)  \\
&\rightarrow  ExprClosure(Exp,ExprRec(v',ExprGet([GetPatTag(Call, [a'], ExprThunk(b'))]))) \\
\end{align*}


## Motifs et Filtrage

<!-- ### Patterns

$$ \vdash_{\text{Patt}} p \rightarrow (p',vars) $$

\begin{align*}
\text{(INTPATT)} \text { si }& i \in NUM \\
\text{ alors }& \vdash_{\text{Patt}} i \rightarrow (IntLitt,[]) \\
\text{(TRUEPATT)} \quad& \vdash_{\text{Patt}} Boolean(true) \rightarrow (True,[]) \\
\text{(FALSEPATT)} \quad& \vdash_{\text{Patt}} Boolean(false) \rightarrow (False,[]) \\
\text{(UNITPATT)} \quad& \vdash_{\text{Patt}} Unit \rightarrow (Unit,[]) \\
\\
\text{(TUPLEPATT)} \quad& \vdash_{\text{Patt}} TuplePatt(VarPattern(v_1);\dots;VarPattern(v_N)) \rightarrow (Tuple,v_1;\dots;v_N) \\
\\
\text{(CONSTRPATT)} \quad& \vdash_{\text{Patt}} ConstructPattern(c,VarPattern(v_1);\dots;VarPattern(v_n) )  \\
&\rightarrow (ConsNamed(c),v_1;\dots;v_N) \\
\text{(VARPATT)} \quad& \vdash_{\text{Patt}} VarPattern(v) \rightarrow (Var,v) \\
\end{align*} -->


$$ \vdash_{\text{Case}} Case(p,e) \rightarrow \alpha $$

\begin{align*}
\text{(INTPAT)} \text{ si }& l \in \text{NUM} \text{ et }  \vdash_{Expr} e \rightarrow  e' \\
\text{alors}&  \vdash_{Case} Case(LitteralPattern(l), e) \rightarrow  
MatchPatTag(IntLitt\; l, [\;], e') \\
\text{(BOOLPAT)} \text{ si }& l = Boolean(\_), \\
\text{ si } & \vdash l \rightarrow  l' \text{ et }  \vdash_{Expr} e \rightarrow  e' \\
\text{alors} & \vdash_{Case} Case(LitteralPattern(l), e) 
\rightarrow  MatchPatTag(l', [\;], e') \\
\text{(UNITPAT)} \text{ si }& l = Unit \text{ et }  \vdash_{Expr} e \rightarrow  e' \\
\text{alors} & \vdash_{Case} Case(LitteralPattern(l), e) \rightarrow  MatchPatTag(Unit, [\;], e') \\
\\
\\
\text{(TUPAT)} 
\text{ si }& p1  \in CASE,\dots \text{ si } pN  \in CASE \\
\text{ si }&  \vdash p1 \rightarrow  p_1, \dots, \text{ si }  \vdash pN \rightarrow  p_N \\ 
\text{ et }&  \vdash_{Expr} e \rightarrow  e' \\
\text{alors}&  \vdash_{Case} Case(TuplePattern([p1,\dots,pN]), e) 
\rightarrow  MatchPatTag(Tuple, [p_1,\dots,p_N], e') \\
\\
\text{(CONSPAT)}  
\text{ si }& c  \in CASE  \\
\text{ si }&  \vdash c \rightarrow  c'  \\
\text{ et }&  \vdash_{Expr} e \rightarrow  e' \\
\text{alors}&  \vdash_{Case} Case(ConstructorPattern((n,c)), e)
\rightarrow  MatchPatTag(ConsNamed(n), c', e') \\
\\
\text{(VARPAT)} \text{ si }&  \vdash_{Expr} e \rightarrow  e' \\
\text{alors}&  \vdash_{Case} Case(VarPattern(x), e, l)
\rightarrow  MatchPatVar((x, l), e', l) \\
\\
\text{(WILDPAT)}
\text{ si }&  \vdash_{Expr} e \rightarrow  e'  \\
\text{alors}&  \vdash_{Case} Case(WildcardPattern(), e, l)
\rightarrow  MatchPatVar((n, l), e', l)
\end{align*}
