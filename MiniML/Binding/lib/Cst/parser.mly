%{
    open Constructors
    open Types
    open Cst
    open Misc
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token COLUMN PLUS EQUAL MINUS DOT ARROW COMMA SLASH META BAR UNDERSCORE
%token LPAREN RPAREN LANGLE RANGLE
%token UUNIT ZZERO TTOP BBOTTOM FFUN TTHUNK CCLOSURE FFIX BBOOL IINT
%token VAL STK CMD BIND BINDCC MATCH RET END IN
%token TUPPLE INJ CALL PROJ LEFT RIGHT YES NO THIS FIX WITH TRUE FALSE INT
%token GOT_TOP GOT_ZERO
%token BOX UNBOX LLINEAR AAFFINE EEXP
%token UNIT FUN THUNK CLOSURE STAR AMPER
%token DECL TYPE DATA COMPUT SORT
%token <string> VAR
%token <string> TCONS
%token <int> NUM
%token EOF

%start <program> prog
%%

(* Généralités  *)

tvar:
  | name = TCONS META? {name}

consvar:
  | name = VAR META? {name}

destrvar:
  | name = VAR META? {name}

var:
  | name = VAR META? {name}

covar:
  | name = VAR META? {name}

sortvar:
  | name = VAR META? {name}

rel:
  | name = VAR META? {name}

pol:
  | PLUS {positive}
  | MINUS {negative}

sort:
  | pol = pol {sort_base pol}
  | so = sortvar {sort_idx so}
  | LPAREN s = sort ARROW t = sort RPAREN {sort_arrow [s] t}

boxkind:
  | LLINEAR {linear}
  | AAFFINE {affine}
  | EEXP {exp}

(* Binders *)

typ_annot:
  | COLUMN typ = typ {Some typ}
  | COLUMN META {None}
  | {None}

pol_annot:
  | pol = pol {Some pol}
  | META {None}
  | {None}

paren_typed_var:
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_var:
  | v = var typ = typ_annot { (v, typ) }
  | bind = paren_typed_var {bind}

paren_typed_covar:
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_covar:
  | var = covar typ = typ_annot { (var, typ) }
  | bind = paren_typed_covar {bind}

sorted_tyvar:
  | v = tvar  {(v , None)}
  | v = tvar COLUMN sort = sort {(v , Some sort)}
  | LPAREN v = tvar sort = sort RPAREN {(v , Some sort)}

paren_sorted_tyvar_def:
  | LPAREN v = tvar COLUMN sort = sort RPAREN {(v , sort)}

sorted_tyvar_def:
  | v = tvar COLUMN sort = sort {(v , sort)}
  | a = paren_sorted_tyvar_def {a}


(* Types *)

typ:
  | t = delim_typ {t}
  | t = infix_tycons_app {t}
  | c = TCONS args = nonempty_list(delim_typ) {app (tvar c) args}
  | FFUN LPAREN args = separated_list(COMMA,typ) RPAREN ARROW ret = typ {func (ret::args)}

delim_typ:
  | LPAREN t = typ RPAREN {t}
  | IINT {cons Int}
  | BBOOL {cons Bool}
  | UUNIT {unit_t}
  | ZZERO {zero}
  | TTOP {top}
  | BBOTTOM {bottom}
  | kind = boxkind content = delim_typ
    {boxed ~loc:(position $symbolstartpos $endpos) kind content}
  | FFIX a = delim_typ {fix a}
  | TTHUNK a = delim_typ {thunk_t a}
  | CCLOSURE a = delim_typ {closure_t a}
  | var = tvar
    {tvar ~loc:(position $symbolstartpos $endpos) var}

infix_tycons_app:
  | h = delim_typ STAR t = separated_nonempty_list(STAR,delim_typ) {prod (h::t) }
  | h = delim_typ PLUS t = separated_nonempty_list(PLUS,delim_typ) {sum (h::t) }
  | h = delim_typ AMPER t = separated_nonempty_list(AMPER,delim_typ) {choice (h::t) }

args_paren(X):
  | LPAREN items = separated_list(COMMA, X) RPAREN {items}

or_underscore(X):
  | x = X {Some x}
  | UNDERSCORE {None}

private_args(X):
  | LANGLE items = separated_list(COMMA, X) RANGLE {items}
  | {[]}

eqn:
  | a = delim_typ EQUAL b = delim_typ {Eq (a,b,())}
  | rel = rel LPAREN args = separated_list(COMMA,typ) RPAREN {Rel (rel, args)}

eqns:
  | WITH eqns = separated_list(COMMA,eqn) {eqns}
  | {[]}

(* Terms *)

cmd:
  | CMD pol = pol_annot typ = typ_annot VAL EQUAL valu = value STK EQUAL stk = stack END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | CMD pol = pol_annot typ = typ_annot STK EQUAL stk = stack VAL EQUAL valu = value END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | valu = value DOT stk = stk_trail
    {cmd ~loc:(position $symbolstartpos $endpos) None valu stk}

  | VAL pol = pol_annot bind = typed_var EQUAL v = value IN c = cmd
    {let x,annot = bind in
     cmd_let_val ~loc:(position $symbolstartpos $endpos) ?pol:pol x annot v c }
  | STK pol = pol_annot bind = typed_covar EQUAL stk = stack IN c = cmd
    {let a, annot = bind in
     cmd_let_env ~loc:(position $symbolstartpos $endpos) ?pol:pol a annot stk c }
  | MATCH pol = pol_annot cons = cons EQUAL valu = value IN c = cmd
    {cmd_match_val ~loc:(position $symbolstartpos $endpos) ?pol:pol valu cons c }
  | MATCH pol = pol_annot STK THIS DOT destr = destr EQUAL stk = stack IN c = cmd
    {cmd_match_env ~loc:(position $symbolstartpos $endpos) ?pol:pol stk destr c}

value:
  | v = var
    {V.var ~loc:(position $symbolstartpos $endpos) v}
  | GOT_TOP
    {V.cotop ~loc:(position $symbolstartpos $endpos) ()}
  | c = value_cons
    {V.cons ~loc:(position $symbolstartpos $endpos) c}
  | BOX LPAREN kind = boxkind RPAREN a = typed_covar ARROW cmd = cmd
    {let a,t = a in V.box ~loc:(position $symbolstartpos $endpos) kind a t cmd}
  | BINDCC pol = pol_annot a = typed_covar ARROW cmd = cmd
    {let (a,t) = a in V.bindcc ~loc:(position $symbolstartpos $endpos) ?pol:pol a t cmd}

  | MATCH THIS DOT FIX LPAREN self = typed_var RPAREN DOT RET cont = paren_typed_covar
    typ_annot ARROW cmd = cmd
    {Fix {self; cmd; cont; loc = (position $symbolstartpos $endpos)}}
  | MATCH patt = copatt
    {V.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH BAR patts = separated_list(BAR,copatt) END
    {V.case ~loc:(position $symbolstartpos $endpos) patts}

  | FUN LPAREN args = separated_list(COMMA, typed_var) RPAREN ARROW v = value
    {V.macro_fun ~loc:(position $symbolstartpos $endpos) args v}
  | BOX LPAREN kind = boxkind COMMA v = value RPAREN
    {V.macro_box ~loc:(position $symbolstartpos $endpos) kind v}


copatt:
  | THIS DOT destr = destr ARROW cmd = cmd { (destr, cmd) }

destr:
  | cons = destrvar
    privates = private_args(or_underscore(sorted_tyvar))
    args = args_paren(typed_var)
    DOT RET cont = paren_typed_covar
    { negcons cons privates args cont }
  | CALL LPAREN vars = separated_list(COMMA, typed_var) RPAREN DOT RET cont = paren_typed_covar
    {Call (vars, cont)}
  | YES  LPAREN RPAREN                 DOT RET cont = paren_typed_covar {yes cont}
  | NO   LPAREN RPAREN                 DOT  RET cont = paren_typed_covar {no cont}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT RET cont = paren_typed_covar {Proj (i,n,cont)}
  | CLOSURE LPAREN RPAREN            DOT RET cont = paren_typed_covar {closure cont}

value_cons:
  | UNIT LPAREN RPAREN { unit }
  | INT LPAREN n = NUM RPAREN { Int n }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | TUPPLE LPAREN xs = separated_list(COMMA, value) RPAREN {Tupple xs}
  | LEFT LPAREN a = value RPAREN {left a}
  | RIGHT LPAREN b = value RPAREN {right b}
  | INJ LPAREN i = NUM SLASH n = NUM COMMA a = value RPAREN {Inj (i,n,a)}
  | THUNK LPAREN a = value RPAREN {thunk a}
  | cons = consvar
    privates = private_args(or_underscore(typ))
    args = args_paren(value)
    {poscons cons privates args}

stack:
  | THIS DOT stk = stk_trail {stk}

stk_trail:
  | GOT_ZERO LPAREN RPAREN
    {S.cozero ~loc:(position $symbolstartpos $endpos) ()}
  | RET LPAREN a = covar RPAREN
    {S.ret ~loc:(position $symbolstartpos $endpos) a}
  | CALL LPAREN xs = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Call (xs, stk))}
  | YES LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (yes stk)}
  | NO LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (no stk)}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Proj (i,n, stk))}
  | CLOSURE LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (closure stk)}
  | cons = destrvar
    privates = private_args(or_underscore(typ))
    args = args_paren(value)
    DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (negcons cons privates args stk)}
  | UNBOX LPAREN kind = boxkind RPAREN DOT stk = stk_trail
    {S.box ~loc:(position $symbolstartpos $endpos) kind stk}
  | FIX LPAREN RPAREN DOT stk = stk_trail
    {CoFix {loc = (position $symbolstartpos $endpos); stk}}
  | BIND pol = pol_annot x = typed_var ARROW cmd = cmd
    {let (x,t) = x in S.bind ~loc:(position $symbolstartpos $endpos) ?pol:pol x t cmd}
  | MATCH patt = patt
    {S.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH BAR patts = separated_list(BAR,patt) END
    {S.case ~loc:(position $symbolstartpos $endpos) patts}

patt:
  | cons = cons ARROW cmd = cmd { (cons, cmd) }

cons:
  | cons = consvar
    privates = private_args(or_underscore(sorted_tyvar))
    args = args_paren(typed_var)
    { poscons cons privates args }
  | UNIT {unit}
  | INT LPAREN n = NUM RPAREN { Int n }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | TUPPLE LPAREN vs = separated_list(COMMA, typed_var) RPAREN { Tupple vs }
  | LEFT LPAREN a = typed_var RPAREN {left a}
  | RIGHT LPAREN b = typed_var RPAREN {right b}
  | INJ LPAREN i = NUM SLASH n = NUM COMMA a = typed_var RPAREN { Inj (i,n,a) }
  | THUNK LPAREN a = typed_var RPAREN {thunk a}


(* Méta-langage *)

data_cons_def:
  | cons = consvar
    privates = private_args(sorted_tyvar_def)
    args = args_paren(typ)
    equations = eqns
    {poscons cons privates args, equations}

codata_cons_def:
  | THIS DOT cons = destrvar
    privates = private_args(sorted_tyvar_def)
    args = args_paren(typ)
    DOT RET LPAREN typ = typ RPAREN
    equations = eqns
    {negcons cons privates args typ, equations }

prog:
  | EOF {[]}
  | first = prog_item rest = prog {(first :: rest)}

prog_item:
  | DECL SORT name = sortvar
    {Sort_declaration {name; loc = position $symbolstartpos $endpos}}

  | DECL TYPE name = tvar COLUMN sort = sort
    {Type_declaration {name; sort;loc = position $symbolstartpos $endpos}}

  | TYPE name = tvar args = list(paren_sorted_tyvar_def) COLUMN sort = sort EQUAL content = typ
    {Type_definition {name;args;sort;content;loc = position $symbolstartpos $endpos}}

  | DATA name = tvar args = list(paren_sorted_tyvar_def) EQUAL
    BAR? content = separated_nonempty_list(BAR, data_cons_def)
    { Data_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  | COMPUT name = tvar args = list(paren_sorted_tyvar_def) EQUAL
    BAR? content = separated_nonempty_list(BAR, codata_cons_def)
    { Codata_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  | CMD META? name = option(var) RET cont = typed_covar EQUAL content = cmd
    { let cont, typ = cont in
      Cmd_execution { name; content; cont; typ; loc = position $symbolstartpos $endpos} }

  | VAL META? name = var typ = typ_annot EQUAL content = value
    { Term_definition {name;typ;content;loc = position $symbolstartpos $endpos} }

  | DECL VAL META? name = var COLUMN typ = typ
    { Term_declaration {name;typ;loc = position $symbolstartpos $endpos} }
