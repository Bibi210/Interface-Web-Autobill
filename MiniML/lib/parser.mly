%{
  open Ast
%}

%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>Lidentifier
/* Parenthesis */
%token LOpenPar LClosePar LSemiColon LDoubleSemiColon LComma


%start <prog> prog
%%

/* TODO

Decl Var
Call FonctionNative

Type Analysis ?


STD LIB for operators
Intepretor */

prog:
| p = separated_list(LDoubleSemiColon,expr); EOF { p }

expr:
| p_val = value; { 
  Value {
    value = p_val;
    loc = Helpers.position $startpos(p_val) $endpos(p_val)
    }
  }

| func_name = Lidentifier; LOpenPar ;args = nonempty_list(expr) LClosePar {
  Func_Call
      { func = func_name
      ; args = args
      ; loc = Helpers.position $startpos(func_name) $endpos(args)
      }
} 

| LOpenPar ; first = expr ; LComma; rest = separated_nonempty_list(LComma,expr) ; LClosePar {
  Tuple{
    value = first :: rest; 
    loc = Helpers.position $startpos($1) $endpos($5)
  }
}
| LOpenPar ; expr = expr ; LClosePar {expr}

%inline value:
|nb = Lint {Integer nb}
|bo = Lbool {Boolean bo}