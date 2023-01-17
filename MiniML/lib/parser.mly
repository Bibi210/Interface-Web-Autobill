%{
  open Ast
%}

%token EOF
%token <int>Lint
%token <bool>Lbool

/* Parenthesis */
%token LOpenPar LClosePar LSemiColon LComma


%start <prog> prog
%%

prog:
| p = separated_list(LSemiColon,expr); EOF { p }

expr:
| p_val = value; { 
  Value {
    value = p_val;
    loc = Helpers.position $startpos(p_val) $endpos(p_val)
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