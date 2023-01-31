%{
  open Ast
  open Ast.Syntax
  open Helpers

  let loc_of_expr = function
  | Syntax.Nil { loc; _ } -> loc
  | Syntax.Cons { loc; _ } -> loc
  | Syntax.Seq { loc; _ } -> loc
  | Syntax.Tuple { loc; _ } -> loc
  | Syntax.Const { loc; _ } -> loc
  | Syntax.Binding { loc; _ } -> loc
  | Syntax.Var { loc; _ } -> loc
;;
%}

%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>Lidentifier

%token LOpenPar LClosePar LSemiColon LDoubleSemiColon LComma LLeftBracket LRightBracket
%token LLet LEqual LIn

%start <prog> prog
%%

/* TODO

Decl Var
Call FonctionNative

Type Analysis ?


STD LIB for operators
Intepretor */

prog:
| p = expr; EOF { p }

expr:
| e = value { Const {const = e ; loc = position $startpos(e) $endpos(e) }  }
| LOpenPar ; expr = expr ; LClosePar {expr}
| LOpenPar ; first = expr ; LComma; rest = separated_nonempty_list(LComma,expr) ; LClosePar {
  Tuple{
    content = Array.of_list (first :: rest); 
    loc = Helpers.position $startpos($1) $endpos($5)
  }
}
| LOpenPar ; first = expr ; LSemiColon; rest = separated_nonempty_list(LSemiColon,expr) ; LClosePar {
  Seq{
    content = Array.of_list (first :: rest);
    loc = Helpers.position $startpos(first) $endpos(rest)
  }
}

| LLeftBracket; ls = separated_list(LComma,expr) ;LRightBracket{
  List.fold_right (fun expr acc -> Cons{hd = expr; tail = acc ; loc = loc_of_expr expr} ) ls 
    (Nil {loc = Helpers.position $startpos(ls) $endpos(ls)})
}

| v_name = Lidentifier {Var {var_name = v_name;loc = Helpers.position $startpos(v_name) $endpos(v_name)}}
| LLet;v_name = Lidentifier;LEqual;init = expr; LIn ; body = expr {
  Binding
        { var_name = v_name
        ; init = init
        ; content = body
        ; loc = Helpers.position $startpos(v_name) $endpos(v_name)
        }
}

%inline value:
|nb = Lint {Integer nb}
|bo = Lbool {Boolean bo}