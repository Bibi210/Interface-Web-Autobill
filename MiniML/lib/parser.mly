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
  | Syntax.Lambda { loc; _ } -> loc
;;
%}

%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>Lidentifier
%token <Ast.types>LType


%token LOpenPar LClosePar LColon LSemiColon LDoubleSemiColon LComma LLeftBracket LRightBracket
%token LLet LEqual LIn LFun LSimpleArrow


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
| e = value_parse { Const {const = e ; loc = position $startpos(e) $endpos(e) }  }
| LOpenPar ; expr = expr ; LClosePar {expr}
| LOpenPar ; first = expr ; LComma; rest = separated_nonempty_list(LComma,expr) ; LClosePar {
  Tuple{
    content = Array.of_list (first :: rest); 
    loc = position $startpos($1) $endpos($5)
  }
}
| LOpenPar ; first = expr ; LSemiColon; rest = separated_nonempty_list(LSemiColon,expr) ; LClosePar {
  Seq{
    content = Array.of_list (first :: rest);
    loc = position $startpos(first) $endpos(rest)
  }
}

| LLeftBracket; ls = separated_list(LComma,expr) ; LRightBracket {
  
  List.fold_right (fun expr acc -> Cons{hd = expr; tail = acc ; loc = loc_of_expr expr}) ls 
  (Nil {loc = position $startpos(ls) $endpos(ls)})
}

| ident = var_parse {
  Var { ident = ident
      ; loc = position $startpos(ident) $endpos(ident)
  }
}
| LLet;ident = var_parse;LEqual;init = expr; LIn ; body = expr {
  Binding
        { ident = ident
        ; init = init
        ; content = body
        ; loc = position $startpos($3) $endpos($3)
        }
}
| LFun;args = list(var_parse);LSimpleArrow;body = expr {
   Lambda
        { args = args
        ; body = body 
        ; loc = position $startpos($1) $endpos($1)
        }
}


 var_parse:
| v_name = Lidentifier;LColon;vtype = type_parse{
  {name = v_name;type_t=Some vtype}
}
| v_name = Lidentifier{
  {name = v_name;type_t=None}
}



type_parse:
|t = LType {t}
|LOpenPar ; args_type = list(LType);LSimpleArrow;returntype = LType ; LClosePar{
  Lambda (args_type,returntype)
}


%inline value_parse:
|nb = Lint {Integer nb}
|bo = Lbool {Boolean bo}

