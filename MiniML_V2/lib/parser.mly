%{
  open Ast
  open Helpers


;;
%}

%token EOF
%token <int>Lint
%token <bool>Lbool
%token LUnit
%token <string>LBasicIdentifier
%token <Ast.etype>LType


%token LOpenPar LClosePar LLeftAngleBracket LRightAngleBracket 
%token LColon LSemiColon LDoubleSemiColon LComma 
%token LSimpleArrow LMult LOr LEqual
%token LLet  LIn LFun 


%start <prog> prog
%%

prog:
| result = separated_nonempty_list(LDoubleSemiColon,prog_node) ; EOF{
  result
}

prog_node:
| e = expr {Expr e}

etype:
| t = LType {t} 
| LOpenPar; t = etype ; LClosePar {t}
| a = etype ; LMult; b = etype{
    TypeTuple {first = a; second = b}
}
| LOpenPar;args_types = nonempty_list(etype);LSimpleArrow;return_type = etype;LClosePar {
  TypeLambda {args_types;return_type}
}

%inline litteral:
| nb = Lint {Litteral (Integer  nb) }
| b = Lbool {Litteral (Boolean b) } 
| LOpenPar; LClosePar {Litteral Unit}
| LLeftAngleBracket ; LRightAngleBracket {
   Construct {constructor_name = "Nil" ; to_group = []}
}

%inline variable:
|var = LBasicIdentifier {
  { etype = None
  ; node = Variable var
  ; loc = position $startpos(var) $endpos(var)
  }
}
|var = LBasicIdentifier ; LColon ; etype = etype{
  { etype = Some etype
  ; node = Variable var
  ; loc = position $startpos(var) $endpos(var)
  }
}

expr:
| LOpenPar; e = expr ; LClosePar {e}
| const = litteral { 
  { etype = Some TypeInt 
  ; node = const
  ; loc = position $startpos(const) $endpos(const)
  }
}
| var = variable {var}
/* TODO FIX */
| e1 = expr; LSemiColon ; e2 = expr{
  
  { etype = None  
  ; node = Tuple {first = e1; second = e2}
  ; loc = position $startpos(e1) $endpos(e2)
  }
  
}










