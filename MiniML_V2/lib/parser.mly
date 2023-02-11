%{
  open Ast
  open Helpers

  let generate_vartype =
  let generatedCounter = ref 0 in
  fun () ->
    incr generatedCounter;
    TypeVar ("Unkown" ^ Int.to_string !generatedCounter)
;;

;;
%}

%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>LBasicIdent LConstructorIdent LVarType
%token <Ast.etype>LParseType


%token LOpenPar LClosePar LLeftAngleBracket LRightAngleBracket 
%token LColon LSemiColon LDoubleSemiColon LComma 
%token LSimpleArrow
%token LLet LFun LIn  LType LRec LOf 

%token LMult LOr LEqual LAnd LAdd LDiv LModulo LSub LTilde

%left LOr 
%left LAnd 
%left LAdd LSub
%left LMult LDiv LModulo 

%start <prog> prog
%%


%inline variable:
|var = LBasicIdent {
  { basic_ident = var;
    expected_type = generate_vartype ()
  }
}
|var = LBasicIdent ; LColon ; etype = etype{
  { basic_ident = var;
    expected_type = etype 
  }
}

prog:
| result = separated_nonempty_list(LDoubleSemiColon,prog_node) ; EOF {result}



prog_node:
| e = expr {Expr e}
| d = def {Def d}

def:
| LLet; var = variable ; LEqual; init = expr{
  {
    dloc = position $startpos($1) $endpos(init);
    dnode = VariableDef{
      var = var;
      init = init
    }
  }
}
| LLet; basic_ident = LBasicIdent; args = nonempty_list(variable);  LEqual; body = expr{
  { dloc = position $startpos($1) $endpos(body);
    dnode = FunctionDef{
      basic_ident; args;body
    }   
  }
}
| LLet;LRec; basic_ident = LBasicIdent; args = nonempty_list(variable);  LEqual; body = expr{
  { dloc = position $startpos($1) $endpos(body);
    dnode = FunctionRecDef{
      basic_ident; args;body
    }   
  }
}
| LType; parameters = list(LVarType);basic_ident = LBasicIdent ; LEqual ; 
  option(LOr) ;constructors = separated_nonempty_list(LOr,newconstructor_case){
    { dloc = position $startpos($1) $endpos(basic_ident);
      dnode = TypeDef{
      basic_ident; 
      parameters;
      constructors
    }   
  }
  }

newconstructor_case:
| constructor_ident = LConstructorIdent{
  { constructor_ident
  ; c_of = TypeUnit
  ; loc = position $startpos(constructor_ident) $endpos(constructor_ident)
  }
}
| constructor_ident = LConstructorIdent; LOf ; etype = etype{
  { constructor_ident
  ; c_of = etype
  ; loc = position $startpos(constructor_ident) $endpos(constructor_ident)
  }
}

%inline litteral:
| nb = Lint {
  { etype = TypeInt
  ; enode = Litteral (Integer nb)
  ; eloc = position $startpos(nb) $endpos(nb)
  }
}
| b = Lbool {
  { etype = TypeBool
  ; enode = Litteral (Boolean b)
  ; eloc = position $startpos(b) $endpos(b)
  }
}
| LOpenPar; LClosePar {
  { etype = TypeUnit
  ; enode = Litteral (Unit)
  ; eloc = position $startpos($1) $endpos($2)
  }
}
/* TODO FIX Apply + Unary + Binary
   */
expr:
| LOpenPar; e = expr ; LClosePar {e}
| const = litteral {const}
| var = variable {
  { etype = var.expected_type 
  ; enode = Variable var.basic_ident
  ; eloc = position $startpos(var) $endpos(var)
  }
}
/* | app = unaryoperator {app} 
  | app = binaryoperator {app}  */
| func = expr ; arg = expr {
  { etype =  generate_vartype ()
  ; enode = Call {func;arg}
  ; eloc = position $startpos(func) $endpos(arg)
  }
}


etype:
| t = LParseType {t} 
| LOpenPar; t = etype ; LClosePar {t}
| a = etype ; LMult; b = etype{
    TypeTuple {first = a; second = b}
}
| LOpenPar;arg = etype;LSimpleArrow;return_type = etype;LClosePar {
  TypeLambda {arg;return_type}
}
| t = LVarType {TypeVar t}
| t = LBasicIdent {TypeVar t} 
| LOpenPar;t1 = etype; t2 = etype ;LClosePar{TypeConstructor [t1;t2]}











