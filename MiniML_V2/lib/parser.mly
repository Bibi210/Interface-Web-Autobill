%{
  open Ast
  open Helpers
  let unit_expr pos = 
   { etype =  None
    ; enode = Litteral Unit
    ; eloc = pos

  }
  ;;


;;
%}
%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>LBasicIdent LConstructorIdent LVarType
%token <Ast.etype>LParseType


%token LOpenPar LClosePar 
%token LColon LSemiColon LDoubleSemiColon
%token LTupleInfixe LConsInfixe 
%token LSimpleArrow
%token LLet LFun LIn LType LRec LOf LMatch LWith LUnderScore

%token LMult LOr LEqual LAnd LAdd LDiv LModulo LSub

%start <prog> prog
%%


%inline variable:
|var = LBasicIdent {
  { basic_ident = var;
    expected_type = None
  }
}
|var = LBasicIdent ; LColon ; etype = etype{
  { basic_ident = var;
    expected_type = Some etype 
  }
}

prog:
| result = separated_nonempty_list(LDoubleSemiColon,prog_node) ; EOF{
  result
}


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
| LLet; var = variable; args = nonempty_list(variable);  LEqual; body = expr{
  { dloc = position $startpos($1) $endpos(body);
    dnode = VariableDef{
              var
              ; init = { eloc = position $startpos($1) $endpos(body)
                        ; enode = Lambda{args;body}   
                        ; etype = None
                      }
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
| nb = Lint { Integer nb }
| b = Lbool { Boolean b}
| LOpenPar;LClosePar {Unit}

expr:
| LOpenPar; e = expr ; LClosePar {e}
| const = litteral {
  { etype = None
  ; enode = Litteral const
  ; eloc = position $startpos(const) $endpos(const)
  }
}
| var = variable {
  { etype = None
  ; enode = Variable var
  ; eloc = position $startpos(var) $endpos(var)
  }
}
| LOpenPar;func = unaryoperator ; arg = expr; LClosePar{
  {  etype = None
  ; enode = Call {func;args = [arg]}
  ; eloc = position $startpos(func) $endpos(arg)
  }
}
| LOpenPar ; arg1 = expr ; func = binaryoperator ; arg2 = expr;LClosePar {
  {  etype = None
  ; enode = Call {func;args = [arg1;arg2]}
  ; eloc = position $startpos(arg1) $endpos(arg2)
  }
}
| LOpenPar ; func = expr ; args = nonempty_list(expr) ; LClosePar {
  { etype = None
  ; enode = Call {func = ApplyExpr func;args}
  ; eloc = position $startpos(func) $endpos(args)
  }
}
| LOpenPar ; hd = expr ; LSemiColon; tail = separated_nonempty_list(LSemiColon,expr);LClosePar {
  { etype =  None
  ; enode = Sequence (hd::tail)
  ; eloc = position $startpos(hd) $endpos(tail)
  }
}
| LLet ; var = variable; LEqual; init = expr ; LIn ; content = expr{
  { etype =  None
  ; enode = Binding {var;init;content}
  ; eloc = position $startpos($1) $endpos(content)
  }
}
 | LFun;args = list(variable);LSimpleArrow;body = expr {
  { etype = None
  ; enode = Lambda
        { args = args
        ; body = body 
        } 
  ; eloc = position $startpos($1) $endpos(body)
  }
} 
| LOpenPar ; hd = expr ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,expr);LClosePar  {
  { etype =  None
  ; enode = Tuple (hd::tail)
  ; eloc = position $startpos(hd) $endpos(tail)
  }
}

/*  | LOpenPar ; hd = expr ; LConsInfixe; tail = separated_nonempty_list(LConsInfixe,expr);LClosePar  {

}  */

| LOpenPar; constructor_ident =  LConstructorIdent ; to_group = expr ;LClosePar {
  { etype =  None
  ; enode = Construct { constructor_ident ; to_group }
  ; eloc = position $startpos(constructor_ident) $endpos(to_group)
  }
}
| LOpenPar;constructor_ident =  LConstructorIdent;LClosePar  {
  let pos = position $startpos(constructor_ident) $endpos(constructor_ident) in
  { etype =  None
  ; enode = Construct { constructor_ident ; to_group = unit_expr pos }
  ; eloc = pos
  }
}
| LLet; basic_ident = LBasicIdent; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
    { etype =  None
    ; enode = Binding {
            var = {
              basic_ident
            ; expected_type  = None
            } 
            ; init = 
              { eloc = position $startpos($1) $endpos(func_body)
              ; enode = Lambda{args;body = func_body}   
              ; etype = None
              }
            ; content}
    ; eloc = position $startpos($1) $endpos(content)
  }
}
| LLet ; LRec; basic_ident = LBasicIdent; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
    { etype =  None
    ; enode = Binding {
            var = {
              basic_ident
            ; expected_type  = None
            } 
            ; init = 
              { eloc = position $startpos($1) $endpos(func_body)
              ; enode = FunctionRec
                  { basic_ident
                  ; args
                  ; body = func_body
                  } 
              ; etype = None
              }
            ; content}
    ; eloc = position $startpos($1) $endpos(content)
  }
}
| LOpenPar;LMatch ; e = expr ;LWith ;option(LOr) ; cases = separated_nonempty_list(LOr,match_case);LClosePar{
   { etype =  None
    ; enode =  Match{ to_match = e ; cases}
    ; eloc = position $startpos($1) $endpos(cases)
  }
} 

match_case :
| pattern = pattern ;LSimpleArrow; consequence = expr{
    { pattern
  ; consequence
  ; cloc = position $startpos(pattern) $endpos(consequence)
  }
}


pattern :
| LOpenPar ; p = pattern ; LClosePar {p}
| l = litteral  {LitteralPattern l}
| ident = LBasicIdent {VarPattern ident }
| LUnderScore {WildcardPattern}
| constructor_ident = LConstructorIdent {
  ConstructorPattern
      { constructor_ident
      ; content = LitteralPattern Unit
      }
}
| constructor_ident = LConstructorIdent;  p = pattern {
  ConstructorPattern
      { constructor_ident
      ; content = p
      }
}
| LOpenPar ; hd = pattern ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,pattern);LClosePar  {
  TuplePattern (hd::tail)
}
| LOpenPar ; hd = pattern ; LConsInfixe; tail = separated_nonempty_list(LConsInfixe,pattern);LClosePar  {
    List.fold_left
    (fun acc elem ->
      ConstructorPattern
        { constructor_ident = "Cons"; content = TuplePattern [ elem; acc ] })
    (ConstructorPattern { constructor_ident = "Nil"; content = LitteralPattern Unit }) (hd::tail)
}


%inline unaryoperator:
|LSub { Sub }

%inline binaryoperator:
|LSub { Sub }
|LDiv { Div }
|LModulo { Modulo }
|LAdd { Add }
|LAnd{ BitAnd }
|LAnd;LAnd { And }

|LOr;LOr {Or}




etype:
| t = LParseType {t} 
| LOpenPar; t = etype ; LClosePar {t}
| LOpenPar ; hd = etype ; LMult; tail = separated_nonempty_list(LMult,etype);LClosePar{
    TypeTuple (hd::tail)
}
| LOpenPar;args = nonempty_list(etype)
  ;LSimpleArrow;return_type = etype;LClosePar {
  TypeLambda {args;return_type}
}
| t = LVarType {TypeVar t}
| t = LBasicIdent {TypeVar t} 
| LOpenPar;t1 = etype; t2 = etype ; LClosePar{TypeConstructor [t1;t2]}











