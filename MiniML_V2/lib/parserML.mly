%{
  open AstML
  open HelpersML

    let unit_expr pos = 
    { enode = Litteral Unit
    ; eloc = pos
    }
  ;;
;;
%}
%token EOF
%token <int>Lint
%token <bool>Lbool
%token <string>LBasicIdent LConstructorIdent LVarType
%token <AstML.pre_etype>LParseType


%token LOpenPar LClosePar 
%token LSemiColon LDoubleSemiColon
%token LTupleInfixe LConsInfixe 
%token LSimpleArrow
%token LLet LFun LIn LType LRec LOf LMatch LWith LUnderScore

%token LMult LOr LEqual LAnd LAdd LDiv LModulo LSub

%start <prog> prog
%%


%inline variable:
|var = LBasicIdent {
  { basic_ident = var;
    vloc = position $startpos(var) $endpos(var) 
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
              ; init = 
                { eloc = position $startpos($1) $endpos(body)
                  ; enode = Lambda{args;body}   
                }
    }   
  }
}
| LLet;LRec; var = variable; args = nonempty_list(variable);  LEqual; body = expr{
  { dloc = position $startpos($1) $endpos(body);
    dnode = FunctionRecDef{
      var; args;body
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
  ; c_of = 
      [{ etype = TypeUnit
      ; tloc = position $startpos(constructor_ident) $endpos(constructor_ident)
      }]
  ; loc = position $startpos(constructor_ident) $endpos(constructor_ident)
  }
}
| constructor_ident = LConstructorIdent; LOf ; etype_ls = separated_nonempty_list(LMult,etype){
  { constructor_ident
  ; c_of = etype_ls
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
  { enode = Litteral const
  ; eloc = position $startpos(const) $endpos(const)
  }
}
| var = variable {
  { enode = Variable var
  ; eloc = position $startpos(var) $endpos(var)
  }
}
| LOpenPar;func = unaryoperator ; arg = expr; LClosePar{
  { enode = Call {func;args = [arg]}
  ; eloc = position $startpos(func) $endpos(arg)
  }
}
| LOpenPar ; arg1 = expr ; func = binaryoperator ; arg2 = expr;LClosePar {
  { enode = Call {func;args = [arg1;arg2]}
  ; eloc = position $startpos(arg1) $endpos(arg2)
  }
}
| LOpenPar ; func = expr ; args = nonempty_list(expr) ; LClosePar {
  { enode = Call {func = ApplyExpr func;args}
  ; eloc = position $startpos(func) $endpos(args)
  }
}
| LOpenPar ; hd = expr ; LSemiColon; tail = separated_nonempty_list(LSemiColon,expr);LClosePar {
  { enode = Sequence (hd::tail)
  ; eloc = position $startpos(hd) $endpos(tail)
  }
}
| LLet ; var = variable; LEqual; init = expr ; LIn ; content = expr{
  { enode = Binding {var;init;content}
  ; eloc = position $startpos($1) $endpos(content)
  }
}
 | LFun;args = list(variable);LSimpleArrow;body = expr {
  { enode = Lambda
        { args = args
        ; body = body 
        } 
  ; eloc = position $startpos($1) $endpos(body)
  }
} 
| LOpenPar ; hd = expr ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,expr);LClosePar  {
  { enode = Tuple (hd::tail)
  ; eloc = position $startpos(hd) $endpos(tail)
  }
}

/*  | LOpenPar ; hd = expr ; LConsInfixe; tail = separated_nonempty_list(LConsInfixe,expr);LClosePar  {

}  */

| constructor_ident =  LConstructorIdent; LOpenPar ; hd = expr ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,expr);LClosePar {
  { 
    enode = Construct { constructor_ident ; to_group = hd::tail }
  ; eloc = position $startpos(hd) $endpos(tail)
  }
}
| LOpenPar;constructor_ident =  LConstructorIdent;LClosePar  {
  let pos = position $startpos(constructor_ident) $endpos(constructor_ident) in
  {
    enode = Construct { constructor_ident ; to_group = [unit_expr pos] }
  ; eloc = pos
  }
}
| LLet; var = variable; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
    { enode = Binding {
      var ; init = 
              { eloc = position $startpos($1) $endpos(func_body)
              ; enode = Lambda{args;body = func_body}   
              }
      ; content}
      ; eloc = position $startpos($1) $endpos(content)
  }
}
| LLet ; LRec; var = variable; args = nonempty_list(variable);  LEqual; func_body = expr; LIn ;content = expr{
    { enode = Binding {
            var ; init = 
              { eloc = position $startpos($1) $endpos(func_body)
              ; enode = FunctionRec
                  { var
                  ; args
                  ; body = func_body
                  } 
              }
            ; content}
    ; eloc = position $startpos($1) $endpos(content)
  }
}
| LOpenPar;LMatch ; e = expr ;LWith ;option(LOr) ; cases = separated_nonempty_list(LOr,match_case);LClosePar{
   { 
      enode =  Match{ to_match = e ; cases}
    ; eloc = position $startpos($1) $endpos(cases)
  }
} 

match_case :
| pattern = pattern ;LSimpleArrow; consequence = expr{
    { pattern
  ; consequence
  ; conseq_loc = position $startpos(consequence) $endpos(consequence)
  ; pattern_loc = position $startpos(pattern) $endpos(pattern)
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
      ; content = [LitteralPattern Unit]
      }
}
| constructor_ident = LConstructorIdent; LOpenPar ; hd = pattern ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,pattern);LClosePar {
  ConstructorPattern
      { constructor_ident
      ; content = hd::tail
      }
}
| LOpenPar ; hd = pattern ; LTupleInfixe; tail = separated_nonempty_list(LTupleInfixe,pattern);LClosePar  {
  TuplePattern (hd::tail)
}
| LOpenPar ; hd = pattern ; LConsInfixe; tail = separated_nonempty_list(LConsInfixe,pattern);LClosePar  {
    let last,rem = list_getlast_rem tail in
    List.fold_right
    (fun elem acc ->
      ConstructorPattern
        { constructor_ident = "Cons"; content = [ elem; acc ] })
    (hd::rem) last
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
| etype = LParseType {
   {  etype
    ; tloc = position $startpos(etype) $endpos(etype)
  }
} 
| LOpenPar; t = etype ; LClosePar {t}
| LOpenPar ; hd = etype ; LMult; tail = separated_nonempty_list(LMult,etype);LClosePar{
    { etype = TypeTuple (hd::tail)
    ; tloc = position $startpos(hd) $endpos(tail)
    }
}
| LOpenPar;args = nonempty_list(etype)
  ;LSimpleArrow;return_type = etype;LClosePar {
    { etype = TypeLambda {args;return_type}
    ; tloc = position $startpos(args) $endpos(return_type)
    }
}
| t = LVarType {
  { etype = TypeVar t
  ; tloc = position $startpos(t) $endpos(t)
  }
}
| t = LBasicIdent {
  { etype = TypeVar t
  ; tloc = position $startpos(t) $endpos(t)
  }
} 
| LOpenPar; hd = etype ;tail = nonempty_list(etype);  LClosePar{
  let last,rem = list_getlast_rem (hd::tail) in
  { etype = TypeConstructor {to_build = last; parameters = rem}
  ; tloc = position $startpos(hd) $endpos(tail)
  }
  }










