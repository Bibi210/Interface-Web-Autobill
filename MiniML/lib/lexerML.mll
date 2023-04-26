{
  open ParserML
  open AstML
  exception Error of string

  let getToken = function
  |"int" -> LParseType TypeInt
  |"bool" -> LParseType TypeBool
  |"unit" -> LParseType TypeUnit
  | a -> LBasicIdent a
}

let num = ('-')?['0'-'9']*
let bool = ("true"|"false")
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let basic_ident = ['a'-'z' '_'] alphanum

let constructeur_ident = ['A'-'Z'] alphanum
let constructeur_infixes = ("::"|',')

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let vartype = [''']['a'-'z']['0'-'9']*

rule token = parse
| '(' {LOpenPar}
| ')' {LClosePar}
| ";;" {LDoubleSemiColon}
| ';' {LSemiColon}
| ',' {LTupleInfixe}
| "::" {LConsInfixe}
| "let" {LLet}
| "rec" {LRec}
| "fun" {LFun}
| "not" {LNot}
| "->" {LSimpleArrow}
| "in" {LIn}
| "of" {LOf}
| "type" {LType}
| '=' {LEqual}
| '*' {LMult}
| '|' {LOr}
| "or" {LTOr}
| "and" {LTAnd}
| '&' {LAnd}
| '+' {LAdd} 
| '/' {LDiv} 
| '%' {LModulo} 
| '-' {LSub} 
| '<' {LInf}
| "match" {LMatch}
| "with" {LWith}
| '_' {LUnderScore}
| '[' {LLeftAngleBraket} 
| ']' {LRightAngleBraket}
| "if" {LIf}
| "then" {LThen}
| "else" {LElse}
| num+ as n       { Lint (int_of_string n) }
| bool as boolean          {Lbool (bool_of_string boolean) }
| vartype as usertype {LVarType (List.nth (String.split_on_char '\'' usertype) 1)}
| basic_ident as ident {getToken ident}
| constructeur_ident as cident {LConstructorIdent cident}
| white* { token lexbuf }
| newline          { Lexing.new_line lexbuf; token lexbuf }
| "(*" {multi_line_comment lexbuf}
| eof {EOF}

and multi_line_comment = parse
| eof  { raise (Error "Never Ending Comment") }
| "*)" { token lexbuf }
| white* { multi_line_comment lexbuf }
| newline          { Lexing.new_line lexbuf; multi_line_comment lexbuf }
| _    { multi_line_comment lexbuf }
