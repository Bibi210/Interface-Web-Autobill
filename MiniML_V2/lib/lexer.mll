{
  open Parser
  open Ast 
  exception Error of string

  let getToken = function
  |"int" -> LType TypeInt
  |"bool" -> LType TypeBool
  |"unit" -> LType TypeUnit
  | a -> LBasicIdentifier a
}

let num = ('-')?['0'-'9']*
let bool = ("true"|"false")
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let basic_ident = ['a'-'z' '_'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let vartype = ['`']['t']['0'-'9']*

rule token = parse
| num+ as n       { Lint (int_of_string n) }
| '(' {LOpenPar}
| ')' {LClosePar}
| ";;" {LDoubleSemiColon}
| ';' {LSemiColon}
| ',' {LComma}
| '[' {LLeftAngleBracket}
| ']' {LRightAngleBracket}
| "let" {LLet}
| "fun" {LFun}
| "->" {LSimpleArrow}
| "in" {LIn}
| '=' {LEqual}
| ':' {LColon}
| '*' {LMult}
| bool as boolean          {Lbool (bool_of_string boolean) }
| vartype as usertype {LType  (TypeCustom usertype)}
| basic_ident as ident {getToken ident}
| white* { token lexbuf }
| newline          { Lexing.new_line lexbuf; token lexbuf }
| eof {EOF}


