{
  open Parser
  open Ast 
  exception Error of string

  let getToken = function
  |"int" -> LType Int_t
  |"bool" -> LType Bool_t
  | a -> Lidentifier a
}

let num = ['0'-'9']*
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let name = ['a'-'z' '_'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"



rule token = parse
| num+ as n       { Lint (int_of_string n) }
| '(' {LOpenPar}
| ')' {LClosePar}
| ";;" {LDoubleSemiColon}
| ';' {LSemiColon}
| ',' {LComma}
| '[' {LLeftBracket}
| ']' {LRightBracket}
| "let" {LLet}
| "fun" {LFun}
| "->" {LSimpleArrow}
| "in" {LIn}
| '=' {LEqual}
| "true" as boolean          {Lbool (bool_of_string boolean) }
| "false" as boolean          {Lbool (bool_of_string boolean) }
| ':' {LColon}
| name as ident {getToken ident}
| white* { token lexbuf }
| newline          { Lexing.new_line lexbuf; token lexbuf }
| eof {EOF}


