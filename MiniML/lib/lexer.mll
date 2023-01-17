{
  open Parser
  exception Error of string
}

let num = ['0'-'9']*
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let name = ['a'-'z'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
| num+ as n       { Lint (int_of_string n) }
| '(' {LOpenPar}
| ')' {LClosePar}
| ';' {LSemiColon}
| ',' {LComma}
| "true" as boolean          {Lbool (bool_of_string boolean) }
| "false" as boolean          {Lbool (bool_of_string boolean) }
| white { token lexbuf }
| newline          { Lexing.new_line lexbuf; token lexbuf }
| eof {EOF}
