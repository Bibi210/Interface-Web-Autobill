{
  open Parser
  open Lexing
  exception Error of string
}

let num = ['0'-'9']*
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let meta = [^ '>']*
let name = ['a'-'z'] alphanum
let tcons = ['A'-'Z'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse

  | "//" {line_comment lexbuf}
  | "/*" {delim_comment lexbuf}

  | '(' {LPAREN}
  | ')' {RPAREN}
  | '<' {LANGLE}
  | '>' {RANGLE}
  | '{' {LCURLY}
  | '}'{RCURLY}
  | ':' {COLUMN}
  | '+' {PLUS}
  | '-' {MINUS}
  | '=' {EQUAL}
  | '.' {DOT}
  | ',' {COMMA}
  | "->" {ARROW}
  | '/' {SLASH}
  | '*' {STAR}
  | '&' {AMPER}
  | '|' {BAR}
  | '_' {UNDERSCORE}

  | "ret" {RET}
  | "this" {THIS}
  | "bind" {BIND}
  | "bind/cc" {BINDCC}
  | "match" {MATCH}
  | "end" {END}
  | "fun" {FUN}
  | "in" {IN}
  | "fix" {FIX}
  | "with" {WITH}

  | "GOT_ZERO" {GOT_ZERO}
  | "GOT_TOP" {GOT_TOP}

  | "box" {BOX}
  | "unbox" {UNBOX}
  | "Lin" {LLINEAR}
  | "Aff" {AAFFINE}
  | "Exp" {EEXP}

  | "Fun" {FFUN}
  | "Thunk" {TTHUNK}
  | "Closure" {CCLOSURE}
  | "Exp" {EEXP}
  | "Unit" {UUNIT}
  | "Zero" {ZZERO}
  | "Top" {TTOP}
  | "Bottom" {BBOTTOM}

  | "tuple" {TUPPLE}
  | "left" {LEFT}
  | "right" {RIGHT}
  | "inj" {INJ}
  | "call" {CALL}
  | "yes" {YES}
  | "no" {NO}
  | "proj" {PROJ}
  | "thunk" {THUNK}
  | "closure" {CLOSURE}
  | "true" {TRUE}
  | "false" {FALSE}
  | "int" {INT}

  | "unit" {UNIT}
  | "fun" {FUN}

  | "decl" {DECL}
  | "type" {TYPE}
  | "sort" {SORT}
  | "data" {DATA}
  | "comput" {COMPUT}
  | "val" {VAL}
  | "stk" {STK}
  | "cmd" {CMD}

  | num {NUM (int_of_string (Lexing.lexeme lexbuf))}
  | name {VAR (Lexing.lexeme lexbuf)}
  | tcons {TCONS (Lexing.lexeme lexbuf)}
  | "<<" meta ">>" {META}
  | eof {EOF}
  | white {token lexbuf}
  | newline {new_line lexbuf; token lexbuf}
  | _ {raise (Error (
    Printf.sprintf "Lexing failed because of unexpected %s" (Lexing.lexeme lexbuf)))}

and line_comment = parse
  | [^ '\n' ]+ { line_comment lexbuf }
  | newline {new_line lexbuf; token lexbuf}
  | eof {EOF}

and delim_comment = parse
  | "*/" {token lexbuf}
  | newline {new_line lexbuf; delim_comment lexbuf}
  | eof {raise (Error ("Lexing failed because unclosed /* */ comment"))}
  | _ { delim_comment lexbuf }
