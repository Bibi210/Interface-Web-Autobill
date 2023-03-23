{
  open Lcbpv_parser
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
  | '{' {LCURLY}
  | '}' {RCURLY}
  | ':' {COLUMN}
  | '+' {PLUS}
  | '-' {MINUS}
  | '=' {EQUAL}
  | ',' {COMMA}
  | "->" {ARROW}
  | '|' {BAR}
  | '.' {DOT}
  | ';' {SEMICOL}
  | '*' {STAR}
  | '/' {SLASH}
  | '%' {PERCENT}
  | '!' {BANG}
  | '<' {LANGLE}
  | "&&" {AND}
  | "||" {OR}

  | "Tuple" {TTUPLE}
  | "Sum" {SSUM}
  | "Fun" {FFUN}
  | "Choice" {CCHOICE}
  | "Thunk" {TTHUNK}
  | "Closure" {CCLOSURE}
  | "Aff" {AAFF}
  | "Exp" {EEXP}
  | "Unit" {UUNIT}
  | "Zero" {ZZERO}
  | "Top" {TTOP}
  | "Bottom" {BBOTTOM}

  | "unit" {UNIT}
  | "thunk" {THUNK}
  | "closure" {CLOSURE}
  | "exp" {EXP}
  | "aff" {AFF}
  | "true" {TRUE}
  | "false" {FALSE}
  | "inj" {INJ}
  | "proj" {PROJ}
  | "call" {CALL}

  | "get" {GET}
  | "end" {END}
  | "match" {MATCH}
  | "return" {RETURN}
  | "let" {LET}
  | "rec" {REC}
  | "is" {IS}
  | "force" {FORCE}
  | "open" {OPEN}
  | "with" {WITH}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | "decl" {DECL}
  | "type" {TYPE}
  | "data" {DATA}
  | "comput" {COMPUT}

  | num {NUM (int_of_string (Lexing.lexeme lexbuf))}
  | name {VAR (Lexing.lexeme lexbuf)}
  | tcons {TCONS (Lexing.lexeme lexbuf)}
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
