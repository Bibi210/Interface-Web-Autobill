open Lexing
open Lexer
open Parser
open CstPrettyPrinter

let pos_of_error lexbuf =
  Printf.sprintf "%d:%d"
      lexbuf.lex_curr_p.pos_lnum
      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let parse lexbuf =
  try
    prog token lexbuf
  with
  | Lexer.Error msg ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^  msg))
  | Parser.Error ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^ " syntax error"))

let parse_cst name inch =
  let lexbuf = from_channel ~with_positions:true inch in
  set_filename lexbuf name;
  parse lexbuf

let string_of_cst prog =
  pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()
