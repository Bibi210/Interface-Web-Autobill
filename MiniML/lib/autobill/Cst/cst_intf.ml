open Lexing
open Format
open Lcbpv_export

let parse_machine_cst name inch =
  let lexbuf = from_channel ~with_positions:true inch in
  set_filename lexbuf name;
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg ->
    Misc.fatal_error "Parsing machine code" ~pos:lexbuf.lex_curr_p msg
  | Parser.Error ->
    Misc.fatal_error "Parsing machine code" ~pos:lexbuf.lex_curr_p "Syntax error"

let string_of_machine_cst prog =
  CstPrettyPrinter.pp_program str_formatter prog;
  flush_str_formatter ()


let parse_lcbpv_cst name inch =
  let lexbuf = from_channel ~with_positions:true inch in
  set_filename lexbuf name;
   try
    Lcbpv_parser.prog Lcbpv_lexer.token lexbuf
  with
   | Lcbpv_lexer.Error msg ->
     Misc.fatal_error "Parsing LCBPV code" ~pos:lexbuf.lex_curr_p msg
   | Lcbpv_parser.Error ->
     Misc.fatal_error "Parsing LCBPV code" ~pos:lexbuf.lex_curr_p "Syntax error"


let string_of_lcbpv_cst prog =
  Lcbpv_Printer.pp_program str_formatter prog;
  flush_str_formatter ()

let convert_to_machine_code =
  try
   export_prog
  with
  | Invalid_type (info, loc) ->
    Misc.fatal_error "Desugaring" ~loc info
  | Sums_with_many_args loc ->
    Misc.fatal_error "Desugaring" ~loc "This constructor/destrcutor takes only one argument."
