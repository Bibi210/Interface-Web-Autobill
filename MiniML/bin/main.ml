open MiniML
open Lexing

(*For error messages*)
let err msg pos =
  Printf.eprintf
    "Error on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg;
  exit 1
;;

let () =
  if Array.length Sys.argv != 2
  then (
    Printf.eprintf "\nUsage: %s <file>\n" Sys.argv.(0);
    exit 1);
  (*Open the file to compile*)
  let f = open_in Sys.argv.(1) in
  (* Tokenize the file *)
  let buf = Lexing.from_channel f in
  try
    (*Generate AST with type informations*)
    let parsed = Parser.prog Lexer.token buf in
    close_in f;
    print_endline ("\n" ^ Ast.fmt_program parsed)
  with
  | Lexer.Error c ->
    err (Printf.sprintf "unrecognized char '%s'" c) (Lexing.lexeme_start_p buf)
  | Parser.Error -> err "syntax error" (Lexing.lexeme_start_p buf)
;;
(*Unrecognized Stuff*)
