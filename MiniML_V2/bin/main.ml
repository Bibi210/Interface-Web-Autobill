open MiniML
open Helpers

let generate_ast fname =
  (*Open the file to compile*)
  let f = open_in fname in
  (* Tokenize the file *)
  let buf = Lexing.from_channel f in
  try
    let output = Parser.prog Lexer.token buf in
    close_in f;
    output
  with
  | Lexer.Error c ->
    err (Printf.sprintf "unrecognized char '%s'" c) (Lexing.lexeme_start_p buf)
  | Parser.Error -> err "syntax error" (Lexing.lexeme_start_p buf)
;;

let () =
  if Array.length Sys.argv != 2
  then (
    Printf.eprintf "\nUsage: %s <file>\n" Sys.argv.(0);
    exit 1);
  let ast = generate_ast Sys.argv.(1) in
  print_endline ("Before Interpretation : " ^ Format.fmt_prog ast)
;;
