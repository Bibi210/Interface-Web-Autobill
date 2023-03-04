open Js_of_ocaml
open Autobill
open Intern_intf
open Sort_intf
open TypeInfer_intf
open Reduction_intf
open MiniML

let string_of_full_ast ?(debug = false) prog =
  PrettyPrinter.PP.pp_program ~debug Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let err msg pos =
  failwith
    (Printf.sprintf
       "Error on line %d col %d: %s.\n"
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
       msg)
;;

let generate_ast code =
  HelpersML.reset_node_counter ();
  try ParserML.prog LexerML.token code with
  | LexerML.Error c ->
    err (Printf.sprintf "unrecognized char '%s'" c) (Lexing.lexeme_start_p code)
  | ParserML.Error -> err "syntax error" (Lexing.lexeme_start_p code)
;;

let trad code = Lcbpv_of_ML.trans_prog (generate_ast code)

let _ =
  Js.export
    "ml"
    (object%js
       method translate code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         object%js
           val resultat = Js.string (Lcbpv_intf.string_of_cst (trad lexbuf))
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method ast code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         object%js
           val resultat = Js.string (FormatML.fmt_prog (generate_ast lexbuf))
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method parse code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         let prog, env =
           internalize (Lcbpv_intf.convert_to_machine_code (Lcbpv_intf.parse lexbuf))
         in
         let prog = polarity_inference ~trace:false env prog in
         let prelude, prog, _ = type_infer ~trace:false prog in
         let res = string_of_full_ast (prelude, prog) in
         object%js
           val resultat = Js.string res
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method interprete code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         let cst = trad lexbuf in
         let prog, env = internalize (Lcbpv_intf.convert_to_machine_code cst) in
         let prog = polarity_inference ~trace:false env prog in
         let prelude, prog, _ = type_infer ~trace:false prog in
         let prog = interpret_prog (prelude, prog) in
         let res = string_of_full_ast prog in
         object%js
           val resultat = Js.string res
           val erreur = Js.string (Buffer.contents stderr_buff)
         end
    end)
;;
