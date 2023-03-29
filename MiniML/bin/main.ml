open Js_of_ocaml
open Autobill
open Cst_intf
open Intern_intf
open Sort_intf
open Reduction_intf
open TypeInfer_intf
open MiniML
open Autobill.Misc

let string_of_full_ast ?(debug = false) prog =
  PrettyPrinter.PP.pp_program ~debug Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let fmtMiniML prog =
  FormatML.fmp_prog Format.str_formatter prog;
  Format.flush_str_formatter ()
;;

let generate_ast code =
  HelpersML.reset_node_counter ();
  Global_counter._counter := 0;
  try ParserML.prog LexerML.token code with
  | LexerML.Error c ->
    HelpersML.err
      (Printf.sprintf "Unrecognized char '%s'" c)
      (position (Lexing.lexeme_start_p code) (Lexing.lexeme_end_p code))
  | ParserML.Error ->
    HelpersML.err
      "Syntax error"
      (position (Lexing.lexeme_start_p code) (Lexing.lexeme_end_p code))
;;

let translate_ML_to_LCBPV code = Lcbpv_of_ML.trans_prog (generate_ast code)

let _ =
  Js.export
    "ml"
    (object%js
       method translate code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         object%js
           val resultat =
             Js.string (Lcbpv_intf.string_of_cst (translate_ML_to_LCBPV lexbuf))

           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method ast code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         object%js
           val resultat = Js.string (fmtMiniML (generate_ast lexbuf))
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method parse code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         let prog, env =
           internalize (Lcbpv_intf.convert_to_machine_code (Lcbpv_intf.parse lexbuf))
         in
         let prog = polarity_inference env prog in
         let res = constraint_as_string prog in
         object%js
           val resultat = Js.string res
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method mltoequation code =
          let stdout_buff = Buffer.create 100 in
          Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buff);
          let stderr_buff = Buffer.create 100 in
          Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
          let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
          let cst = translate_ML_to_LCBPV lexbuf in
          let prog, env = internalize (Lcbpv_intf.convert_to_machine_code cst) in
          let prog = polarity_inference env prog in
          let prog, post_con = type_infer ~trace:false prog in
          let post_con = AaraCompress.compress_unification post_con in
          let no_goal = (Types.cons (Types.Cons Primitives.nat_zero)) in
          let res = AaraExport.convert_to_optimization
                (fun _ -> failwith "unimplemented")
                post_con no_goal in
          AaraExport.pp_solution Format.std_formatter res;
         object%js
           val resultat = Js.string (Buffer.contents stdout_buff)
           val erreur = Js.string (Buffer.contents stderr_buff)
         end

       method mlinterpretation code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         let cst = translate_ML_to_LCBPV lexbuf in
         let prog, env = internalize (Lcbpv_intf.convert_to_machine_code cst) in
         let prog = polarity_inference env prog in
         let prog, post_con  = type_infer ~trace:false prog in
         let prog = interpret_prog prog in
         let res = string_of_full_ast prog in
         object%js
           val resultat = Js.string res
           val erreur = Js.string (Buffer.contents stderr_buff)
         end
    end)
;;