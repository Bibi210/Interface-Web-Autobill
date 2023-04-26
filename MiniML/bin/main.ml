open Js_of_ocaml
open Autobill
open Cst_intf
open Intern_intf
open Sort_intf
open Reduction_intf
open TypeInfer_intf
open MiniML
open Autobill.Misc

let rec json_error_reporter e = match e with

  | Fatal_error {phase; info; loc; pos} ->
    let loc = match loc, pos with
      | Some loc, _ -> HelpersML.json_of_loc loc
      | None, Some pos -> HelpersML.json_of_pos pos
      | None, None -> "false" in
    failwith (Printf.sprintf "{\"phase\": \"%s\", \"loc\": %s, \"info\": \"%s\"}" phase loc info)

  | Invariant_break (info, loc) ->
    json_error_reporter (Fatal_error {info; loc; phase = "false"; pos = None})

  | e -> raise e

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
             Js.string (string_of_lcbpv_cst (translate_ML_to_LCBPV lexbuf))
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
      
       method machine code =
        let stderr_buff = Buffer.create 100 in
        Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
        let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
        let cbpv = translate_ML_to_LCBPV lexbuf in
        try
          let intern = internalize (convert_to_machine_code cbpv) in 
          let res = polarity_inference intern in
          object%js
            val resultat = Js.string (string_of_full_ast res)
            val erreur = Js.string (Buffer.contents stderr_buff)
          end
        with
          e -> (
            ignore (json_error_reporter e);
            object%js
              val resultat = Js.string ""
              val erreur = Js.string (Buffer.contents stderr_buff)
            end
          )

       method parse code =
         let stderr_buff = Buffer.create 100 in
         Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
         let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
         let cst = translate_ML_to_LCBPV lexbuf in
         try
            let prog  = internalize (convert_to_machine_code cst) in
            let prog = polarity_inference prog in
            let res = constraint_as_string prog in
            object%js
              val resultat = Js.string res
              val erreur = Js.string (Buffer.contents stderr_buff)
            end
          with
            e -> (
              ignore (json_error_reporter e);
              object%js
                val resultat = Js.string ""
                val erreur = Js.string (Buffer.contents stderr_buff)
              end
            )

       method mltoequation code =
          let stderr_buff = Buffer.create 100 in
          Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
          let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
          let cst = translate_ML_to_LCBPV lexbuf in
          try
            let prog = internalize (convert_to_machine_code cst) in
            let prog = polarity_inference prog in
            let prog, post_con = type_infer ~trace:false prog in
            let post_con = AaraCompress.compress_unification post_con in
            let res =  match prog.goal with
            | Some goal -> AaraExport.convert_to_minizinc_file goal post_con
            | None -> Misc.fatal_error "Generating complexity model" "The program defines no goal to infer" 
            in
            object%js
              val resultat = Js.string res
              val erreur = Js.string (Buffer.contents stderr_buff)
            end
          with
            e -> (
              ignore (json_error_reporter e);
              object%js
                val resultat = Js.string ""
                val erreur = Js.string (Buffer.contents stderr_buff)
              end
            )

       method mlinterpretation code =
          let stderr_buff = Buffer.create 100 in
          Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
          let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in
          let cst = translate_ML_to_LCBPV lexbuf in
          try
            let prog = internalize (convert_to_machine_code cst) in
            let prog = polarity_inference prog in
            let prog, _  = type_infer ~trace:false prog in
            let prog = interpret_prog prog in
            let res = string_of_full_ast prog in
            object%js
              val resultat = Js.string res
              val erreur = Js.string (Buffer.contents stderr_buff)
            end
          with
            e -> (
              ignore (json_error_reporter e);
              object%js
                val resultat = Js.string ""
                val erreur = Js.string (Buffer.contents stderr_buff)
              end
            )
    end)
;;
