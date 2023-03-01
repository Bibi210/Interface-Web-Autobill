open Js_of_ocaml
open Autobill
open Intern_intf
open Sort_intf
open TypeInfer_intf
open MiniML

let string_of_full_ast ?debug:(debug=false) prog =
  PrettyPrinter.PP.pp_program ~debug Format.str_formatter prog;
  Format.flush_str_formatter ()

let generate_ast code =
  ParserML.prog LexerML.token code

let _ =
  Js.export "ml"
    (object%js
      method ast code = 
        let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in 
        object%js
          val resultat = Js.string (FormatML.fmt_prog (generate_ast lexbuf))
        end 
      method parse code =
        let stdout_buff = Buffer.create 100 in
        Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buff);
        let lexbuf = Lexing.from_string ~with_positions:true (Js.to_string code) in 
        let prog, env = internalize (Lcbpv_intf.convert_to_machine_code (Lcbpv_intf.parse lexbuf)) in
        let prog = polarity_inference ~trace:false env prog in 
        let prelude, prog, _ = type_infer ~trace:false prog in
        let res = string_of_full_ast (prelude, prog) in 
        object%js
          val resultat = Js.string res
        end
     end)
