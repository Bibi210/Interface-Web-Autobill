open Js_of_ocaml
open Js_of_ocaml_toplevel
open Autobill

let execute code =
  JsooTop.initialize ();
  let code = Js.to_string code in
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  JsooTop.execute true formatter code;
  Js.string (Buffer.contents buffer)

let _ =
  Js.export "ml"
    (object%js
      method parse code =
        let cst = Lcbpv_intf.parse code in
        let res = Lcbpv_intf.string_of_cst cst in 
        object%js
          val types = Js.string ""
          val resultat = Js.string res
          val erreurs = Js.string ""
        end
      method eval code = 
        let stdout_buff = Buffer.create 100 in
        let stderr_buff = Buffer.create 100 in
        Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buff);
        Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buff);
        let typeOutput = (execute code) in
        object%js
          val types = typeOutput
          val resultat = Js.string (Buffer.contents stdout_buff)
          val erreurs = Js.string (Buffer.contents stderr_buff)
        end
     end)