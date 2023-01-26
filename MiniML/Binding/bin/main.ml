open Js_of_ocaml
open Js_of_ocaml_toplevel

let isNotSet = ref false
let res = ref " "
let redirect (out : string) = 
  res := out;
  isNotSet := false

let execute code =
  isNotSet := true;
  Sys_js.set_channel_flusher stdout redirect;
  JsooTop.initialize ();
  let code = Js.to_string code in
  let buffer = Buffer.create 100 in
  let formatter = Format.formatter_of_buffer buffer in
  JsooTop.execute true formatter code;
  Js.string (Buffer.contents buffer)

let _ =
  Js.export "ml"
    (object%js
       method eval code = 
        let typeOutput = (execute code) in
        while(!isNotSet) do () done;
        object%js
          val types = typeOutput
          val resultat = Js.string(!res)
        end
     end)