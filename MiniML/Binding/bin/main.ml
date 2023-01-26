open Js_of_ocaml
open Js_of_ocaml_toplevel.JsooTop

let eval code =
  initialize ();
  ignore (Format.flush_str_formatter ());
  execute true Format.str_formatter code;
  Format.flush_str_formatter ()
let _ =
  Js.export "ml"
    (object%js
       method eval code = 
        Js.string(eval code)
     end)