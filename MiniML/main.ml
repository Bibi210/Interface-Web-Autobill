open Js_of_ocaml

let _ =
  Js.export "test"
    (object%js
       method add x y = x +. y
       method abs x = abs_float x
       val zero = 0.
     end)