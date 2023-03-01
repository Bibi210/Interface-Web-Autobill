
let _counter = ref 0

let fresh name =
  let ret =  name ^ "__" ^ (string_of_int !_counter) in
  incr _counter;
  ret

let refresh = fresh

let fresh_int () = let n = !_counter in incr _counter; n
