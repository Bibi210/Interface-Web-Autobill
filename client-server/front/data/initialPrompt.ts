const initial = `(* 
  Welcome to OCaml Play, the official OCaml playground!

  You don't need to install anything - just write your code
  and see the results appear in the Output panel.

  This playground is powered by OCaml 5 which comes with
  support for shared-memory parallelism through domains and effects.
  Below is some naive example code that calculates
  the Fibonacci sequence in parallel.
  
  Happy hacking!
*)

let num_domains = 2
let n = 20

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par n d =
  if d <= 1 then fib n
  else
    let a = fib_par (n-1) (d-1) in
    let b = Domain.spawn (fun _ -> fib_par (n-2) (d-1)) in
    a + Domain.join b

let () =
  let res = fib_par n num_domains in
  Printf.printf "fib(%d) = %d\\n" n res`

export default initial