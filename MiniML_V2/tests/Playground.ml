let a = true;;
let x = (1,2,3) ;;
(match x with 
(*   | (hd::tail) -> true *)
  | (a,b,c) -> a
  | _ -> true
  )
;;
(1,2,4,5)
;;
42
(* ;;
type 'a list = 
| Cons of 'a *  ('a list)
| Nil *)
(* let rec f x = (x + 2)
;;
(4;5;true)

;;
fun a -> a
;;
(Cons(a,(Nil)))
;;
let rec add a b = (a + b)
;;

 *)