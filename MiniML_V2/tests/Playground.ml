let a = true ;;
let rec f x = (x + 2)
;;
type 'a list = 
| Cons of ('a * ('a list))
| Nil
;;
(4;5;true)
;;
(1,2,4,5)
;;
fun a:int -> a
;;
Cons(a,Nil)
;;
let rec add a b = (a + b)
;;
let add a b = (a + b) in (add a b)
