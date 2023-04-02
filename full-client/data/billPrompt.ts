const billPrompts = {
'playground' :`type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;
type 'a option =
| None
| Some of 'a
;; 
let fastCons a b= (a::b) ;;
let create = ([],[]);;
let push  file elem = 
  (match file with
  | (a,b) -> ( a , (elem::b) ))
;;
let rec fold_left f accu l =
  (match l with
    [] -> accu
  | (a::l) -> (fold_left f (f accu a) l))
;;
let rec fold_right f l accu =
  (match l with
    [] -> accu
  | (a::l) -> (f a (fold_right f l accu)))
;;

let rev l = 
  let rec rev_append l1 l2 =
  (match l1 with
    [] -> l2
  | (a :: l) -> (rev_append l (a :: l2)))
  in
(rev_append l [])
;;
let pop file =
  (match file with
  | (debut, fin) -> 
    (match debut with
    | [] -> (match (rev fin) with
            | [] -> (None,debut,fin)
            | (hd :: tail) -> ((Some(hd)), tail ,[] ) )
    | (hd::tail) -> ((Some(hd)),tail,fin ))
  )
;;
 let elems = [1;2;3;4;5;6;7];;
let queue = (fold_left push create elems);;
(pop queue)
;; 
type 'a tree = 
| Node of 'a * ('a tree) * ('a tree)
| Empty
(* ;;
let rec addToTree  tree  elem = (
  match tree with
  | Empty -> (Node(elem, Empty ,Empty))
  | (Node(e,g,d)) -> (match (elem > e) with 
    |true -> (Node(e, (addToTree  g elem),d))
    |false -> (Node(e,g,(addToTree  d elem)))))

let treeToList tree  =
  let rec aux acc node = 
    (match node with
    | Empty -> acc
    | (Node(e,g,d)) -> (aux (e:: (aux acc g)) d))
  in (aux [] tree)
;;
let l = (fold_left addToTree Empty [3;2;1;5;7]);;
let l = treeToList l;;
*)
`, 
'equation' : `var int: T__777; constraint T__777 >= 0;
var int: T__776; constraint T__776 >= 0;
var int: T__775; constraint T__775 >= 0;
var int: T__774; constraint T__774 >= 0;
var int: T__773; constraint T__773 >= 0;
var int: T__772; constraint T__772 >= 0;
var int: T__771; constraint T__771 >= 0;
var int: T__770; constraint T__770 >= 0;
var int: T__769; constraint T__769 >= 0;
var int: T__768; constraint T__768 >= 0;
constraint (T__768 + (-1 * T__771)) = 0;
constraint (((T__768 + T__769) + T__770) + (-1 * (T__768 + 1))) = 0;
constraint (-1 * (T__775 + T__772)) = 0;
constraint ((T__769 + (T__770 * 2)) + (-1 * ((T__769 + T__776) + T__773))) = 0;
constraint (T__770 + (-1 * T__770)) = 0;
constraint (-1 * (T__777 + T__774)) = 0;
solve minimize ((T__768 + (T__769 * 1000)) + (T__770 * 1000000));
output ["Fuel(X) = \(T__768) * 1 + \(T__769) * X + \(T__770) * X^2"]
`
}

export default billPrompts