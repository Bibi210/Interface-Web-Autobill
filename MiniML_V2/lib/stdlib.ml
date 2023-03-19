
type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;
type 'a option =
| None
| Some of 'a
;; 
type 'a tree = 
| Node of 'a * ('a tree) * ('a tree)
| Empty
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

