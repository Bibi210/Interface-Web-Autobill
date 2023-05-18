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
'hello_world': `type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;
let rec append l1 l2 =
  (match l1 with
    | [] -> l2
    | (x::xs) -> (x::(append xs l2)))
;;
let hello = [72;101;108;108;111] in
let world = [87;111;114;108;100] in 
let hello1 = (append hello [32]) in
let world1 = (append world [33]) in
(append hello1 world1)
`,
'merge_sort': `type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;

let rec split l =
  (match l with
    | [] -> ([],[])
    | (x1::xs) ->
      (match xs with
        | [] -> ([x1],[])
        | (x2::xs1) ->(
            (match (split xs1) with
            | (l1,l2) -> ((x1::l1), (x2::l2))
            )
         )
     )
  )   
;;

let rec merge l1 l2 =
(match l1 with
  | [] -> l2
  | (x::xs) ->
    (match l2 with
      | [] -> (x::xs)
      | (y::ys) -> (
        (match (x < y) with
          | true -> (x::(merge xs l2))
          | false -> (y::(merge l1 ys))
        )
      )
    )
 )
;;
let rec mergesort l =
  (match l with
    [] -> []
  | (x1::xs) -> 
     (match xs with
        | [] -> [x1]
        | (x2::xs1) ->
          (match (split l) with
            | (l1,l2) -> (
              let l1_prime = (mergesort l1) in
              let l2_prime = (mergesort l2) in
              (merge l1_prime l2_prime)
            )
          )
      )
  )
;;

(mergesort [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17])`,
'+ long sous seq.': `type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;

let rec firstline l =
  (match l with 
    | [] -> []
    | (x::xs) -> (0::(firstline xs))
  )
;;
let rec newline y lastline l =
  let max a b = (match (b < a) with | true -> a | false -> b) in 
  let head_or_zero l =
    (match l with
      | [] -> 0
      | (x::xs) -> x
    )
  in
  (match l with
    | []     -> []
    | (x::xs) ->
      (match lastline with
        | [] -> []
        | (belowVal::lastline1) ->
    	  let nl = (newline y lastline1 xs) in
          let rightVal = (head_or_zero nl) in
          let diagVal =  (head_or_zero lastline1) in
          let elem = (match (x = y) with | true -> (diagVal+1) | false -> (max belowVal rightVal)) in
    	  (elem::nl)
      )
  )
;;
let rec lcstable l1 l2 =
  (match l1 with
    | [] -> [(firstline l2)]
    | (x::xs) ->
      let m = (lcstable xs l2) in
      (match m with
        | [] -> []
        | (l::ls) -> (((newline x l l2)::l)::ls)
      )
)
;;

let rec lcs l1 l2 =
  let m = (lcstable l1 l2) in
  (match m with
    | [] -> 0
    | (l1::ls) ->
      (match l1 with
        | [] -> 0
        | (len::s) -> len
      )
  )
;;
let l1 = [1;2;3;4;5;6;7;8;9;10] in
let l2 = [5;3;2;6;7;1;8;0;9;11;101;102;1;2;3;10] in
  
(lcs l1 l2)`,
'sous-arbres': `let rec append l1 l2 =
(match l1 with
| [] -> l2
| (x::xs) -> (x::(append xs l2)));;

type tree =
  Leaf
| Node of int * tree * tree;;

let rec subtrees t =
(match t with
| Leaf -> []
| Node(x, t1, t2) ->
  let l1 = (subtrees t1) in
  let l2 = (subtrees t2) in
  (Node(x, t1, t2)::(append l1 l2)));;

let t = (Node (1, Node (2, Node(3,Leaf,Leaf), Leaf), Node (4,Leaf,Leaf))) in
(subtrees (Node (0,t,t)))
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
output ["Fuel(X) = \\(T__768) * 1 + \\(T__769) * X + \\(T__770) * X^2"]
`
}

export default billPrompts