const billPrompts = { 'lists' : `data List (T : +) =
| nil()
| cons(T, List(T));

let map = rec self is get

| call(f,l) -> thunk(

  match l with

    | nil() -> nil()

    | cons(h, t) -> {

        open exp(ff) = f;
        let hh = ff.call(h);
        force thunk(hhh) = hh;

        open exp(mapp) = self;
        let tt = mapp.call(f,t);
        force thunk(ttt) = tt;

        return cons(hhh, ttt)
      }

  end)

end;

return map
`,
'playground' :`
type 'a list = 
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
'equation' : `% Baking cakes for the school fete

var 0..100: b; % no. of banana cakes
var 0..100: c; % no. of chocolate cakes

% flour
constraint 250*b + 200*c <= 4000;
% bananas
constraint 2*b  <= 6;
% sugar
constraint 75*b + 150*c <= 2000;
% butter
constraint 100*b + 150*c <= 500;
% cocoa
constraint 75*c <= 500;

% maximize our profit
solve maximize 400*b + 450*c;

output ["no. of banana cakes = \(b)","no. of chocolate cakes = \(c)"];
`
}

export default billPrompts