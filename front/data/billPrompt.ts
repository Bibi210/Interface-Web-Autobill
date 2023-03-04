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
(* let rec fibo n = 
  (match n with
  0 -> (Cons (0,(Nil)))
| 1 -> (Cons (1,(Nil)))
| n -> (Cons ( (fibo (n - 1)),(fibo (n - 2)))))
;;
(fibo 10)
;;
 *)
type 'a list = 
| Cons of 'a *  ('a list)
| Nil
;;
type 'a option =
| None
| Some of 'a
;;
let x = [28;2;3;4];;
let y = [14];;
let getHead ls = (
  match ls with
  | (hd::tail) -> Some (hd)
  | _ -> None
  )
;;   (* Fun Bug Here *)
let getHead2 ls = (
  match ls with
  | (hd::tail) -> Some (hd)
  | _ -> None
  )
;;
let addHeadSafe ls1 ls2 = 
let head1 = (getHead ls1) in
let head2 = (getHead2 ls2) in
(
  match head1 with
  | None -> None
  | Some(first) -> (
    match head2 with
    | None -> None
    | Some(second) -> Some ( (first + second))
    )
);;
(addHeadSafe  y x)
`}

export default billPrompts