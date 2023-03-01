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
'playground' :`let a = true;;
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

`}

export default billPrompts