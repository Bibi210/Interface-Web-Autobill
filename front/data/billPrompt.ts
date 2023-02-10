const billPrompts = {
  'fold' : `decl sort idx
decl type Z : idx
decl type One : idx
decl type Add : (idx -> (idx -> idx))

data List (A : +) (N : idx) =
  | nil() with N = Z
  | cons<M : idx>(A, List A M) with N = (Add M One)

comput Fold (A : +) (B : -) (N : idx) =
  | this.coNil().ret(B) with N = Z
  | this.coCons<M:idx>(A).ret(Fold A B M) with N = (Add M One)

comput Fold_t =
  | this.spec_it<T : +, U : -, N : idx>().ret(Fun (List T N, Closure (Fold T U N) ) -> U)


val fold : Fold_t =
  bind/cc a -> cmd
    stk = this.fix().ret(a)
    val = match this.fix(self).ret(b) -> cmd
      stk = this.ret(b)
      val = match this.spec_it().ret(c) ->
        match stk this.call(l,f).ret(d) = this.ret(c) in
        l.match
          | nil() ->
            f.closure().coNil().ret(d)
          | cons(h,t) ->
            val f = match this.closure().ret(e) -> f.closure().coCons(h).ret(e) in
            self.unbox(Exp).fix().spec_it().call(t,f).ret(d)
        end
      end
    end
`, 'indexed_lists' : `decl sort idx
decl type Z : idx
decl type One : idx
decl type Add : (idx -> (idx -> idx))


data List (A : +) (N : idx) =
  | nil() with N = Z
  | cons<M : idx>(A, List A M) with N = (Add M One)

decl type T : +
decl type U : +

comput Forall_idx =
  | this.spec_idx<N : idx>().ret(Fun (List T N, Exp(Fun(T) -> Thunk U)) -> Thunk (List U N))

val map_idx =
match this.fix(self).ret(o) ->
  match stk this.spec_idx().ret(a) = this.ret(o) in
  match stk this.call(l,f).ret(b) = this.ret(a) in
    l.match
      | nil() -> thunk(nil()).ret(b)
      | cons(h,t) ->
        match thunk(hh) = bind/cc c -> f.unbox(Exp).call(h).ret(c) in
        match thunk(tt) = bind/cc c -> self.unbox(Exp).fix().spec_idx().call(t,f).ret(c) in
        thunk(cons(hh,tt)).ret(b)
    end
`, 'lists' : `data List (T : +) =
  | nil()
  | cons(T, (List T))

val map =
match this.fix(self).ret(a) ->
  cmd
  val = match this.call(f,l).ret(b) ->
    l.match
      | nil() -> thunk(nil()).ret(b)
      | cons(h,t) ->
        match thunk(hh) = bind/cc c -> f.unbox(Exp).call(h).ret(c) in
        match thunk(tt) = bind/cc c -> self.unbox(Exp).fix().call(f,t).ret(c) in
          thunk(cons(hh,tt)).ret(b)
    end
  stk = this.ret(a)
  end
`}

export default billPrompts