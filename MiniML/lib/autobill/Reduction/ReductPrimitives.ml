open Vars
open Ast
open Constructors
open FullAst

let mk_int n = V.cons (cons (Int n) [] [])

let mk_bool b = V.cons (cons (Bool b) [] [])

let process_int_binop v a b =
  match Var.to_string v with
  | "op_add" -> mk_int (a+b)
  | "op_sub" -> mk_int (a-b)
  | "op_mul" -> mk_int (a*b)
  | "op_div" -> mk_int (a/b)
  | "op_mod" -> mk_int (a mod b)
  | "op_eq" -> mk_bool (a=b)
  | "op_lt" -> mk_bool (a<b)
  | "op_leq" -> mk_bool (a<=b)
  | _ -> assert false

let process_int_monop v a =
  match Var.to_string v with
  | "op_op" -> mk_int (-a)
  | _ -> assert false

let process_bool_binop v a b =
  match Var.to_string v with
  | "op_and" -> mk_bool (a&&b)
  | "op_or" -> mk_bool (a||b)
  | _ -> assert false

let process_bool_monop v a =
  match Var.to_string v with
  | "op_neg" -> mk_bool (not a)
  | _ -> assert false


let process_prim v args = match Var.to_string v with
  | "op_add" | "op_sub" | "op_mul" | "op_div" | "op_mod" | "op_lt" | "op_leq" | "op_eq" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Int a; _}); _};
         MetaVal {node = Cons (Raw_Cons {tag = Int b; _}); _}] ->
        process_int_binop v a b
      | _ -> assert false
    end
  | "op_and" | "op_or" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Bool a; _}); _};
         MetaVal {node = Cons (Raw_Cons {tag = Bool b; _}); _}] ->
        process_bool_binop v a b
      | _ -> assert false
    end
  | "op_op" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Int a; _}); _}] ->
        process_int_monop v a
      | _ -> assert false
    end
  | "op_not" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Bool a; _}); _}] ->
        process_bool_monop v a
      | _ -> assert false
    end
  | _ -> assert false

let go (Command cmd) =
  let MetaStack {node = s; _} = cmd.stk in
  let MetaVal {node = v; _} = cmd.valu in
  match v,s with
  | Var v, CoDestr (Raw_Destr {tag = Call _; args; idxs; cont}) ->
    assert (idxs = []);
    let v = process_prim v args in
    Command {cmd with
             valu = v;
             stk = cont
            }
  | _ -> (Command cmd)
