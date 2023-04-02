let rec insert_nodup l x = match l with
  | [] -> [x]
  | h::t ->
    if x = h then l else
    if x < h then (x::h::t)
    else h::(insert_nodup t x)

let is_sublist xs ys =
  let rec go xs ys = match xs, ys with
    | [],_ -> true
    | _::_, [] -> false
    | x::xs, y::ys -> if x = y then go xs (y::ys) else go (x::xs) ys
  in go (List.sort compare xs) (List.sort compare ys)

type position = {
  start_pos : Lexing.position;
  end_pos : Lexing.position;
  is_dummy : bool
}

let position start endd = {
  start_pos = start;
  end_pos = endd;
  is_dummy = false
}

let dummy_pos = {
  start_pos = Lexing.dummy_pos;
  end_pos = Lexing.dummy_pos;
  is_dummy = true
}

let string_of_position ?(with_filename = true) p =
  let fname = if with_filename then " in " ^ p.start_pos.pos_fname else "" in
  let st_lnum = p.start_pos.pos_lnum in
  let endd_lnum = p.end_pos.pos_lnum in
  let st = p.start_pos.pos_cnum - p.start_pos.pos_bol in
  let endd = p.end_pos.pos_cnum - p.end_pos.pos_bol in
  if p.is_dummy then
    "(no-position)"
  else if st_lnum = endd_lnum then
    Printf.sprintf "%d:%d-%d%s" st_lnum st endd fname
  else
    Printf.sprintf "%d:%d-%d:%d%s" st_lnum st endd_lnum endd fname

exception Invariant_break of string * position option

let fail_invariant_break ?loc message = raise (Invariant_break (message, loc))

exception Fatal_error of {
    phase : string;
    info : string;
    loc : position option;
    pos : Lexing.position option
  }

let fatal_error phase ?loc ?pos info = raise (Fatal_error {phase; loc; pos; info})
