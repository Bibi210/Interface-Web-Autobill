let paren s = Printf.sprintf "(%s)" s

let string_of_tupple k = function
  | [] -> "()"
  | [e] -> paren (k e)
  | e::rest ->
      paren (List.fold_left (fun acc x -> acc ^ ", " ^ k x) (k e) rest)

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

  (* let rec test_and_remove x ys occured acc = *)
  (*   match ys with *)
  (*   | [] -> if occured then acc else raise (Failure "") *)
  (*   | y::ys -> *)
  (*     if x = y then *)
  (*       test_and_remove x ys true acc *)
  (*     else *)
  (*       test_and_remove x ys occured (y::acc) in *)

  (* try *)
  (*   ignore (List.fold_left (fun acc x -> test_and_remove x acc false []) ys xs); *)
  (*   true *)
  (* with *)
  (* | Failure _ -> false *)


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
  let fname = if with_filename then p.start_pos.pos_fname ^ ":" else "" in
  let st_lnum = p.start_pos.pos_lnum in
  let endd_lnum = p.end_pos.pos_lnum in
  let st = p.start_pos.pos_cnum - p.start_pos.pos_bol in
  let endd = p.end_pos.pos_cnum - p.end_pos.pos_bol in
  if p.is_dummy then
    "(no-position)"
  else if st_lnum = endd_lnum then
    Printf.sprintf "%s%d:%d-%d" fname st_lnum st endd
  else
    Printf.sprintf "%s%d:%d-%d:%d" fname st_lnum st endd_lnum endd
