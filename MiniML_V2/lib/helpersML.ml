type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  ; is_dummy : bool
  }

let position start endd = { start_pos = start; end_pos = endd; is_dummy = false }

let dummy_pos =
  { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos; is_dummy = true }
;;

List.fold_right

let string_of_position p =
  let fname = p.start_pos.pos_fname in
  let lnum = p.start_pos.pos_lnum in
  let st = p.start_pos.pos_cnum - p.start_pos.pos_bol in
  let endd = p.end_pos.pos_cnum - p.end_pos.pos_bol in
  Printf.sprintf "%s:%d:%d-%d" fname lnum st endd
;;

(*For error messages*)
let err msg pos =
  Printf.eprintf
    "Error on line %d col %d: %s.\n"
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
    msg;
  exit 1
;;

let rec list_getlast_rem = function
  | [] -> raise Not_found
  | [ hd ] -> hd, []
  | hd :: tl ->
    let hd', tl' = list_getlast_rem tl in
    hd', hd :: tl'
;;

let list_getlast ls =
  let last, _ = list_getlast_rem ls in
  last
;;

let array_getlast array = Array.get array (Array.length array - 1)

let generate_name =
  let node_counter = ref 0 in
  fun () ->
    incr node_counter;
    "%.MLTempVar" ^ Int.to_string !node_counter
;;