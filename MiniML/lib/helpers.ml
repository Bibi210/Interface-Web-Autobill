type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

let position start endd = { start_pos = start; end_pos = endd }

let string_of_position p =
  let fname = p.start_pos.pos_fname in
  let lnum = p.start_pos.pos_lnum in
  let st = p.start_pos.pos_cnum - p.start_pos.pos_bol in
  let endd = p.end_pos.pos_cnum - p.end_pos.pos_bol in
  Printf.sprintf "%s:%d:%d-%d" fname lnum st endd
;;
