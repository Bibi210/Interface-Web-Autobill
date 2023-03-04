(*For error messages*)

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
let node_counter = ref 0

let generate_name () =
  incr node_counter;
  "%.MLTempVar" ^ Int.to_string !node_counter
;;

let reset_node_counter () = node_counter := 0
