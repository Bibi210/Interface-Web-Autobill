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

let generate_name =
  let node_counter = ref 0 in
  fun () ->
    incr node_counter;
    "%.MLTempVar" ^ Int.to_string !node_counter
;;
