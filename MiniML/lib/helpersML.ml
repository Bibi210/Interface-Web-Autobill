open AstML



let rec json_of_loc (loc : Autobill.Misc.position) =
  Printf.sprintf "{\"beginning\": %s, \"end\": %s}" (json_of_pos loc.start_pos) (json_of_pos loc.end_pos)

and  json_of_pos p =
  Printf.sprintf "{\"line\": %d, \"column\": %d}" p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

and err msg (pos : Autobill.Misc.position) =
  failwith
  (Printf.sprintf
      "{\"loc\": %s, \"info\": \"Error on line %d col %d: %s.\"}"
      (json_of_loc pos)
      pos.start_pos.Lexing.pos_lnum
      (pos.start_pos.Lexing.pos_cnum - pos.start_pos.Lexing.pos_bol)
      msg
  )
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
let node_counter = ref 0

let generate_name () =
  incr node_counter;
  "mLTempVar" ^ Int.to_string !node_counter
;;

let reset_node_counter () = node_counter := 0

let func_curryfy args body =
  List.fold_right
    (fun a acc -> { enode = Lambda { arg = a; body = acc }; eloc = a.vloc })
    args
    body
;;
let func_rec var args body =
  match args with
  | [] -> err "Recusive Function Without Arguments" var.vloc
  | arg :: args ->
    { enode = FunctionRec { var; arg; body = func_curryfy args body }; eloc = var.vloc }
;;

let functype_curryfy args body =
  List.fold_right
    (fun a acc -> { etype = TypeLambda { arg = a; return_type = acc }; tloc = a.tloc })
    args
    body
;;

let call_curryify func args =
  List.fold_left
    (fun acc a -> { enode = Call { func = acc; arg = a }; eloc = a.eloc })
    func
    args
;;
