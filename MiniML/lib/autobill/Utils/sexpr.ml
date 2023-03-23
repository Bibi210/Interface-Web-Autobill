open Format

(* Les AST en sortie, en syntaxe lisp.
 * Les K sont les mots clés: K "let" --> :let
 * Les V sont des variables: V "x" --> x
 * Les S sont les expression paranthèsées
 * Et B dénote une expression identée ("block") *)
type sexpr =
  | K of string
  | V of string
  | S of sexpr list
  | B of sexpr
  | L of sexpr list

let arg_list = List.map (fun s -> V s)

let block args = B (S args)

let rec pp_sexpr fmt = function
    | K kw -> pp_print_string fmt (String.capitalize_ascii kw)
  | V var -> pp_print_string fmt var
  | S expr ->
    (match expr with
     | [e] -> pp_sexpr fmt e
     | _ -> begin
         pp_open_box fmt 2;
         pp_print_char fmt '(';
         pp_print_list ~pp_sep:pp_print_space pp_sexpr fmt expr;
         pp_print_char fmt ')';
         pp_close_box fmt ()
       end)
  | B expr -> begin
      pp_open_hvbox fmt 2;
      pp_sexpr fmt expr;
      pp_close_box fmt ()
    end
  | L exprs -> begin
      pp_open_vbox fmt 0;
      List.iter (l_aux fmt) exprs;
      pp_close_box fmt ()
    end

and l_aux fmt expr =
  pp_sexpr fmt expr;
  pp_print_space fmt ()

let to_string expr =
  let fmt = Format.str_formatter in
  pp_set_margin fmt 120;
  pp_set_max_indent fmt 80;
  pp_sexpr fmt expr;
  Format.flush_str_formatter ()

let print_sexpr expr =
  let fmt = Format.std_formatter in
  pp_sexpr fmt expr;
  pp_print_newline fmt ()

let buffer () = print_endline "* New cycle"
