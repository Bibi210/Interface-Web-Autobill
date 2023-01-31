(* Used file because unsure about this *)

open Ast
module Env = Map.Make (String)

type funcbody = Native of (Interpreter.value list -> Interpreter.value)

type env_value =
  | Function of
      { f_type : Ast.types
      ; f_body : funcbody
      }
  | Variable of Ast.types

let add =
  ( "_%add"
  , Function
      { f_type = Ast.Lambda ([ Ast.Int_t; Ast.Int_t ], Ast.Int_t)
      ; f_body =
          Native
            (function
            | [ Interpreter.Const (Integer a); Interpreter.Const (Integer b) ] ->
              Interpreter.Const (Integer (a + b))
            | _ -> failwith "Add Failure")
      } )
;;

let std = List.fold_left (fun acc (name, data) -> Env.add name data acc) Env.empty [ add ]
