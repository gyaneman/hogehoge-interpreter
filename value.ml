open Syntax

exception WrongValue

type value =
  | IntVal of int
  | BoolVal of bool
  | StringVal of string
  | ProcVal of string list * nameless_exp * (string*value) list
  | RefVal of int
  | NullVal
;;


let int_val v =
  match v with
  | IntVal (n) -> n
  | _ -> raise WrongValue
;;
let bool_val v =
  match v with
  | BoolVal (n) -> n
  | _ -> raise WrongValue
;;

let string_of_value v =
  match v with
  | IntVal (n) -> string_of_int n
  | BoolVal (b) -> string_of_bool b
  | StringVal (s) -> s
  | ProcVal (args, body, env) -> "ProcVal"
  | RefVal (loc) -> string_of_int loc
  | NullVal -> "NULL"
;;

