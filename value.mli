open Syntax

exception WrongValue
type value =
  | IntVal of int
  | BoolVal of bool
  | StringVal of string
  | ProcVal of string list * nameless_exp * (string*value) list
  | NullVal
;;

val int_val : value -> int
val bool_val : value -> bool
val string_of_value : value -> string

