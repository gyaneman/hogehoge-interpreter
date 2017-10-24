open Syntax
open Value
open Environment

let strv s = "\"" ^ s ^ "\""
let prop_name s = "\"" ^ s ^ "\""
let prop p v = prop_name p ^ ": " ^ v
let type_prop t = prop "type" ("\"" ^ t ^ "\"")

let rec to_json ast =
  "{" ^
  match ast with
  | Plus (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Plus") ^ ", " ^
      prop "left" (to_json left) ^ ", " ^
      prop "right" (to_json right)
  | Minus (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Minus") ^ ", " ^
      prop "left" (to_json left) ^ ", " ^
      prop "right" (to_json right)
  | Times (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Times") ^ ", " ^
      prop "left" (to_json left) ^ ", " ^
      prop "right" (to_json right)
  | Div (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Div") ^ ", " ^
      prop "left" (to_json left) ^ ", " ^
      prop "right" (to_json right)
  | BoolLit (b) ->
      type_prop "BoolLit" ^ ", " ^
      prop "val" (string_of_bool b)
  | IntLit (n) ->
      type_prop "IntLit" ^ ", " ^
      prop "val" (string_of_int n)
  | Proc (params, body) ->
      type_prop "Proc" ^ ", " ^
      prop "params" (to_json_array_string params) ^ ", " ^
      prop "body" (to_json body)
  | Call (proc, args) ->
      type_prop "Call" ^ ", " ^
      prop "proc" (to_json proc) ^ ", " ^
      prop "args" (to_json_array args)
  | If (test, conseq, alter) ->
      type_prop "If" ^ ", " ^
      prop "test" (to_json test) ^ ", " ^
      prop "conseq" (to_json conseq) ^ ", " ^
      prop "alter" (to_json alter)
  | Let (id, e, body) ->
      type_prop "Let" ^ ", " ^
      prop "id" ("\"" ^ id ^ "\"") ^ ", " ^
      prop "e" (to_json e) ^ ", " ^
      prop "body" (to_json body)
  | Letrec (id, params, pbody, letrec_body) ->
      type_prop "Letrec" ^ ", " ^
      prop "id" ("\"" ^ id ^ "\"") ^ ", " ^
      prop "params" (to_json_array_string params) ^ ", " ^
      prop "pbody" (to_json pbody) ^ ", " ^
      prop "letrec_body" (to_json letrec_body)
  | Var (str) ->
      type_prop "Var" ^ ", " ^
      prop "val" ("\"" ^ str ^ "\"")
  ;
  ^ "}"
and to_json_array lst =
  "[" ^
  match lst with
  | [] -> ""
  | el :: lst_ ->
      to_json el ^
      match lst_ with
      | [] -> ""
      | _ -> ", " ^ to_json_array lst_;
      ;
  ^ "]"
and to_json_array_string_ l =
  match l with
  | [] -> ""
  | el :: lst ->
      strv el ^
      match lst with
      | [] -> ""
      | _ -> ", " ^ to_json_array_string_ lst
and to_json_array_string lst =
  "[" ^ to_json_array_string_ lst ^ "]"
;;


