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
  | Set (dst, src) ->
      type_prop "Set" ^ ", " ^
      prop "dst" (to_json dst) ^ ", " ^
      prop "src" (to_json src)
  | Ref (e) ->
      type_prop "Ref" ^ ", " ^
      prop "expr" (to_json e)
  | Deref (ref) ->
      type_prop "Deref" ^ ", " ^
      prop "ref" (to_json ref)
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

let rec to_json_nameless ast =
  "{" ^
  match ast with
  | PlusNL (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Plus") ^ ", " ^
      prop "left" (to_json_nameless left) ^ ", " ^
      prop "right" (to_json_nameless right)
  | MinusNL (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Minus") ^ ", " ^
      prop "left" (to_json_nameless left) ^ ", " ^
      prop "right" (to_json_nameless right)
  | TimesNL (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Times") ^ ", " ^
      prop "left" (to_json_nameless left) ^ ", " ^
      prop "right" (to_json_nameless right)
  | DivNL (left, right) ->
      type_prop "BinOp" ^ ", " ^
      prop "op" (strv "Div") ^ ", " ^
      prop "left" (to_json_nameless left) ^ ", " ^
      prop "right" (to_json_nameless right)
  | BoolLitNL (b) ->
      type_prop "BoolLit" ^ ", " ^
      prop "val" (string_of_bool b)
  | IntLitNL (n) ->
      type_prop "IntLit" ^ ", " ^
      prop "val" (string_of_int n)
  | ProcNL (params, body) ->
      type_prop "Proc" ^ ", " ^
      prop "params" (to_json_array_nameless_string params) ^ ", " ^
      prop "body" (to_json_nameless body)
  | CallNL (proc, args) ->
      type_prop "Call" ^ ", " ^
      prop "proc" (to_json_nameless proc) ^ ", " ^
      prop "args" (to_json_array_nameless args)
  | IfNL (test, conseq, alter) ->
      type_prop "If" ^ ", " ^
      prop "test" (to_json_nameless test) ^ ", " ^
      prop "conseq" (to_json_nameless conseq) ^ ", " ^
      prop "alter" (to_json_nameless alter)
  | LetNL (id, e, body) ->
      type_prop "Let" ^ ", " ^
      prop "id" ("\"" ^ id ^ "\"") ^ ", " ^
      prop "e" (to_json_nameless e) ^ ", " ^
      prop "body" (to_json_nameless body)
  | LetrecNL (id, params, pbody, letrec_body) ->
      type_prop "Letrec" ^ ", " ^
      prop "id" ("\"" ^ id ^ "\"") ^ ", " ^
      prop "params" (to_json_array_nameless_string params) ^ ", " ^
      prop "pbody" (to_json_nameless pbody) ^ ", " ^
      prop "letrec_body" (to_json_nameless letrec_body)
  | LexVarNL (str, lexdep) ->
      type_prop "VarNL" ^ ", " ^
      prop "val" ("\"" ^ str ^ "\"") ^ ", " ^
      prop "lexdep" (string_of_int lexdep)
  | SetNL (dst, src) ->
      type_prop "Set" ^ ", " ^
      prop "dst" (to_json_nameless dst) ^ ", " ^
      prop "src" (to_json_nameless src)
  | RefNL (e) ->
      type_prop "Ref" ^ ", " ^
      prop "expr" (to_json_nameless e)
  | DerefNL (ref) ->
      type_prop "Deref" ^ ", " ^
      prop "ref" (to_json_nameless ref)
  ;
  ^ "}"
and to_json_array_nameless lst =
  "[" ^
  match lst with
  | [] -> ""
  | el :: lst_ ->
      to_json_nameless el ^
      match lst_ with
      | [] -> ""
      | _ -> ", " ^ to_json_array_nameless lst_;
      ;
  ^ "]"
and to_json_array_nameless_string_ l =
  match l with
  | [] -> ""
  | el :: lst ->
      strv el ^
      match lst with
      | [] -> ""
      | _ -> ", " ^ to_json_array_nameless_string_ lst
and to_json_array_nameless_string lst =
  "[" ^ to_json_array_nameless_string_ lst ^ "]"
;;


