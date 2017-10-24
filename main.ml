open Syntax
open Value
open Environment
open Interp
open Ast_printer


let init_env = [("zero", IntVal (0))];;

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      translate_exp_to_nameless result;
      print_string (to_json result);
      print_newline();
      print_string (string_of_value (eval_exp result init_env));
      print_newline();
      flush stdout
    done
  with Lexer.Eof ->
    exit 0
;;


