open Syntax
open Value

exception NoBindingFound

let empty_env = [];;

let extend_env  (var:string)
                (value:Value.value)
                (env:(string*Value.value) list) =
  (var, value) :: env
;;

let rec apply_env (var:string) (env:(string*Value.value) list) =
  match env with
  | [] -> raise NoBindingFound
  | e :: env_ ->
      let (var_, value_) = e in
      if var = var_ then
        value_
      else 
        apply_env var env_
;;

let rec print_env env =
  match env with
  | [] -> ();
  | (var_, value_) :: env_ ->
      print_string ("(" ^ var_ ^ ", " ^ string_of_value value_ ^ ")\n");
      print_env env_
;;
