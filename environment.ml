open Syntax
open Value

exception NoBindingFound


let empty_env = [];;

let global_space = ref (Array.make 256 NullVal);;
let crt_loc_ptr = ref 0;;

let newref (v:Value.value) =
  !global_space.(!crt_loc_ptr) <- v;
  let ret = !crt_loc_ptr in
  crt_loc_ptr := (!crt_loc_ptr + 1);
  ret
;;
let deref loc =
  if loc < !crt_loc_ptr then
    !global_space.(loc)
  else
    raise NoBindingFound;
;;
let setref loc v =
  !global_space.(loc) <- v;
  v;
;;


let extend_env  (var:string)
                (v:Value.value)
                (env:(string*Value.value) list) =
  (var, v) :: env
;;

let extend_env_rec (id:string)
                (params:string list)
                (pbody:nameless_exp)
                (env:(string*Value.value) list) =
  let rec new_env = (id, v) :: env
  and v = ProcVal (params, pbody, new_env) in
  new_env
;;
  (*let newref_tmp = newref NullVal in
  let new_env = (id, newref_tmp) :: env in
  let v = ProcVal (params, pbody, new_env) in
  setref newref_tmp v;
  new_env;*)

let rec apply_env (var:string) (env:(string*Value.value) list) =
  match env with
  | [] -> 
      print_string "apply_env"; raise NoBindingFound;
  | e :: env_ ->
      let (var_, value_) = e in
      if var = var_ then
        value_
      else 
        apply_env var env_
;;

let rec apply_env_nameless (dep:int) (env:(string*Value.value) list) =
  match env with
  | [] ->
      print_string "apply_env_nameless"; raise NoBindingFound;
  | e :: env_ ->
      if dep = 0 then
        let (_, value_) = e in value_
      else apply_env_nameless (dep - 1) env_
;;

let rec print_env env =
  match env with
  | [] -> ();
  | (var_, value_) :: env_ ->
      print_string ("(" ^ var_ ^ ", " ^ string_of_value value_ ^ ")\n");
      print_env env_
;;


