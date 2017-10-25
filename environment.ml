open Syntax
open Value

exception NoBindingFound


let empty_env = [];;

let global_space = ref (Array.make 256 NullVal);;
let crt_loc_ptr = ref 0;;

let newref (v:Value.value) =
  !global_space.(!crt_loc_ptr) <- v;
  crt_loc_ptr := (!crt_loc_ptr + 1);
  !crt_loc_ptr
;;
let deref loc =
  if !crt_loc_ptr < loc then
    !global_space.(loc)
  else
    raise NoBindingFound
;;
let setref loc v =
  !global_space.(loc) <- v;
  v;
;;


let extend_env  (var:string)
                (reference:int)
                (env:(string*int) list) =
  (var, reference) :: env
;;

let extend_env_rec (id:string)
                (params:string list)
                (pbody:nameless_exp)
                (env:(string*int) list) =
  let newref_tmp = newref NullVal in
  let new_env = (id, newref_tmp) :: env in
  let v = ProcVal (params, pbody, new_env) in
  setref newref_tmp v;
  new_env;
;;

let rec apply_env (var:string) (env:(string*int) list) =
  match env with
  | [] -> raise NoBindingFound
  | e :: env_ ->
      let (var_, reference) = e in
      if var = var_ then
        reference
      else 
        apply_env var env_
;;

let rec apply_env_nameless (dep:int) (env:(string*int) list) =
  match env with
  | [] -> raise NoBindingFound
  | e :: env_ ->
      if dep = 0 then
        let (_, reference) = e in reference
      else apply_env_nameless (dep - 1) env_
;;

let rec print_env env =
  match env with
  | [] -> ();
  | (var_, ref_) :: env_ ->
      print_string ("(" ^ var_ ^ ", " ^ string_of_value (deref ref_) ^ ")\n");
      print_env env_
;;


