open Syntax
open Value
open Environment

exception WrongValue


let rec extend_env_params params args env =
  match params with
  | [] -> env
  | p :: params_ ->
      match args with
      | [] ->
          let env_ = extend_env p NullVal env in
          extend_env_params params_ [] env_
      | a :: args_ ->
          let env_ = extend_env p a env in
          extend_env_params params_ args_ env_
;;


let rec eval_exp exp_ast env =
  match exp_ast with
  | Plus (left, right) ->
      IntVal ((int_val (eval_exp left env))
      + (int_val (eval_exp right env)))
  | Minus (left, right) ->
      IntVal ((int_val (eval_exp left env))
      - (int_val (eval_exp right env)))
  | Times (left, right) ->
      IntVal ((int_val (eval_exp left env))
      * (int_val (eval_exp right env)))
  | Div (left, right) ->
      IntVal ((int_val (eval_exp left env))
      / (int_val (eval_exp right env)))
  | IntLit (n) ->
      IntVal (n)
  | BoolLit (b) ->
      BoolVal (b)
  | Let (id, e, body) ->
      eval_exp body (extend_env id (eval_exp e env) env)
  | Proc (params, body) ->
      ProcVal (params, body, env)
  | Letrec (id, params, pbody, retrec_body) ->
      let rec new_env = (id, v) :: env
      and v = ProcVal (params, pbody, new_env) in
      eval_exp retrec_body new_env
  | Call (proc, args) ->
      let proc_ = eval_exp proc env in
      match proc_ with
      | ProcVal (params, body, env) ->
          let args_ = eval_exp_list args env in
          eval_exp body (extend_env_params params args_ env)
      | _ -> raise WrongValue;
      ;
  | If (test, conseq, alter) ->
      if bool_val (eval_exp test env) then
        eval_exp conseq env
      else
        eval_exp alter env
  | Var (str) ->
      apply_env str env
and eval_exp_list lst env =
  match lst with
  | [] -> []
  | el :: lst_ -> eval_exp el env :: eval_exp_list lst_ env
;;



