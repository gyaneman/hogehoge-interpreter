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
          let env_ = extend_env p (newref NullVal) env in
          extend_env_params params_ [] env_
      | a :: args_ ->
          let env_ = extend_env p (newref a) env in
          extend_env_params params_ args_ env_
;;


let rec eval_exp exp_ast env =
  match exp_ast with
  | PlusNL (left, right) ->
      IntVal ((int_val (eval_exp left env))
      + (int_val (eval_exp right env)))
  | MinusNL (left, right) ->
      IntVal ((int_val (eval_exp left env))
      - (int_val (eval_exp right env)))
  | TimesNL (left, right) ->
      IntVal ((int_val (eval_exp left env))
      * (int_val (eval_exp right env)))
  | DivNL (left, right) ->
      IntVal ((int_val (eval_exp left env))
      / (int_val (eval_exp right env)))
  | IntLitNL (n) ->
      IntVal (n)
  | BoolLitNL (b) ->
      BoolVal (b)
  | LetNL (id, e, body) ->
      let e_ = newref (eval_exp e env) in
      eval_exp body (extend_env id e_ env)
  | ProcNL (params, body) ->
      ProcVal (params, body, env)
  | LetrecNL (id, params, pbody, retrec_body) ->
      (*let rec new_env = (id, newref v) :: env
      and v = ProcVal (params, pbody, new_env) in*)
      eval_exp retrec_body (extend_env_rec id params pbody env)
  | CallNL (proc, args) ->
      let proc_ = eval_exp proc env in
      match proc_ with
      | ProcVal (params, body, env) ->
          let args_ = eval_exp_list args env in
          eval_exp body (extend_env_params params args_ env)
      | _ -> raise WrongValue;
      ;
  | IfNL (test, conseq, alter) ->
      if bool_val (eval_exp test env) then
        eval_exp conseq env
      else
        eval_exp alter env
  | LexVarNL (_, lexdep) ->
      deref (apply_env_nameless lexdep env)
  | SetNL (dst, src) ->
      let src_val = eval_exp src env in
      match dst with
      | LexVarNL (_, lexdep) ->
          setref lexdep src_val
      | _ -> raise WrongValue;
and eval_exp_list lst env =
  match lst with
  | [] -> []
  | el :: lst_ -> eval_exp el env :: eval_exp_list lst_ env
;;

