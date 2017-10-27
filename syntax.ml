exception NoBindingFound

type exp =
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp
  | IntLit of int
  | BoolLit of bool
  | Var of string
  | Let of string * exp * exp
  | If of exp * exp * exp
  | Proc of string list * exp
  | Letrec of string * string list * exp * exp
  | Call of exp * exp list
  | Set of exp * exp
  | Ref of exp
  | Deref of exp
;;

type nameless_exp =
  | PlusNL of nameless_exp * nameless_exp
  | MinusNL of nameless_exp * nameless_exp
  | TimesNL of nameless_exp * nameless_exp
  | DivNL of nameless_exp * nameless_exp
  | IntLitNL of int
  | BoolLitNL of bool
  | LexVarNL of string * int (* lexical depth *)
  | LetNL of string * nameless_exp * nameless_exp
  | IfNL of nameless_exp * nameless_exp * nameless_exp
  | ProcNL of string list * nameless_exp
  | LetrecNL of string * string list * nameless_exp * nameless_exp
  | CallNL of nameless_exp * nameless_exp list
  | SetNL of nameless_exp * nameless_exp
  | RefNL of nameless_exp
  | DerefNL of nameless_exp
;;


let extend_senv var senv = var :: senv
let rec apply_senv senv var =
  match senv with
  | [] -> raise NoBindingFound
  | x :: xs -> if x = var then 0 else 1 + apply_senv xs var
;;
  
 
(* translate exp to nameless_exp *)
let rec trans_to_nameless exp_ast senv =
  match exp_ast with
  | Plus (left, right) ->
      PlusNL (trans_to_nameless left senv, trans_to_nameless right senv)
  | Minus (left, right) ->
      MinusNL (trans_to_nameless left senv, trans_to_nameless right senv)
   | Times (left, right) ->
      TimesNL (trans_to_nameless left senv, trans_to_nameless right senv)
  | Div (left, right) ->
      DivNL (trans_to_nameless left senv, trans_to_nameless right senv)
  | IntLit (n) ->
      IntLitNL (n)
  | BoolLit (b) ->
      BoolLitNL (b)
  | Var (str) ->
      LexVarNL (str, apply_senv senv str)
  | Let (id, e, body) ->
      let new_senv = extend_senv id senv in
      LetNL (id, trans_to_nameless e senv, trans_to_nameless body new_senv)
  | If (test, conseq, alter) ->
      let new_test = trans_to_nameless test senv in
      let new_conseq = trans_to_nameless conseq senv in
      let new_alter = trans_to_nameless alter senv in
      IfNL (new_test, new_conseq, new_alter)
  | Proc (params, body) ->
      let rec extend_senv_params params senv =
        match params with
        | [] -> senv
        | x :: xs -> extend_senv x (extend_senv_params xs senv)
      in
      ProcNL (params, trans_to_nameless body (extend_senv_params params senv))
  | Letrec (id, params, pbody, retrec_body) ->
      let new_senv = extend_senv id senv in
      let new_pbody = trans_to_nameless pbody new_senv in
      LetrecNL (id, params, new_pbody, trans_to_nameless retrec_body new_senv)
  | Call (proc, args) ->
      let new_proc = trans_to_nameless proc senv in
      let rec trans_list_to_nameless li senv =
        match li with
        | [] -> []
        | x :: xs ->
          trans_to_nameless x senv :: trans_list_to_nameless xs senv;
        ;
      in
      CallNL (new_proc, trans_list_to_nameless args senv)
  | Set (dst, src) ->
      SetNL (trans_to_nameless dst senv, trans_to_nameless src senv)
  | Ref (e) ->
      RefNL (trans_to_nameless e senv)
  | Deref (ref) ->
      DerefNL (trans_to_nameless ref senv)
;;


let translate_exp_to_nameless exp_ast = 
  trans_to_nameless exp_ast []
;;


