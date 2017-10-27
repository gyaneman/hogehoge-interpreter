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


val translate_exp_to_nameless : exp -> nameless_exp
