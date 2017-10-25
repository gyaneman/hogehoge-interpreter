val newref : Value.value -> int
val deref : int -> Value.value
val setref : int -> Value.value -> Value.value

val extend_env : string -> int -> (string*int) list -> (string*int) list
val extend_env_rec : string -> string list -> Syntax.nameless_exp -> (string*int) list -> (string*int) list
val apply_env : string -> (string*int) list -> int
val apply_env_nameless : int -> (string*int) list -> int

val print_env : (string*int) list -> unit
