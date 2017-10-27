val newref : Value.value -> int
val deref : int -> Value.value
val setref : int -> Value.value -> Value.value

val extend_env : string -> Value.value -> (string*Value.value) list -> (string*Value.value) list
val extend_env_rec : string -> string list -> Syntax.nameless_exp -> (string*Value.value) list -> (string*Value.value) list
val apply_env : string -> (string*Value.value) list -> Value.value
val apply_env_nameless : int -> (string*Value.value) list -> Value.value

val print_env : (string*Value.value) list -> unit
