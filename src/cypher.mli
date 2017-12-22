


val coupe_ligne : string -> string array

val coder_sauver : ?code:int -> ?decalage:int -> ?graine:int * int * int ->
  string -> string -> unit

val decoder_charger : ?code:int -> ?decalage:int -> string -> string

val lit_string : string array * int ref -> string
val lit_float  : string array * int ref -> float
val lit_int    : string array * int ref -> int
val lit_bool   : string array * int ref -> bool

