
val adresse : string

val get_path : string list -> string

val get_include : string -> string

val get_level : int -> int -> string


val aff_list : int list -> unit

val debug : ('a, Format.formatter, unit) format -> 'a
