
type elt =
  | Para of string
  | Verbatim of string
  | BlankLine
type t = elt list

val of_string : string -> elt list

val pp_print_verbatim : Format.formatter -> string -> unit

val pp_print : Format.formatter -> elt list -> unit

val to_string : elt list -> string

val value : elt list OASISValues.t
