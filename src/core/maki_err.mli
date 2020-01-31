
(* This file is free software. See file "license" for more details. *)

(** Error-related Utils *)

type 'a or_error = ('a, string) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t

val error : string -> _ or_error
val errorf : ('a, Format.formatter, unit, 'b or_error) format4 -> 'a
