
(* This file is free software. See file "license" for more details. *)

(** {1 Logs} *)

type logger = {
  log: 'a.
    int ->
    ((('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a) -> unit) ->
    unit
}

val log : int -> string -> unit

val logf :
  int ->
  ((('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a) -> unit) ->
  unit
(** Use like this:
    [logf 1 (fun k->k "hello %s, 42=%d" "world" (41+1))] *)

val default_logger : logger

val set_logger : logger -> unit

val set_level : int -> unit
