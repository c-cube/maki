
(* This file is free software. See file "license" for more details. *)

(** {1 Simple wrapper for Yojson} *)

type 'a or_error = ('a, exn) Result.result

(** Yojson.Safe *)
type json = Yojson.Safe.json

val make :
  to_yojson:('a -> json) ->
  of_yojson:(json -> 'a or_error) ->
  string -> 'a Maki.Value.ops

val make_str :
  to_yojson:('a -> json) ->
  of_yojson:(json -> ('a, string) Result.result) ->
  string -> 'a Maki.Value.ops
(** @since NEXT_RELEASE *)

val make_err :
  to_yojson:('a -> json) ->
  of_yojson:(json -> [`Ok of 'a | `Error of string]) ->
  string -> 'a Maki.Value.ops

