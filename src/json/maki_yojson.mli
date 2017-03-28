
(* This file is free software. See file "license" for more details. *)

(** {1 Simple wrapper for Yojson} *)

type 'a or_error = 'a Maki.or_error

(** Yojson.Safe *)
type json = Yojson.Safe.json

val hash : json Maki.Arg.Hash.t
(** Taking json as hash *)

val codec : json Maki.Codec.t

val make :
  to_yojson:('a -> json) ->
  of_yojson:(json -> 'a or_error) ->
  string ->
  'a Maki.Codec.t

val make_exn :
  to_yojson:('a -> json) ->
  of_yojson:(json -> ('a, exn) Result.result) ->
  string ->
  'a Maki.Codec.t

val make_err :
  to_yojson:('a -> json) ->
  of_yojson:(json -> [`Ok of 'a | `Error of string]) ->
  string ->
  'a Maki.Codec.t

