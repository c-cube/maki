
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations}

    Maki is a system for memoizing costly OCaml functions using the disk.
    It requires the functions to be {b pure}, that is, to always return
    the same result given that the set of {b dependencies} declared by
    the function doesn't change.

    {b status: experimental}

    This module is not thread-safe.
*)

type 'a or_error = ('a, string) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t
type ('a,'rw) pipe = ('a,'rw) Maki_pipe.t

(** {2 Basic types} *)

type path = string
type program = string
type time = float

(** {2 Controlling Parallelism} *)

module Limit : sig
  type t
  val create : int -> t
  val acquire : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val j : unit -> t
  (** Default limiter for concurrency, shoudl be set by CLI options *)

  val set_j : int -> unit
  (** Should be called at the beginning to set the value of [j].
      @raise Failure if [j] is already evaluated *)
end

(** {2 Values}

    Maki deals with functions that take and return values of any
    type that provides a {!'a Value.t} implementation;
    that is, values that we can serialize, unserialize, and hash.
    The reason is that values are stored on disk for memoization purposes. *)
module Value : sig
  type 'a ops = {
    descr: string; (* description of 'a *)
    serialize : 'a -> Bencode.t;
    unserialize : Bencode.t -> 'a or_error;
    timestamp: ('a -> time) option;
      (* last time the value was modified *)
    canonical_form : ('a -> string Lwt.t) option;
      (* unique representation of ['a]. If absent, the bencode
         encoding of [serialize] is used *)
  }
  (* ['a ops] describes how to use values of type ['a] as memoized values.
     A value must be serializable, unserializable, and
     it should have a conversion to a {b unique} string; two values that
     have the same canonical form (the string) are considered equal
     w.r.t. caching and recomputations. *)

  val make :
    ?timestamp:('a -> time) ->
    ?canonical_form:('a -> string Lwt.t) ->
    serialize:('a -> Bencode.t) ->
    unserialize:(Bencode.t -> 'a or_error) ->
    string -> 'a ops

  val serialize : 'a ops -> 'a -> Bencode.t
  val unserialize : 'a ops -> Bencode.t -> 'a or_error
  val to_string : 'a ops -> 'a -> string
  val of_string : 'a ops -> string -> 'a or_error
  val canonical_form : 'a ops -> 'a -> string Lwt.t
  val last_timestamp : 'a ops -> 'a -> time option

  val int : int ops
  val string : string ops
  val bool : bool ops
  val list : 'a ops -> 'a list ops
  val assoc : 'a ops -> (string * 'a) list ops

  val file : path ops
  (** A {b reference} to some file content. This should be compared by
      hash of the file content *)

  val pair : 'a ops -> 'b ops -> ('a * 'b) ops
  val triple : 'a ops -> 'b ops -> 'c ops -> ('a * 'b * 'c) ops
  val quad : 'a ops -> 'b ops -> 'c ops -> 'd ops -> ('a * 'b * 'c * 'd) ops

  (** {6 Universal type for Values} *)

  type t = Value : 'a ops * 'a -> t

  val pack : 'a ops -> 'a -> t
end

(** {2 On-Disk storage}

    We use a generic interface for on-disk storage, in the form of a
    dictionary [string -> string]. The default storage just uses
    one file per pair. *)
module Storage = Maki_storage

(** {2 Memoized Functions}

    This is the heart of the library: a wrapper around {b pure} functions
    from {!Value} arguments to a {!Value}-aware type. Such functions
    are supposed to always return the same value given the same arguments
    and dependencies (a typical dependency is an external program that
    might have several version).

    The {!call} function is used to actually evaluate a wrapped function,
    or return its memoized result if the computation was done already.

    We need to name functions because, from one execution to another,
    the results of a function call must be stored on disk. Names are used
    to map function calls to their result. If two different functions
    share the same name (even across programs), the results will be
    unpredictable.
*)

val call :
  ?storage:Storage.t ->
  name:string ->
  deps:Value.t list ->
  op:'res Value.ops ->
  (unit -> 'res Lwt.t) ->
  'res Lwt.t

(** {2 Utils} *)

val last_mtime : path -> time or_error
(** Last modification time of the file *)

val sha1 : path -> Sha1.t Lwt.t
(** [sha1 f] hashes the file [f] *)

val sha1_of_string : string -> Sha1.t
(** hash the given string *)

val abspath : path -> path
(** Make the path absolute *)

val shell :
  ?timeout:float -> ?stdin:string ->
  string -> (string * string * int) Lwt.t
(** [shell cmd] runs the command [cmd] and
    returns [stdout, sterr, errcode].
    @param stdin optional input to the sub-process *)

val shellf :
  ?timeout:float -> ?stdin:string ->
  ('a, Format.formatter, unit, (string * string * int) Lwt.t) format4 -> 'a
(** Same as {!shell} but with a format string. Careful with escaping! *)


