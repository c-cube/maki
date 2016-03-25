
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

type 'a or_error = ('a, string) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t
type ('a,'rw) pipe = ('a,'rw) Maki_pipe.t

(** {2 Basic types} *)

type path = string
type program = string

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
  }
  (* TODO: also explain how to hash/compare values
     (e.g. with a to_string_canon function, which, for files, will
     be the hash of the content) *)

  type t = Value : 'a ops * 'a -> t
  (** Universal type, can be hashed, serialized, etc. *)

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

  val serialize : 'a ops -> 'a -> Bencode.t
  val unserialize : 'a ops -> Bencode.t -> 'a or_error
  val to_string : 'a ops -> 'a -> string
  val of_string : 'a ops -> string -> 'a or_error
  val pack : 'a ops -> 'a -> t
end

(** {2 On-Disk storage}

    We use a generic interface for on-disk storage, in the form of a
    dictionary [string -> string]. The default storage just uses
    one file per pair. *)
module Storage : sig
  type t = {
    name: string;
    get: string -> string or_error;
    set: string -> string -> unit or_error;
    iter: unit -> (string * string, [`r]) Maki_pipe.t;
  }

  val default : ?dir:path -> unit -> t
  (** [default ?dir ()] creates a new default storage (one file per pair)
      @param dir if provided, set the directory used for storing files
      if [dir] is not set, then the current directory is used, unless the
        environment variable "MAKI_DIR" is set *)

  val set : t -> unit
  (** Change the storage that is used to evaluate every Maki function *)
end

(** {2 Controlling Parallelism} *)

module J : sig
  val set : int -> unit
  val get : unit -> int
  val acquire : (unit -> 'a Lwt.t) -> 'a Lwt.t
end

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


