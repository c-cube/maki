
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations}

    Maki is a system for memoizing costly OCaml functions using the disk.
    It requires the functions to be {b pure}, that is, to always return
    the same result given that the set of {b dependencies} declared by
    the function doesn't change.

    {b status: experimental}

    This module is not thread-safe.
*)

type 'a or_error = ('a, exn) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t

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
    serialize :
      [`Fast of ('a -> Bencode.t)
      | `Slow of ('a -> Bencode.t Lwt.t)
      ];
    unserialize : Bencode.t -> 'a or_error;
  }
  (* ['a ops] describes how to use values of type ['a] as memoized values.
     A value must be serializable, unserializable, and
     it should have a conversion to a {b unique} string; two values that
     have the same canonical form (the string) are considered equal
     w.r.t. caching and recomputations. *)

  val make :
    serialize:[`Fast of ('a -> Bencode.t) | `Slow of ('a -> Bencode.t Lwt.t)]->
    unserialize:(Bencode.t -> 'a or_error) ->
    string -> 'a ops

  val make_fast :
    serialize:('a -> Bencode.t) ->
    unserialize:(Bencode.t -> 'a or_error) ->
    string -> 'a ops

  val make_slow :
    serialize:('a -> Bencode.t Lwt.t)->
    unserialize:(Bencode.t -> 'a or_error) ->
    string -> 'a ops

  val serialize : 'a ops -> 'a -> Bencode.t Lwt.t
  val unserialize : 'a ops -> Bencode.t -> 'a or_error
  val to_string : 'a ops -> 'a -> string Lwt.t
  val of_string : 'a ops -> string -> 'a or_error

  val int : int ops
  val string : string ops
  val bool : bool ops
  val list : 'a ops -> 'a list ops
  val assoc : 'a ops -> (string * 'a) list ops

  val file : path ops
  (** A {b reference} to some file content. This should be compared by
      hash of the file content *)

  val program : program ops
  (** A {b reference} to some program in the path. This will be turned
      into an absolute file path first, then handled same as {!file} *)

  val set : 'a ops -> 'a list ops
  (** [set op] is similar to {!list}, except the order of elements does
      not matter. *)

  val marshal : string -> 'a ops
  (** Use {!Marshal} to store and retrieve data. Caution, this is somewhat
      unsafe, but useful for quick-and-dirty work.
      @param descr the description of the type (see {!make}) *)

  val pair : 'a ops -> 'b ops -> ('a * 'b) ops
  val triple : 'a ops -> 'b ops -> 'c ops -> ('a * 'b * 'c) ops
  val quad : 'a ops -> 'b ops -> 'c ops -> 'd ops -> ('a * 'b * 'c * 'd) ops

  (** {6 Universal type for Values} *)

  type t = Value : 'a ops * 'a -> t

  val argv0 : t
  (** Value corresponding to the program [Sys.argv.(0)] *)

  val pack : 'a ops -> 'a -> t
  val pack_int : int -> t
  val pack_string : string -> t
  val pack_bool : bool -> t
  val pack_file : path -> t
  val pack_list : 'a ops -> 'a list -> t
  val pack_program : program -> t
  val pack_set : 'a ops -> 'a list -> t
  val pack_assoc : 'a ops -> (string * 'a) list -> t
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

(** lifetime for a cached value *)
type lifetime =
  [ `Keep
  | `KeepFor of time (** Time delta *)
  | `KeepUntil of time (** Absolute deadline *)
  | `CanDrop
  ]

val call :
  ?storage:Storage.t ->
  ?lifetime:lifetime ->
  ?limit:Limit.t ->
  name:string ->
  deps:Value.t list ->
  op:'res Value.ops ->
  (unit -> 'res Lwt.t) ->
  'res or_error Lwt.t
(** Call the function iff its result has not been cached yet
    @param storage the storage used for caching values
      (default [Storage.get_default ()])
    @param lifetime how long to keep the cached value (defautl: CanDrop)
    @param limit if given, [call] will acquire a handle from [limit] before
      calling the (potentially costly) function
    @param name the name of the function, should be unique!
    @param deps dependencies of the computation; if they change, the function
      will have to be re-computed. In other words, the function is supposed
      to be pure on the list of dependencies (same dependencies => same result)
*)

val call_exn :
  ?storage:Storage.t ->
  ?lifetime:lifetime ->
  ?limit:Limit.t ->
  name:string ->
  deps:Value.t list ->
  op:'res Value.ops ->
  (unit -> 'res Lwt.t) ->
  'res Lwt.t
(** Same as {!call} but raises the exception instead of wrapping it in Error *)

(** {2 GC}

    Garbage Collection for the stored values. It needs to be called
    explicitely *)

type gc_stats = {
  gc_kept: int;
  gc_removed: int;
}

val string_of_gc_stats : gc_stats -> string

val gc_storage : ?remove_file:bool -> Storage.t -> gc_stats or_error Lwt.t
(** [gc_storage s] removes uneeded values and uneeded dependencies,
    and returns some statistics. It might take a long time.
    @param remove_file if true, when a path result is removed, the
      file it corresponds to is also deleted from the file system *)

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

(* TODO: globbing, for depending on lists of files easily *)

