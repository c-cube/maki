
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

val error : string -> _ or_error
val errorf : ('a, Format.formatter, unit, 'b or_error) format4 -> 'a

(** {2 Basic types} *)

type path = string
type program = string
type time = float
type hash = string
type encoded_value = string

(** {2 Controlling Parallelism} *)

module Limit : sig
  type t
  val create : int -> t
  val acquire : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val j : unit -> t
  (** Default limiter for concurrency, should be set by CLI options *)

  val set_j : int -> unit
  (** Should be called at the beginning to set the value of [j].
      @raise Failure if [j] is already evaluated *)
end

(** {2 Inputs} *)

(** To memoize a function, Maki must be able to hash the function's input
    arguments. Arguments that hash to the same value are considered
    identical. We use a cryptographic hash to ensure that the probability
    of collisions is astronomically low. *)

module Hash : sig
  type 'a t = Sha1.ctx -> 'a -> unit Lwt.t

  val unit : unit t
  val int : int t
  val bool: bool t
  val string : string t
  val float : float t
  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t

  val map : ('a -> 'b) -> 'b t -> 'a t
  (** [map f hash x] encodes [x] using [f], and then uses [hash]
      to hash [f x]. *)

  val file : path t
  (** A {b reference} to some file content. This should be compared by
      hash of the file content *)

  val program : program t
  (** A {b reference} to some program in the path. This will be turned
      into an absolute file path first, then handled same as {!file} *)

  val set : 'a t -> 'a list t
  (** [set op] is similar to {!list}, except the order of elements does
      not matter. *)

  val marshal : 'a t
  (** Encode the data into a string using marshal, then hash
      the string.
      Caution, this is somewhat unsafe, but useful for quick-and-dirty work. *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  val hash : 'a t -> 'a -> Sha1.t Lwt.t
  (** Hash a value. *)

  val hash_to_string : 'a t -> 'a -> string Lwt.t
  (** Hash a value, then encode the hash into a string. *)
end

(** {2 Codec}

    Maki deals with functions that return values of any
    type that provides a {!'a Codec.t} implementation;
    that is, values that we can serialize and unserialize.
    The reason is that values are stored on disk for memoization purposes.
*)
module Codec : sig
  type 'a t = {
    descr: string;
    encode: 'a -> encoded_value * hash list;
    (** [encode x] should return a string encoding of [x], to be stored
        for the computation of some function, as well as a list of
        hash of sub-values used by [x] (for garbage collection purposes) *)
    decode: encoded_value -> 'a or_error;
    (** Decode the main value from its serialized representation *)
  }

  val make :
    encode:('a -> encoded_value * hash list) ->
    decode:(encoded_value -> 'a or_error) ->
    string ->
    'a t

  val encode : 'a t -> 'a -> encoded_value * hash list
  val decode : 'a t -> encoded_value -> 'a or_error

  val int : int t
  val string : string t
  val bool : bool t
  val float : float t
  val or_error : 'a t -> 'a or_error t

  val marshal : string -> 'a t
  (** [marshal descr] encodes and decodes using marshal.
      Unsafe, but useful for prototyping. *)

  val to_hash : 'a t -> 'a Hash.t
  (** Hashing by encoding, then hashing the encoded value *)
end

(** {2 Arguments of a function} *)

(** A memoized function takes a list of arguments paired with a
    hash function. *)
module Arg : sig
  type t = A : 'a Hash.t * 'a -> t

  val make : 'a Hash.t -> 'a -> t

  val int : int -> t
  val unit : unit -> t
  val bool: bool -> t
  val string : string -> t
  val float : float -> t
  val list : 'a Hash.t -> 'a list -> t
  val array : 'a Hash.t -> 'a array -> t
  val file : path -> t
  val program : program -> t
  val set : 'a Hash.t -> 'a list -> t
  val marshal : 'a -> t
  (** See {!Hash.marshal} *)

  val pair : 'a Hash.t -> 'b Hash.t -> ('a * 'b) -> t
  val triple : 'a Hash.t -> 'b Hash.t -> 'c Hash.t -> ('a * 'b * 'c) -> t
  val quad : 'a Hash.t -> 'b Hash.t -> 'c Hash.t -> 'd Hash.t -> ('a * 'b * 'c * 'd) -> t
end

(** {2 On-Disk storage}

    We use a generic interface for on-disk storage, in the form of a
    dictionary [string -> string]. The default storage just uses
    one file per pair. *)

module Storage = Maki_storage

(** {2 Time Utils} *)

module Time : sig
  type t = time
  val seconds : int -> t
  val hours : int -> t
  val minutes : int -> t
  val days : int -> t
  val now : unit -> t
  val (++) : t -> t -> t
end

(** lifetime for a value on disk *)
type lifetime =
  [ `Keep
  | `KeepFor of time (** Time delta *)
  | `KeepUntil of time (** Absolute deadline *)
  | `CanDrop
  ]

(** {2 Value Stored on Disk} *)

module On_disk_val : sig
  type 'a t = 'a Codec.t * hash

  val store :
    ?storage:Maki_storage.t ->
    ?lifetime:lifetime ->
    'a Codec.t ->
    'a ->
    'a t or_error Lwt.t
  (** [store v x] stores [x] on disk using [v] to encode it *)

  val find :
    ?storage:Maki_storage.t ->
    'a t ->
    'a or_error Lwt.t
  (** [find codec h] fetches the value whose hash is [h], assuming it
      is stored, and decodes it. *)

  val get :
    ?storage:Maki_storage.t ->
    'a t ->
    'a option or_error Lwt.t
  (** [get codec h] fetches the value whose hash is [h], if it is
      stored, and decodes it. *)
end

(** {2 Result of Memoization} *)

module On_disk_result : sig
  type t
  val lifetime : t -> lifetime
  val key : t -> hash
  val data : t -> encoded_value
  val children : t -> hash list
  val tags : t -> string list
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
  ?bypass:bool ->
  ?storage:Storage.t ->
  ?lifetime:lifetime ->
  ?limit:Limit.t ->
  ?tags:string list ->
  name:string ->
  args:Arg.t list ->
  returning:'a Codec.t ->
  (unit -> 'a or_error Lwt.t) ->
  'a or_error Lwt.t
(** Call the function iff its result has not been cached yet
    @param bypass if true, then cache is disabled
    @param storage the storage used for caching values
      (default [Storage.get_default ()])
    @param lifetime how long to keep the cached value (default: [`CanDrop])
    @param limit if given, [call] will acquire a handle from [limit] before
      calling the (potentially costly) function
    @param name the name of the function, should be unique!
    @param deps the dependencies (arguments) of the function
    @param returning how to encode/decode the result on disk
*)

val call_exn :
  ?bypass:bool ->
  ?storage:Storage.t ->
  ?lifetime:lifetime ->
  ?limit:Limit.t ->
  ?tags:string list ->
  name:string ->
  args:Arg.t list ->
  returning:'a Codec.t ->
  (unit -> 'a or_error Lwt.t) ->
  'a Lwt.t
(** Same as {!call} but raises the exception instead of wrapping it in Error *)

(** {2 GC}

    Garbage Collection for the stored values. It needs to be called
    explicitely *)

type gc_stats = {
  gc_kept: int;
  gc_removed: int;
}

val string_of_gc_stats : gc_stats -> string

val gc_storage : Storage.t -> gc_stats or_error Lwt.t
(** [gc_storage s] removes uneeded values and uneeded dependencies,
    and returns some statistics. It might take a long time. *)

(** {2 Utils} *)

val last_mtime : path -> time or_error
(** Last modification time of the file *)

val sha1 : path -> string Lwt.t
(** [sha1 f] hashes the file [f] *)

val sha1_of_string : string -> string
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

