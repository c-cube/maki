
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

(** {2 Error Handling} *)
module E : sig
  type 'a t = 'a or_error Lwt.t

  val return : 'a -> 'a t
  val return_unit : unit t
  val fail : string -> _ t
  val unwrap_res : ('a, exn) Result.result -> 'a Lwt.t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
end

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

  val make_leaf :
    encode:('a -> encoded_value) ->
    decode:(encoded_value -> 'a or_error) ->
    string ->
    'a t

  val encode : 'a t -> 'a -> encoded_value * hash list
  val decode : 'a t -> encoded_value -> 'a or_error

  val make_bencode:
    encode:('a -> Bencode.t * hash list) ->
    decode:(Bencode.t -> 'a or_error) ->
    string ->
    'a t

  val make_leaf_bencode:
    encode:('a -> Bencode.t) ->
    decode:(Bencode.t -> 'a or_error) ->
    string ->
    'a t

  val int : int t
  val string : string t
  val bool : bool t
  val float : float t
  val or_error : 'a t -> 'a or_error t

  val marshal : string -> 'a t
  (** [marshal descr] encodes and decodes using marshal.
      Unsafe, but useful for prototyping. *)
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
  val weeks : int -> t
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

val default_lifetime : lifetime
(** Default lifetime for values *)

(** {2 Value Stored on Disk} *)

module File_ref : sig
  type t
  (** An immutable reference to a file, as a path, with a hash of its
      content.
      If the file changes on the filesystem, the reference becomes
      invalid. *)

  val path : t -> path
  val hash : t -> hash

  val to_string : t -> string

  val compute : path -> t or_error Lwt.t
  (** Make a file ref out of a simple path *)

  val is_valid : t -> bool Lwt.t
  (** Check if the reference is up-to-date (i.e. the file content
      did not change) *)

  val codec : t Codec.t
end

module Program_ref : sig
  type t

  val find : path -> path or_error Lwt.t

  val make : path -> t or_error Lwt.t

  val as_file : t -> File_ref.t

  val codec : t Codec.t

  val to_string : t -> string
end

(** {6 Reference to On-Disk Value} *)
module Ref : sig
  type 'a t = 'a Codec.t * hash
  (** A reference to some value of type ['a], referred to by the
      hash of the value.
      The codec is there to deserialize the value when it's dereferenced. *)

  val hash : _ t -> hash
  (** Recover the hash corresponding to the reference. *)

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
      is stored, and decodes it. Returns an error if the value is
      not present. *)

  val get :
    ?storage:Maki_storage.t ->
    'a t ->
    'a option or_error Lwt.t
  (** [get codec h] fetches the value whose hash is [h], if it is
      stored, and decodes it. *)
end

(** {2 Arguments of Memoized Functions} *)

(** To memoize a function, Maki must be able to hash the function's input
    arguments. Arguments that hash to the same value are considered
    identical. We use a cryptographic hash to ensure that the probability
    of collisions is astronomically low.

    An argument is then the pair of the value and its hash function;
    if the result is stored (by the computation's hash), we return it,
    otherwise we compute the value.

    Example: to pass a [int list] as argument:
    {[
      Arg.(Hash.(list int) @:: [41; 0; 1] )
    ]}
*)
module Arg : sig
  module Hash : sig
    type 'a t = Sha1.ctx -> 'a -> unit

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

    val file_ref : File_ref.t t
    (** How to hash a file ref *)

    val program_ref : Program_ref.t t
    (** How to hash a program ref *)

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

    val hash : 'a t -> 'a -> Sha1.t
    (** Hash a value. *)

    val hash_to_string : 'a t -> 'a -> string
    (** Hash a value, then encode the hash into a string. *)

    val of_codec : 'a Codec.t -> 'a t
    (** Hashing by encoding, then hashing the encoded value *)
  end

  type t = A : 'a Hash.t * 'a -> t
  (** A pair of a value (in case we need to compute) and a hash
      function (to check whether a result is computed already).
  
      Typically one would use {!@::}:

      - {[  int @:: 42 ]}
      - {[ list string @ ["a"; "b"] ]} *)

  val make : 'a Hash.t -> 'a -> t

  module Infix : sig
    val (@::) : 'a Hash.t -> 'a -> t (** Infix alias to {!make} *)
  end
  include module type of Infix
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

val call_pure :
  ?bypass:bool ->
  ?storage:Storage.t ->
  ?lifetime:lifetime ->
  ?limit:Limit.t ->
  ?tags:string list ->
  name:string ->
  args:Arg.t list ->
  returning:'a Codec.t ->
  (unit -> 'a Lwt.t) ->
  'a or_error Lwt.t

(** {2 GC}

    Garbage Collection for the stored values. It needs to be called
    explicitely *)

module GC_info : sig
  type t =
    | Keep
    | KeepUntil of time
    | CanDrop
  val lt : t -> t -> bool
  val of_lifetime : lifetime -> t
  val codec : t Codec.t
end

module On_disk_record : sig
  type t

  val gc_info : t -> GC_info.t
  val key : t -> hash
  val children : t -> hash list
  val data : t -> encoded_value
  val lifetime : t -> lifetime

  val codec : t Codec.t
end

module GC : sig
  type stats = {
    roots: int;
    kept: int; (* ≥ roots *)
    removed: int;
  }

  val string_of_stats : stats -> string

  val cleanup : Storage.t -> stats or_error Lwt.t
  (** [cleanup s] removes uneeded values and uneeded dependencies,
      and returns some statistics. It might take a long time. *)
end

(** {2 Utils} *)

val last_mtime : path -> time or_error
(** Last modification time of the file *)

val sha1 : path -> string or_error Lwt.t
(** [sha1 f] hashes the file [f] *)

val sha1_of_string : string -> string
(** hash the given string *)

val abspath : path -> path
(** Make the path absolute *)

val shell :
  ?timeout:float -> ?stdin:string ->
  string ->
  (string * string * int) or_error Lwt.t
(** [shell cmd] runs the command [cmd] and
    returns [stdout, sterr, errcode].
    @param stdin optional input to the sub-process *)

val shellf :
  ?timeout:float -> ?stdin:string ->
  ('a, Format.formatter, unit, (string * string * int) or_error Lwt.t) format4
  -> 'a
(** Same as {!shell} but with a format string. Careful with escaping! *)

(* TODO: globbing, for depending on lists of files easily *)

