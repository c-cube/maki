
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
type 'a printer = Format.formatter -> 'a -> unit

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
  val lift_ok : 'a Lwt.t -> 'a t
  val lift_err : string Lwt.t -> 'a t
  val unwrap_res : ('a, exn) Result.result -> 'a Lwt.t
  module Infix : sig
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
    val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  end
  include module type of Infix
end

include module type of E.Infix

(** {2 Controlling Parallelism} *)

module Limit : sig
  type t
  val create : int -> t
  val acquire : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  val map_l : t -> ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
  (** [map_l limit f l] maps [f] in parallel on [l], but restricts
      parallelism using [j] *)

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
  (** Encode an atomic value, as a leaf of the dependency graph.
      The value cannot depend on any other value (see {!make} for that) *)

  val encode : 'a t -> 'a -> encoded_value * hash list
  (** [encode codec x] uses the [codec] to encode [x] into a string that
      can be persisted to some {!Storage}.
      It also returns the list of hashes of other values this one
      depends on *)

  val decode : 'a t -> encoded_value -> 'a or_error
  (** [decode codec s] tries to decode the string [s] using [codec]
      into a proper value *)

  val make_bencode:
    encode:('a -> Bencode.t * hash list) ->
    decode:(Bencode.t -> 'a or_error) ->
    string ->
    'a t
  (** Encode via a conversion to Bencode *)

  val make_leaf_bencode:
    encode:('a -> Bencode.t) ->
    decode:(Bencode.t -> 'a or_error) ->
    string ->
    'a t
  (** Encode a leaf via bencode *)

  val int : int t
  val string : string t
  val bool : bool t
  val float : float t
  val or_error : 'a t -> 'a or_error t

  val marshal : string -> 'a t
  (** [marshal descr] encodes and decodes using marshal.
      Unsafe, but useful for prototyping.
      @param descr the (unique) description of this type. It might be
      a good idea to version it to avoid segfaults at decoding time.  *)
end

(** {2 Persistent storage}

    We use a generic interface for persistent storage, in the form of a
    dictionary [string -> string]. The default storage just uses
    one file per pair. *)

module Storage : sig
  type t = {
    name: string;
    get: string -> string option or_error Lwt.t;
    set: string -> string -> unit or_error Lwt.t;
    remove: string -> unit Lwt.t;
    fold: 'a. f:('a -> string * string -> 'a or_error Lwt.t) -> x:'a -> 'a or_error Lwt.t;
    flush_cache: unit -> unit;
  }

  val name : t -> string
  (** Informal description of the storage *)

  val get : t -> string -> string option or_error Lwt.t
  (** [get t k] obtains the value for [k] in [t] *)

  val get_exn : t -> string -> string option Lwt.t

  val set : t -> string -> string -> unit or_error Lwt.t
  (** [set t k v] puts the pair [k -> v] in [t] *)

  val set_exn : t -> string -> string -> unit Lwt.t

  val remove : t -> string -> unit Lwt.t

  val fold : t -> f:('a -> string * string -> 'a or_error Lwt.t) -> x:'a -> 'a or_error Lwt.t
  (** [fold ~f ~x t] folds over all the pairs [key, value] in [t]. *)

  val to_list : t -> (string * string) list or_error Lwt.t
  (** Get all bindings in this storage *)

  val flush_cache : t -> unit
  (** Flush in-process cache, if any *)

  val none : t
  (** A dummy storage which does not store any result, thus forcing
      every computation to run. *)

  val default : ?dir:path -> unit -> t Lwt.t
  (** [default ?dir ()] creates a new default storage (one file per pair)
      @param dir if provided, set the directory used for storing files
        if [dir] is not set, then the current directory is used, unless the
          environment variable "MAKI_DIR" is set
      @raise Unix.Error in case of error, if it could not create [dir] properly *)

  val set_default : t -> unit
  (** Change the storage that is used to evaluate every Maki function *)

  val get_default : unit -> t
end

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
  val pp : t printer
end

(** {2 lifetime for a value on disk}  *)
module Lifetime : sig
  type t =
    | Keep
    | KeepFor of time (** Time delta *)
    | KeepUntil of time (** Absolute deadline *)
    | CanDrop

  val keep : t
  val can_drop : t
  val keep_for : time -> t
  val keep_until : time -> t

  val pp : t printer

  val default : t
  (** Default lifetime for values *)

  (** A few useful lifetimes *)
  val short : t
  val one_minute : t
  val one_hour : t
  val one_day : t
end

(** {2 Values Stored on Disk} *)

module File_ref : sig
  type t
  (** An immutable reference to a file, as a path, with a hash of its
      content.
      If the file changes on the filesystem, the reference becomes
      invalid. *)

  val path : t -> path
  val hash : t -> hash

  val to_string : t -> string

  val make : path -> t or_error Lwt.t
  (** Make a file ref out of a simple path *)

  val make_exn : path -> t Lwt.t
  (** @raise Invalid_argument if the path is not valid *)

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
    ?lifetime:Lifetime.t ->
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

(** {2 Hash function}

    A cryptographic hash function used to map objects to (probably) unique keys *)

module Hash : sig
  module Sha : module type of Digestif.SHA1

  type 'a t = Sha.ctx -> 'a -> Sha.ctx

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

  val hash : 'a t -> 'a -> Sha.t
  (** Hash a value. *)

  val hash_to_string : 'a t -> 'a -> string
  (** Hash a value, then encode the hash into a string. *)

  val of_codec : 'a Codec.t -> 'a t
  (** Hashing by encoding, then hashing the encoded value *)
end

(** {2 Memoized Functions}

    This is the heart of the library: a wrapper around {b pure} functions
    from {!Arg} arguments to a {!Codec}-aware type. Such functions
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

(** {3 High-Level API} *)

module Fun : sig
  type ('f,'f2,'ret) t

  type 'f call_wrap =
    ?bypass:bool ->
    ?storage:Storage.t ->
    ?lifetime:Lifetime.t ->
    ?limit:Limit.t ->
    ?tags:string list ->
    'f
end

val return_ok : 'a -> 'a or_error Lwt.t
val return_fail : string -> 'a or_error Lwt.t

val mk1 :
  ( name:string ->
    'a Hash.t -> 'ret Codec.t ->
    f:(('a -> 'ret or_error Lwt.t) as 'f) ->
    'f) Fun.call_wrap
(** [mk1 ~name h codec ~f] behaves like the unary function [f : 'a -> 'ret]
    but uses the hash function [h] to hash arguments, and [codec] to
    save/restore values from the cache when [f] has already been evaluated
    on a given value.
    @param name is used to distinguish calls to [f] from calls to other
    functions that have the same signature.

    Example: memoizing a recursive function:
{[
let fib =
  let rec fib n = Maki.(
      mk1 ~name:"fib" Hash.int Codec.int ~lifetime:Lifetime.one_minute
        ~f:(fun x -> if x <= 1
          then return_ok 1
          else (fib (x-1) >>= fun x1 ->
            fib (x-2) >|= fun x2 -> x1+x2))
        n
    ) in
  fib;;

fib 42 ;;
(* returns [Ok 42] *)
]}

*)

val mk2 :
  ( name:string ->
    'a Hash.t -> 'b Hash.t -> 'ret Codec.t ->
    f:(('a -> 'b -> 'ret or_error Lwt.t) as 'f) ->
    'f) Fun.call_wrap
(** Binary version of {!mk1}

Example: memoized concatenation of two files :
{[
open Lwt.Infix;;

let concat =
  Maki.(mk2 ~name:"concat" Hash.file_ref Hash.file_ref Codec.string ~lifetime:Lifetime.one_hour
    ~f:(fun f1 f2 ->
      let open E in
      read_file f1 >>= fun content1 ->
      read_file f2 >>= fun content2 ->
      return_ok (content1 ^ content2)))
;;

let x1 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;

(* cached *)
let x2 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;

(* now change contnet of file "foo1", so this should change too *)
let x3 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;

*)

val mk3 :
  ( name:string ->
    'a Hash.t -> 'b Hash.t -> 'c Hash.t -> 'ret Codec.t ->
    f:(('a -> 'b -> 'c -> 'ret or_error Lwt.t) as 'f) ->
    'f) Fun.call_wrap

val mk4 :
  ( name:string ->
    'a Hash.t -> 'b Hash.t -> 'c Hash.t -> 'd Hash.t -> 'ret Codec.t ->
    f:(('a -> 'b -> 'c -> 'd -> 'ret or_error Lwt.t) as 'f) ->
    'f) Fun.call_wrap

val mk5 :
  ( name:string ->
    'a Hash.t -> 'b Hash.t -> 'c Hash.t -> 'd Hash.t -> 'e Hash.t -> 'ret Codec.t ->
    f:(('a -> 'b -> 'c -> 'd -> 'e -> 'ret or_error Lwt.t) as 'f) ->
    'f) Fun.call_wrap

(** {3 Low-Level API} *)

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
  type t = A : 'a Hash.t * 'a -> t
  (** A pair of a value (in case we need to compute) and a hash
      function (to check whether a result is computed already).

      Typically one would use {!@::}:

      - {[  int @:: 42 ]}
      - {[ list string @ ["a"; "b"] ]} *)

  val make : 'a Hash.t -> 'a -> t

  val of_codec : 'a Codec.t -> 'a -> t

  module Infix : sig
    val (@::) : 'a Hash.t -> 'a -> t (** Infix alias to {!make} *)
  end
  include module type of Infix
end

val call :
  ?bypass:bool ->
  ?storage:Storage.t ->
  ?lifetime:Lifetime.t ->
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
  ?lifetime:Lifetime.t ->
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
  val of_lifetime : Lifetime.t -> t
  val codec : t Codec.t
end

module On_disk_record : sig
  type t

  val gc_info : t -> GC_info.t
  val key : t -> hash
  val children : t -> hash list
  val data : t -> encoded_value
  val lifetime : t -> Lifetime.t

  val codec : t Codec.t
end

module GC : sig
  type stats = {
    roots: int;
    kept: int; (* ≥ roots *)
    removed: int;
  }

  val string_of_stats : stats -> string

  val cleanup : ?force:bool -> Storage.t -> stats or_error Lwt.t
  (** [cleanup s] removes uneeded values and uneeded dependencies,
      and returns some statistics. It might take a long time.
      @param force if true, ignore roots and remove every entry *)
end

(** {2 Utils} *)

module Util = Maki_utils

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

val read_file : File_ref.t -> string or_error Lwt.t
(** Read the content of the file *)

val walk :
  ?filter:(path -> bool) ->
  ?recursive:bool ->
  ?which:[`File | `Dir] list ->
  path ->
  path list or_error Lwt.t
(** [walk dir] traverses the directory and yields
    its content, by {b absolute} path.
    @param which filters on the type of the content
    @param recursive if true, walks into subdirectories too
    @param filter filters the absolute path of objects
      and yields only these which satisfy the predicate
*)

(* TODO: globbing, for depending on lists of files easily *)

(** {2 Logging} *)

module Log : sig
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
  (** Log at the given level, using a {!Format}-ready message. This is
      designed to be cheap if the message won't be printed because its
      level is too high.
      Use like this:
      [logf 1 (fun k->k "hello %s, 42=%d" "world" (41+1))] *)

  val default_logger : logger

  val set_logger : logger -> unit

  val set_level : int -> unit
end
