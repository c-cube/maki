
(* This file is free software. See file "license" for more details. *)

(** {1 Various Utils} *)

type 'a or_error = ('a, string) Result.result

val error : string -> _ or_error
val errorf : ('a, Format.formatter, unit, 'b or_error) format4 -> 'a


module ProgressBar : sig
  type t

  val make : n:int -> t

  val stop : t -> unit

  val incr : t -> unit

  val set_count : t -> int -> unit
end

(** {2 Cache} *)
module Cache : sig
  type 'a equal = 'a -> 'a -> bool
  type 'a hash = 'a -> int

  type ('a, 'b) t

  val get : ('a, 'b) t -> 'a -> 'b option

  val set : ('a, 'b) t -> 'a -> 'b -> unit

  val clear : (_,_) t -> unit
  (** Clear the content of the cache *)

  val with_cache : ('a, 'b) t -> ('a -> 'b) -> 'a -> 'b
  (** [with_cache c f] behaves like [f], but caches calls to [f] in the
      cache [c]. It always returns the same value as
      [f x], if [f x] returns, or raise the same exception.
      However, [f] may not be called if [x] is in the cache. *)

  val size : (_,_) t -> int
  (** Size of the cache (number of entries). At most linear in the number
      of entries. *)

  val iter : ('a,'b) t -> ('a -> 'b -> unit) -> unit
  (** Iterate on cached values. Should yield [size cache] pairs. *)

  val replacing : ?eq:'a equal -> ?hash:'a hash ->
    int -> ('a,'b) t
  (** Replacing cache of the given size. Equality and hash functions can be
      parametrized. It's a hash table that handles collisions by replacing
      the old value with the new (so a cache entry is evicted when another
      entry with the same hash (modulo size) is added).
      Never grows wider than the given size. *)
end
