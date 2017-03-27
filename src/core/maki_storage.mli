

(* This file is free software. See file "license" for more details. *)

(** {1 On-Disk Storage} *)

type 'a or_error = ('a, string) Result.result
type path = string

(* TODO:
   - optional interface for Memcached
   - optional interface for LMDB
   - optional interface for Sqlite
   - optional interface to DHT
   - composition of storages:
     * like RAID1, duplicate writes, use first terminating read
     * use one storage as a fast cache for the second storage (slower but
       probably more comprehensive; e.g. memcached + DHT for distributed jobs)
   - a dynlink based system for loading other storage systems
   - some system for picking storage from CLI options
*)

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
