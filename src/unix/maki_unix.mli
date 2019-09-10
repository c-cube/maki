(** {2 Values Stored on Disk} *)
open Maki

module Storage : sig
  val default : ?dir:path -> unit -> Storage.t Lwt.t
  (** [default ?dir ()] creates a new default storage (one file per pair)
      @param dir if provided, set the directory used for storing files
        if [dir] is not set, then the current directory is used, unless the
          environment variable "MAKI_DIR" is set
      @raise Unix.Error in case of error, if it could not create [dir] properly *)
end

module ProgressBar = ProgressBar

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

module Hash : sig
  val file_ref : File_ref.t Hash.t
  (** How to hash a file ref *)

  val program_ref : Program_ref.t Hash.t
  (** How to hash a program ref *)
end

val last_mtime : path -> time or_error
(** Last modification time of the file *)

val sha1 : path -> string or_error Lwt.t
(** [sha1 f] hashes the file [f] *)

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
