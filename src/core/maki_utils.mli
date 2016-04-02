
(* This file is free software. See file "license" for more details. *)

(** {1 Various Utils} *)

module ProgressBar : sig
  type t

  val make : n:int -> t

  val stop : t -> unit

  val incr : t -> unit

  val set_count : t -> int -> unit
end
