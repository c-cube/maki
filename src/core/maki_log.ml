
(* This file is free software. See file "license" for more details. *)

(** {1 Logs} *)

type logger = {
  log: 'a.
    int ->
    ((('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a) -> unit) ->
    unit
}

let level_ = ref 1
let set_level i = level_ := i

let debug_fmt_ = Format.err_formatter

let default_logger = {
  log=(
    fun i f ->
      if i <= !level_
      then (
        f
          (fun fmt ->
            Format.kfprintf
              (fun _ -> ())
              debug_fmt_
              ("@[<2>maki@ " ^^ fmt ^^ "@]@."))
      )
  )
}

let log_ : logger ref = ref default_logger

let log i s = (!log_).log i (fun k->k "%s" s)
let logf i k = (!log_).log i k

let set_logger l = log_ := l
