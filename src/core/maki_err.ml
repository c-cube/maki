
(* This file is free software. See file "license" for more details. *)

(** Error-related Utils *)

open Result

type 'a or_error = ('a, string) result
type 'a lwt_or_error = 'a or_error Lwt.t

let error msg = Error msg
let errorf msg =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun out -> Format.pp_print_flush out (); error (Buffer.contents buf))
    out msg
