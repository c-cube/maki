
(* This file is free software. See file "license" for more details. *)

(** {1 Maki GC implementation} *)

module S = Maki_storage

open Result
open Lwt.Infix

let parse_argv () =
  let options =
    Arg.align
      [ "--debug", Arg.Int Maki_log.set_level, " set debug level";
        "-d", Arg.Int Maki_log.set_level, " short for --debug";
      ]
  in
  Arg.parse options (fun _ -> ()) "usage: maki_gc [options]";
  ()

let () =
  parse_argv ();
  (* TODO: also parse which storage to GC *)
  let s = S.get_default () in
  Lwt_main.run (
    Maki.gc_storage s
    >>= function
    | Ok stats ->
      Printf.printf "GC done (%s)\n" (Maki.string_of_gc_stats stats);
      Lwt.return ()
    | Error e ->
      Printf.printf "error: %s\n" e;
      exit 1
  )
