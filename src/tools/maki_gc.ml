
(* This file is free software. See file "license" for more details. *)

(** {1 Maki GC implementation} *)

module S = Maki.Storage

open Result
open Lwt.Infix

let force = ref false

let parse_argv () =
  let options =
    Arg.align
      [ "--debug", Arg.Int Maki.Log.set_level, " set debug level";
        "-d", Arg.Int Maki.Log.set_level, " short for --debug";
        "--force", Arg.Set force, " force collection of every object";
      ]
  in
  Arg.parse options (fun _ -> ()) "usage: maki_gc [options]";
  ()

let () =
  parse_argv ();
  (* TODO: also parse which storage to GC *)
  let s = S.get_default () in
  Lwt_main.run (
    Maki.GC.cleanup ~force:!force s
    >>= function
    | Ok stats ->
      Printf.printf "GC done (%s)\n" (Maki.GC.string_of_stats stats);
      Lwt.return ()
    | Error e ->
      Printf.printf "error: %s\n" e;
      exit 1
  )
