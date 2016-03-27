
(* This file is free software. See file "license" for more details. *)

(** {1 Maki GC implementation} *)

module S = Maki_storage

open Result
open Lwt.Infix

type cli_options = {
  cli_remove_file: bool;
}

let parse_argv () =
  let rm_file = ref false in
  let options =
    Arg.align
    [ "--rm-files", Arg.Set rm_file, " remove files corresponding to unnedeed results"
    ]
  in
  Arg.parse options (fun _ -> ()) "usage: maki_gc [options]";
  { cli_remove_file= !rm_file }

let () =
  let options = parse_argv () in
  (* TODO: also parse which storage to GC *)
  let s = S.get_default () in
  Lwt_main.run (
    Maki.gc_storage ~remove_file:options.cli_remove_file s
    >>= function
    | Ok stats ->
      Printf.printf "GC done (%s)\n" (Maki.string_of_gc_stats stats);
      Lwt.return ()
    | Error e ->
      Printf.printf "error: %s\n" (Printexc.to_string e);
      exit 1
  )
