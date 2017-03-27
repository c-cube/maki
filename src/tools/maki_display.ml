
(* This file is free software. See file "license" for more details. *)

(** {1 Display Storage} *)

open Result
open Lwt.Infix

let collect_entries s : (string * Maki.Cache_val.t) list Lwt.t =
  Maki_log.log 3 "display: collecting values...";
  Maki_storage.fold s ~x:[]
    ~f:(fun acc (key, value) ->
      let b = Bencode.decode (`String value) in
      match Maki.cache_value_of_bencode b with
        | Error e -> Lwt.fail e
        | Ok c -> Lwt.return ((key,c) :: acc))

let print_entries l =
  let pp_lifetime out = function
    | `KeepFor _ -> assert false
    | `KeepUntil t ->
      let now = Unix.gettimeofday () in
      Format.fprintf out "keep for %.1f s" (t -. now)
    | `CanDrop -> Format.pp_print_string out "can drop"
    | `Keep -> Format.pp_print_string out "keep"
  and pp_tags out = function
    | [] -> ()
    | l ->
      Format.fprintf out ", tags(@[<hv>%a)"
        (Format.pp_print_list Format.pp_print_string) l
  in
  let pp_pair out (k,c) =
    Format.fprintf out "@[<hv2>`%s` ->@ `%s`@ [fun: %s, %a%a]@]"
      k (Maki.cache_value_data c) (Maki.cache_value_fun_name c)
      pp_lifetime (Maki.cache_value_lifetime c)
      pp_tags (Maki.cache_value_tags c)
  in
  Format.printf "@[<v2>entries:@ %a@]@."
    (Format.pp_print_list pp_pair) l

let () =
  let options =
    Arg.align
      [ "--debug", Arg.Int Maki_log.set_level, " set debug level"
      ; "-d", Arg.Int Maki_log.set_level, " short for --debug"
      ]
  in
  Arg.parse options (fun _ -> ()) "usage: maki_display [options]";
  (* TODO: also parse which storage to GC *)
  let s = Maki_storage.get_default () in
  Lwt_main.run (
    collect_entries s >|= print_entries
  )
