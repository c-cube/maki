
(* This file is free software. See file "license" for more details. *)

(** {1 Display Storage} *)

open Lwt.Infix

type 'a or_error = 'a Maki.or_error

let collect_entries s : (string * Maki.On_disk_record.t) list or_error Lwt.t =
  Maki_log.log 3 "display: collecting values...";
  Maki_storage.fold s ~x:[]
    ~f:(fun acc (key, value) ->
      let open Maki.E in
      (Maki.Codec.decode Maki.On_disk_record.codec value |> Lwt.return)
      >|= fun c -> (key,c) :: acc)

let print_entries l =
  let pp_lifetime out = function
    | `KeepFor _ -> assert false
    | `KeepUntil t ->
      let now = Unix.gettimeofday () in
      Format.fprintf out "keep for %.1f s" (t -. now)
    | `CanDrop -> Format.pp_print_string out "can drop"
    | `Keep -> Format.pp_print_string out "keep"
  in
  let pp_pair out (k,c) =
    Format.fprintf out "@[<hv2>`%s` ->@ `%s`@ [key: `%s`, %a]@]"
      k (Maki.On_disk_record.data c) (Maki.On_disk_record.key c)
      pp_lifetime (Maki.On_disk_record.lifetime c)
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
    let res =
      let open Maki.E in
      collect_entries s >|= print_entries
    in
    res >>= function
    | Result.Ok () -> Lwt.return_unit
    | Result.Error msg ->
      Lwt.fail (Failure msg)
  )
