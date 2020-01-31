(** Thread that prints progress *)
open Lwt.Infix

let nb_sec_minute = 60
let nb_sec_hour = 60 * nb_sec_minute
let nb_sec_day = 24 * nb_sec_hour

(* how to print the time *)
let time_string f =
  let n = int_of_float f in
  let aux n div = n / div, n mod div in
  let n_day, n = aux n nb_sec_day in
  let n_hour, n = aux n nb_sec_hour in
  let n_min, n = aux n nb_sec_minute in
  let print_aux s n = if n <> 0 then (string_of_int n) ^ s else "" in
  (print_aux "d" n_day) ^
    (print_aux "h" n_hour) ^
    (print_aux "m" n_min) ^
    (string_of_int n) ^ "s"

type t = {
  thread: unit Lwt.t lazy_t;
  start: float; (* timestamp *)
  n: int;  (* max *)
  mutable cur: int;
}

let rec loop t =
  let time_elapsed = Unix.gettimeofday () -. t.start in
  let len_bar = 30 in
  let bar =
    String.init len_bar
      (fun i -> if i * t.n <= len_bar * t.cur then '#' else ' ') in
  let percent = if t.n=0 then 100 else (t.cur * 100) / t.n in
  Lwt_io.printf "\r... %5d/%d | %3d%% [%6s: %s]"
    t.cur t.n percent (time_string time_elapsed) bar
  >>= fun () ->
  Lwt_io.flush Lwt_io.stdout >>= fun () ->
  if t.cur = t.n
  then
    Lwt_io.printl "" >>= fun () ->
    Lwt_io.flush Lwt_io.stdout
  else
    Lwt_unix.sleep 0.2 >>= fun () -> loop t

let make ~n =
  let start = Unix.gettimeofday () in
  let rec t = { thread=lazy (loop t); start; n; cur=0; } in
  ignore (Lazy.force t.thread);
  t

let stop t = Lwt.cancel (Lazy.force t.thread)

let incr t = t.cur <- t.cur + 1

let set_count t m =
  assert (m >= 0 && m <= t.n);
  t.cur <- m
