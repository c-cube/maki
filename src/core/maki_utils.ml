
(* This file is free software. See file "license" for more details. *)

(** {1 Various Utils} *)

open Lwt.Infix

type 'a or_error = ('a, string) Result.result

let error msg = Error msg
let errorf msg =
  let buf = Buffer.create 64 in
  let out = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun out -> Format.pp_print_flush out (); error (Buffer.contents buf))
    out msg

(* thread that prints progress *)
module ProgressBar = struct
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
end

(** {2 Caches} *)
module Cache = struct
  type 'a equal = 'a -> 'a -> bool
  type 'a hash = 'a -> int

  let default_eq_ = Pervasives.(=)
  let default_hash_ = Hashtbl.hash

  (** {2 Value interface} *)

  (** Invariants:
      - after [cache.set x y], [get cache x] must return [y] or raise [Not_found]
      - [cache.set x y] is only called if [get cache x] fails, never if [x] is already bound
      - [cache.size()] must be positive and correspond to the number of items in [cache.iter]
      - [cache.iter f] calls [f x y] with every [x] such that [cache.get x = y]
      - after [cache.clear()], [cache.get x] fails for every [x]
  *)
  type ('a,'b) t = {
    set : 'a -> 'b -> unit;
    get : 'a -> 'b;  (* or raise Not_found *)
    size : unit -> int;
    iter : ('a -> 'b -> unit) -> unit;
    clear : unit -> unit;
  }

  let clear c = c.clear ()

  let with_cache c f x =
    try
      c.get x
    with Not_found ->
      let y = f x in
      c.set x y;
      y

  let get c x = try Some (c.get x) with Not_found -> None
  let set c x y = c.set x y

  let size c = c.size ()

  let iter c f = c.iter f

  module Replacing = struct
    type ('a,'b) bucket =
      | Empty
      | Pair of 'a * 'b

    type ('a,'b) t = {
      eq : 'a equal;
      hash : 'a hash;
      arr : ('a,'b) bucket array;
      mutable c_size : int;
    }

    let make eq hash size =
      assert (size>0);
      {arr=Array.make size Empty; eq; hash; c_size=0 }

    let clear c =
      c.c_size <- 0;
      Array.fill c.arr 0 (Array.length c.arr) Empty

    let get c x =
      let i = c.hash x mod Array.length c.arr in
      match c.arr.(i) with
        | Pair (x', y) when c.eq x x' -> y
        | Pair _
        | Empty -> raise Not_found

    let set c x y =
      let i = c.hash x mod Array.length c.arr in
      if c.arr.(i) = Empty then c.c_size <- c.c_size + 1;
      c.arr.(i) <- Pair (x,y)

    let iter c f =
      Array.iter (function Empty -> () | Pair (x,y) -> f x y) c.arr

    let size c () = c.c_size
  end

  let replacing ?(eq=default_eq_) ?(hash=default_hash_) size =
    let c = Replacing.make eq hash size in
    { get=(fun x -> Replacing.get c x);
      set=(fun x y -> Replacing.set c x y);
      clear=(fun () -> Replacing.clear c);
      size=Replacing.size c;
      iter=Replacing.iter c;
    }
end
