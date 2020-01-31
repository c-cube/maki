
(* This file is free software. See file "license" for more details. *)

(** {1 Persistent Storage} *)

(* TODO:
   - optional interface for Memcached
   - optional interface for LMDB
   - optional interface for Sqlite
   - optional interface to DHT
   - composition of storages:
     * like RAID1, duplicate writes, use first terminating read
     * use one storage as a fast cache for the second storage (slower but
       probably more comprehensive; e.g. memcached + DHT for distributed jobs)
   - a dynlink based system for loading other storage systems
   - some system for picking storage from CLI options
*)


open Result
open Lwt.Infix

type path = string

open Maki_err
module E = Maki_lwt_err

type t = {
  name: string;
  get: string -> string option or_error Lwt.t;
  set: string -> string -> unit or_error Lwt.t;
  remove: string -> unit Lwt.t;
  fold: 'a. f:('a -> string * string -> 'a or_error Lwt.t) -> x:'a -> 'a or_error Lwt.t;
  flush_cache: unit -> unit;
}

let name t = t.name
let get t k = t.get k
let set t k v = t.set k v
let remove t k = t.remove k
let fold t ~f ~x = t.fold ~f ~x
let flush_cache t = t.flush_cache ()

let get_exn t k =
  t.get k >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail (Failure e)

let set_exn t k v =
  t.set k v >>= function
  | Ok () -> Lwt.return_unit
  | Error e -> Lwt.fail (Failure e)

let none = {
  name = "dummy storage";
  get = (fun _ -> Lwt.return (Result.Ok None));
  set = (fun _ _ -> Lwt.return (Result.Ok ()));
  remove = (fun _ -> Lwt.return_unit);
  fold = (fun ~f:_ ~x -> Lwt.return (Ok x));
  flush_cache = (fun () -> ());
}

let storage_ = ref (Lazy.from_val none)
let set_default s = storage_ := Lazy.from_val s
let get_default () = Lazy.force !storage_

let to_list st = fold ~x:[] ~f:(fun acc pair -> E.return @@ pair::acc) st
