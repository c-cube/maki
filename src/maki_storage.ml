
(* This file is free software. See file "license" for more details. *)

(** {1 On-Disk Storage} *)

open Result
open Lwt.Infix

type 'a or_error = ('a, string) Result.result
type path = string

type t = {
  name: string;
  get: string -> string option or_error Lwt.t;
  set: string -> string -> unit or_error Lwt.t;
  iter: ?preload:int -> unit -> (string * string, [`r]) Maki_pipe.t;
  flush_cache: unit -> unit;
}

let env_var_ = "MAKI_DIR"

let get t k = t.get k
let set t k v = t.set k v
let iter ?preload t = t.iter ?preload ()
let flush_cache t = t.flush_cache ()

exception Storage_error of string

let get_exn t k =
  t.get k >>= function
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail (Storage_error e)

let find t k =
  t.get k >>= function
    | Ok (Some x) -> Lwt.return x
    | Ok None -> Lwt.fail Not_found
    | Error e -> Lwt.fail (Storage_error e)

let set_exn t k v =
  t.set k v >>= function
    | Ok () -> Lwt.return_unit
    | Error e -> Lwt.fail (Storage_error e)

module Default = struct
  type t = {
    dir: path;
    cache: (string, string option or_error) Hashtbl.t;
  }

  let k_to_file t f = Filename.concat t.dir f

  let read_file_ f =
    Lwt_io.with_file ~mode:Lwt_io.input f (Lwt_io.read ~count:2048)

  let get t k =
    try Lwt.return (Hashtbl.find t.cache k)
    with Not_found ->
      Lwt.catch
        (fun () ->
          let f = k_to_file t k in
          if Sys.file_exists f
          then read_file_ f >|= fun x -> Ok (Some x)
          else Lwt.return (Ok None))
        (fun e ->
          Lwt.return (Error (Printexc.to_string e)))
      >|= fun res ->
      Hashtbl.add t.cache k res;
      res

  let set t k v =
    Lwt.catch
      (fun () ->
        let f = k_to_file t k in
        Lwt_io.with_file f
          ~mode:Lwt_io.output ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
          ~perm:0o644
          (fun oc ->
            (* invalidate cache *)
            Hashtbl.remove t.cache k;
            Lwt_io.write oc v >>= fun () ->
            Lwt_io.flush oc)
        >|= fun () -> Ok ()
      )
      (fun e ->
        Lwt.return (Error (Printexc.to_string e)))

  let iter t ?(preload=16) () =
    let module P = Maki_pipe in
    let d = Unix.opendir t.dir in
    let pipe = P.create ~max_size:preload () in
    (* push directory entries into [pipe] *)
    let rec push pipe dir =
      match Unix.readdir dir with
      | k ->
          read_file_ k >>= fun value ->
          P.write pipe (k,value) >>= fun () ->
          push pipe dir
      | exception End_of_file -> P.close pipe
    in
    Lwt.async (fun () -> push pipe d);
    pipe

  let flush t () = Hashtbl.clear t.cache

  let create dir =
    (* first, create dir *)
    begin try Unix.mkdir dir 0o755
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end;
    let t = {dir; cache=Hashtbl.create 256} in
    {
      name="shelf";
      get=get t;
      set=set t;
      iter=iter t;
      flush_cache=flush t;
    }
end

let default_ ?dir () =
  let dir = match dir with
    | Some d -> d
    | None ->
      try Sys.getenv env_var_
      with Not_found ->
        ".maki/"
  in
  Default.create dir

let default ?dir () = Lwt.return (default_ ?dir ())

let storage_ = ref (lazy (default_ ()))
let set_default s = storage_ := Lazy.from_val s
let get_default () = Lazy.force !storage_

