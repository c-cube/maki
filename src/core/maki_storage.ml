
(* This file is free software. See file "license" for more details. *)

(** {1 On-Disk Storage} *)

open Result
open Lwt.Infix

type 'a or_error = ('a, string) Result.result
type path = string

let error = Maki_utils.error
let errorf = Maki_utils.errorf

type t = {
  name: string;
  get: string -> string option or_error Lwt.t;
  set: string -> string -> unit or_error Lwt.t;
  remove: string -> unit Lwt.t;
  fold: 'a. f:('a -> string * string -> 'a or_error Lwt.t) -> x:'a -> 'a or_error Lwt.t;
  flush_cache: unit -> unit;
}

let env_var_ = "MAKI_DIR"

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

module Default = struct
  type t = {
    pool: unit Lwt_pool.t;
    dir: path;
    cache: (string, string option or_error) Hashtbl.t;
  }

  let k_to_file t f = Filename.concat t.dir f

  let read_file_ f =
    Lwt_io.with_file ~mode:Lwt_io.input f (fun ic -> Lwt_io.read ic)

  let get_ t k =
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

  let get t k = Lwt_pool.use t.pool (fun _ -> get_ t k)

  let set_ t k v =
    Lwt.catch
      (fun () ->
        let f = k_to_file t k in
        Lwt_io.with_file f
          ~mode:Lwt_io.output ~flags:[Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
          ~perm:0o644
          (fun oc ->
            (* invalidate cache *)
            Hashtbl.replace t.cache k (Ok(Some v));
            Lwt_io.write oc v >>= fun () ->
            Lwt_io.flush oc)
        >|= fun () -> Ok ()
      )
      (fun e ->
         errorf "storage: error when writing `%s`: %s" k (Printexc.to_string e)
         |> Lwt.return)

  let set t k v = Lwt_pool.use t.pool (fun _ -> set_ t k v)

  let remove t k =
    let f = k_to_file t k in
    Sys.remove f;
    Lwt.return_unit

  let fold t ~f ~x:acc =
    let dir = Unix.opendir t.dir in
    let rec aux acc =
      match Unix.readdir dir with
      | k ->
        let file = k_to_file t k in
        if Sys.is_directory file
        then aux acc (* ignore directories *)
        else (
          read_file_ file >>= fun value ->
          f acc (k,value) >>=
          function
          | Ok acc -> aux acc
          | Error e -> Lwt.return (Error e)
        )
      | exception (Unix.Unix_error _ as e) ->
        Unix.closedir dir;
        Lwt.fail e
      | exception End_of_file ->
        Lwt.return (Ok acc)
    in
    aux acc

  let flush t () = Hashtbl.clear t.cache

  let create dir =
    (* first, create dir *)
    begin try Unix.mkdir dir 0o755
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end;
    let t =
      {dir;
       pool=Lwt_pool.create 100 (fun _ -> Lwt.return_unit);
       cache=Hashtbl.create 256
      }
    in
    {
      name="shelf";
      get=get t;
      set=set t;
      remove=remove t;
      fold=(fun ~f ~x -> fold t ~f ~x);
      flush_cache=flush t;
    }
end

let none = {
  name = "dummy storage";
  get = (fun _ -> Lwt.return (Result.Ok None));
  set = (fun _ _ -> Lwt.return (Result.Ok ()));
  remove = (fun _ -> Lwt.return_unit);
  fold = (fun ~f:_ ~x -> Lwt.return (Ok x));
  flush_cache = (fun () -> ());
}

let default_ ?dir () =
  let dir = match dir with
    | Some d -> d
    | None ->
      try Sys.getenv env_var_
      with Not_found ->
        let dir =
          try Sys.getenv "XDG_CACHE_HOME"
          with Not_found ->
            Filename.concat
              (try Sys.getenv "HOME" with Not_found -> "/tmp/")
              ".cache"
        in
        Filename.concat dir "maki"
  in
  Default.create dir

let default ?dir () = Lwt.return (default_ ?dir ())

let storage_ = ref (lazy (default_ ()))
let set_default s = storage_ := Lazy.from_val s
let get_default () = Lazy.force !storage_

