open Maki
open Lwt.Infix

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

  let split_dir_ =
    let rec aux acc s =
      let parent = Filename.dirname s in
      if parent = "." || parent="/" then s::acc
      else aux (s::acc) parent
    in
    aux []

  let create dir =
    (* first, create dir (and parents, recursively) *)
    List.iter
      (fun s ->
         try Unix.mkdir s 0o755
         with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
      (split_dir_ dir);
    let t =
      {dir;
       pool=Lwt_pool.create 100 (fun _ -> Lwt.return_unit);
       cache=Hashtbl.create 256
      }
    in
    {
      Storage.name="shelf";
      get=get t;
      set=set t;
      remove=remove t;
      fold=(fun ~f ~x -> fold t ~f ~x);
      flush_cache=flush t;
    }
end

let env_var_ = "MAKI_DIR"

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
