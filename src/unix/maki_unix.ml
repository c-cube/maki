module Storage = Storage
open Maki
module Ca = Cache
module ProgressBar = ProgressBar

let (>>>=) = E.(>>=)
let (>>|=) = E.(>|=)

(* last time file [f] was modified *)
let last_time_ f =
  let s = Unix.stat f in
  s.Unix.st_mtime

(* number of threads to use in parallel for computing Sha *)
let sha1_pool_ = Limit.create 20

(* fast sha1 on a file *)
let sha1_exn f =
  Limit.acquire sha1_pool_
    (fun () ->
       Log.logf 5 (fun k->k "compute sha1 of `%s`" f);
       Lwt_preemptive.detach (fun () -> Sha1.file_fast f |> Sha1.to_hex) ())

open Lwt.Infix

let sha1 f =
  Lwt.catch
    (fun () -> sha1_exn f >|= fun x -> Ok x)
    (fun e ->
       errorf "error when computing sha1 of `%s`: %s"
         f (Printexc.to_string e)
       |> Lwt.return)

let abspath f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let last_mtime f : time or_error =
  try Ok (last_time_ f)
  with e ->
    errorf "could not compute `last_mtime %s`: %s" f (Printexc.to_string e)

let errcode_of_status_ =
  fun (Unix.WEXITED c | Unix.WSIGNALED c | Unix.WSTOPPED c) -> c

let shell ?timeout ?(stdin="") cmd0 =
  let cmd = "sh", [|"sh"; "-c"; cmd0|] in
  Lwt.catch
    (fun () ->
       Lwt_process.with_process_full ?timeout cmd
         (fun p ->
            Lwt_io.write p#stdin stdin >>= fun () ->
            Lwt_io.flush p#stdin >>= fun () ->
            let stdout = Lwt_io.read p#stdout
            and stderr = Lwt_io.read p#stderr
            and errcode = p#status
            and close_in = Lwt_io.close p#stdin in
            stdout >>= fun o ->
            stderr >>= fun e ->
            errcode >|= errcode_of_status_ >>= fun c ->
            close_in >>= fun _ ->
            E.return (o, e, c)))
    (fun e ->
       errorf "error when calling `%s`: %s" cmd0 (Printexc.to_string e)
       |> Lwt.return)

let shellf ?timeout ?stdin cmd =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ ->
       Format.pp_print_flush fmt ();
       shell ?timeout ?stdin (Buffer.contents buf))
    fmt cmd

(** {2 References to Files} *)

module File_ref : sig
  type t

  val path : t -> path
  val hash : t -> hash
  val to_string : t -> string

  val make : path -> t or_error Lwt.t
  val make_exn : path -> t Lwt.t
  val is_valid : t -> bool Lwt.t

  val codec : t Codec.t
end = struct
  module B = Bencode

  type t = {
    f_path: path;
    f_hash: hash;
  }

  let path f = f.f_path
  let hash f = f.f_hash

  let to_string f: string =
    Printf.sprintf "{path=`%s`, hash=%s}" (path f) (hash f)

  let of_bencode = function
    | B.Dict l ->
      let open Rresult in
      BM.assoc "path" l >>= BM.as_str >>= fun f_path ->
      BM.assoc "hash" l >>= BM.as_str >>= fun f_hash ->
      Ok {f_path; f_hash;}
    | b -> BM.expected_b "file_state" b

  let to_bencode fs =
    B.Dict
      [ "path", B.String fs.f_path;
        "hash", B.String fs.f_hash;
      ]


  let codec =
    Codec.make_leaf_bencode ~encode:to_bencode ~decode:of_bencode "file_stat"

  (* cache for files: maps file name to hash + last modif *)
  type file_cache = (path, time * t or_error Lwt.t) Ca.t

  let file_cache_ : file_cache = Ca.replacing 512

  let make f : t or_error Lwt.t =
    if not (Sys.file_exists f) then (
      errorf "file `%s` not found" f |> Lwt.return
    ) else (
      let last = last_time_ f in
      match Ca.get file_cache_ f with
      | Some (time,fs) when time >= last -> fs (* cache hit *)
      | _ ->
        let fut =
          sha1 f >>|= fun f_hash ->
          { f_path=f; f_hash;}
        in
        Ca.set file_cache_ f (last,fut);
        fut
    )

  let make_exn f : _ Lwt.t =
    make f >>= function
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail (Invalid_argument e)

  (* check if [f] is a [file_state] that doesn't correspond to the actual
     content of the disk *)
  let is_valid (f:t): bool Lwt.t =
    Log.logf 5 (fun k->k "check if file %s is up-to-date..." (to_string f));
    (* compare [fs] with actual current file state *)
    make (path f) >>= fun res ->
    begin match res with
      | Error _ -> Lwt.return_true (* file not present, etc. *)
      | Ok f' ->
        let res =
          path f = path f' &&
          hash f = hash f'
        in
        Log.logf 5 (fun k->k "file %s up-to-date? %B" (to_string f) res);
        Lwt.return res
    end
end

module Program_ref : sig
  type t
  val find : path -> path or_error Lwt.t
  val make : path -> t or_error Lwt.t
  val as_file : t -> File_ref.t
  val codec : t Codec.t
  val to_string : t -> string
end = struct
  type t = File_ref.t

  let as_file (p:t): File_ref.t = p
  let codec = File_ref.codec

  (* program name -> path *)
  let path_tbl_ : (string, string or_error Lwt.t) Ca.t = Ca.replacing 64

  let path_pool_ = Lwt_pool.create 100 (fun _ -> Lwt.return_unit)

  (* turn [f], a potentially relative path to a program, into an absolute path *)
  let find (f:path) : path or_error Lwt.t =
    if Filename.is_relative f && Filename.is_implicit f
    then match Ca.get path_tbl_ f with
      | Some r -> r
      | None ->
        let fut =
          Lwt_pool.use path_pool_
            (fun _ ->
               Log.logf 5 (fun k->k "invoke `which` on `%s`" f);
               let p = Lwt_process.open_process_in ("", [|"which"; f|]) in
               Lwt_io.read p#stdout >>= fun out ->
               p#status >|= errcode_of_status_ >|= fun errcode ->
               if errcode=0
               then Ok (String.trim out)
               else errorf "program `%s` not found in path" f)
        in
        (* cache *)
        Ca.set path_tbl_ f fut;
        fut
    else Lwt.return (Ok f)

  let make (f:path) = find f >>>= File_ref.make

  let to_string = File_ref.to_string
end

module Hash = struct
  open Hash

  let file_ref ctx (f:File_ref.t) =
    let h = File_ref.hash f in
    str_ (str_ ctx (File_ref.path f)) h

  let program_ref ctx (p:Program_ref.t) =
    let h = Program_ref.as_file p in
    file_ref ctx h
end

let read_file (f:File_ref.t) : string or_error Lwt.t =
  let s = File_ref.path f in
  Lwt.catch
    (fun () ->
       Lwt_io.with_file ~mode:Lwt_io.Input s
         (fun ic -> Lwt_io.read ic |> E.lift_ok))
    (fun e ->
       E.fail (Printexc.to_string e))

let walk
    ?(filter=fun _ -> true)
    ?(recursive=true)
    ?(which=[`File;`Dir])
    dir =
  let dir = abspath dir in
  let rec walk ~rec_ acc file =
    if not (Sys.file_exists file) then acc
    else if not (filter file) then acc
    else (
      (* yield this particular file? *)
      let acc =
        if filter file &&
           ((Sys.is_directory file &&
             List.mem `Dir which) ||
            (not (Sys.is_directory file) &&
             List.mem `File which))
        then file :: acc
        else acc
      in
      if Sys.is_directory file then (
        (* try to list the directory *)
        let arr = try Sys.readdir file with Sys_error _ -> [||] in
        Array.fold_left
          (fun acc sub ->
             (* abspath *)
             let sub = Filename.concat file sub in
             walk ~rec_:(rec_ && recursive) acc sub)
          acc arr
      ) else acc
    )
  in
  try walk ~rec_:true [] dir |> E.return
  with e -> E.fail (Printexc.to_string e)
