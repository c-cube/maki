
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

open Result
open Lwt.Infix

module B = Bencode
module BM = Maki_bencode
module Ca = Maki_utils.Cache

module LwtErr = Maki_lwt_err

let (>>>=) = LwtErr.(>>=)
let (>>|=) = LwtErr.(>|=)

type 'a or_error = ('a, string) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t

let error = Maki_utils.error
let errorf msg = Maki_utils.errorf msg

module Res_ = struct
  let return x = Ok x
  let fail e = Error e
  let (<*>) f x = match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error e, _
    | _, Error e -> Error e
  let (>|=) r f = match r with
    | Ok x -> Ok (f x)
    | Error e -> Error e
  let map f x = x >|= f
  let (>>=) r f = match r with
    | Ok x -> f x
    | Error e -> Error e

  exception Exit_map of string

  let map_l f l =
    try
      let res =
        List.map
        (fun x -> match f x with Ok y -> y | Error e -> raise (Exit_map e))
        l
      in
      Ok res
    with Exit_map msg -> Error msg
end

(** {2 Controlling Parallelism} *)

module Limit = struct
  type t = unit Lwt_pool.t

  let create size = Lwt_pool.create size (fun () -> Lwt.return_unit)

  let acquire = Lwt_pool.use

  let j_init = ref 1

  (* pool used to limit concurrent access to cores, memory, etc. *)
  let j_ = lazy (create !j_init)

  let j () = Lazy.force j_

  let set_j n =
    if Lazy.is_val j_ then failwith "Limit.set_j: too late to set limit";
    j_init := n
end

(** {2 Basic types} *)

type path = string
type program = string
type time = float
type hash = string
type encoded_value = string

(** {2 Utils} *)

let last_time_ f =
  let s = Unix.stat f in
  s.Unix.st_mtime

(* number of threads to use in parallel for computing Sha1 *)
let sha1_pool_ = Limit.create 20

(* fast sha1 on a file *)
let sha1 f =
  Limit.acquire sha1_pool_
    (fun () ->
       Maki_log.logf 5 (fun k->k "compute sha1 of `%s`" f);
       Lwt_preemptive.detach (fun () -> Sha1.file_fast f |> Sha1.to_hex) ())

let abspath f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let sha1_of_string s = Sha1.string s |> Sha1.to_hex

let last_mtime f : time or_error =
  try Ok (last_time_ f)
  with e ->
    errorf "could not compute `last_mtime %s`: %s" f (Printexc.to_string e)

let errcode_of_status_ =
  fun (Unix.WEXITED c | Unix.WSIGNALED c | Unix.WSTOPPED c) -> c

let shell ?timeout ?(stdin="") cmd =
  let cmd = "sh", [|"sh"; "-c"; cmd|] in
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
       Lwt.return (o, e, c))

let shellf ?timeout ?stdin cmd =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ ->
       Format.pp_print_flush fmt ();
       shell ?timeout ?stdin (Buffer.contents buf))
    fmt cmd

(** {2 Values} *)

type file_state = {
  fs_path: path;
  fs_hash: string Lwt.t; (* SHA1 in hex form *)
}

(* cache for files: maps file name to hash + last modif *)
type file_cache = (string, time * file_state) Ca.t

let file_cache_ : file_cache = Ca.replacing 512

let compute_file_state_ f : file_state or_error =
  if not (Sys.file_exists f) then (
    errorf "file `%s` not found" f
  ) else match Ca.get file_cache_ f with
  | Some (time, fs) ->
    (* cache hit, but is it up-to-date? *)
    let time' = last_time_ f in
    if time' <> time
    then (
      Maki_log.logf 5 (fun k->k "hash entry for `%s` invalidated: %.2f --> %.2f" f time time');
      let fs_hash = sha1 f in
      let fs = {fs with fs_hash} in
      Ca.set file_cache_ f (time', fs);
      Ok fs
    )
    else Ok fs
  | None ->
    let time = last_time_ f in
    let fs_hash = sha1 f in
    let fs = { fs_path=f; fs_hash} in
    Ca.set file_cache_ f (time, fs);
    Ok fs

(* program name -> path *)
let path_tbl_ : (string, string or_error Lwt.t) Ca.t = Ca.replacing 64

let path_pool_ = Lwt_pool.create 100 (fun _ -> Lwt.return_unit)

(* turn [f], a potentially relative path to a program, into an absolute path *)
let find_program_path_ f : string or_error Lwt.t =
  if Filename.is_relative f && Filename.is_implicit f
  then match Ca.get path_tbl_ f with
    | Some r -> r
    | None ->
      let fut =
        Lwt_pool.use path_pool_
          (fun _ ->
             Maki_log.logf 5 (fun k->k "invoke `which` on `%s`" f);
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

(** {2 hash input values} *)
module Hash = struct
  type 'a t = Sha1.ctx -> 'a -> unit Lwt.t

  let hash (h:_ t) x =
    let buf = Sha1.init() in
    h buf x >|= fun () ->
    Sha1.finalize buf

  let hash_to_string h x = hash h x >|= Sha1.to_hex

  let str_ ctx s = Sha1.update_string ctx s

  let unit _ _ = Lwt.return_unit
  let int ctx x = str_ ctx (string_of_int x); Lwt.return_unit
  let bool ctx x = str_ ctx (string_of_bool x); Lwt.return_unit
  let float ctx x = str_ ctx (string_of_float x); Lwt.return_unit
  let string ctx x = str_ ctx x; Lwt.return_unit

  let map f h ctx x = h ctx (f x)

  let list h ctx l = Lwt_list.iter_s (h ctx) l
  let array h ctx a = list h ctx (Array.to_list a)

  let file_state ctx (fs:file_state): unit Lwt.t =
    fs.fs_hash >|= fun h ->
    str_ ctx fs.fs_path;
    str_ ctx h

  (* special behavior for files (comparison is done be by timestamp+hash)
     look into cache for the file_state, compare timestamp,
     and compute hash if timestamp changed or file is not in cache *)
  let file ctx f =
    let f = abspath f in
    begin match compute_file_state_ f with
      | Error e ->  Lwt.fail (Failure e)
      | Ok fs -> file_state ctx fs
    end

  (* special behavior for programs: find the path to the binary,
     then behave like a file *)
  let program ctx f: unit Lwt.t =
    find_program_path_ f >>= function
    | Error e -> Lwt.fail (Failure e)
    | Ok p ->
      begin match compute_file_state_ p with
        | Error e -> Lwt.fail (Failure e)
        | Ok fs -> file_state ctx fs
      end

  let pair h1 h2 ctx (x1,x2) =
    h1 ctx x1 >>= fun () ->
    h2 ctx x2

  let triple h1 h2 h3 ctx (x1,x2,x3) =
    h1 ctx x1 >>= fun () ->
    h2 ctx x2 >>= fun () ->
    h3 ctx x3

  let quad h1 h2 h3 h4 ctx (x1,x2,x3,x4) =
    h1 ctx x1 >>= fun () ->
    h2 ctx x2 >>= fun () ->
    h3 ctx x3 >>= fun () ->
    h4 ctx x4

  (* set: orderless. We compute all hashes, sort, then hash the resulting list *)
  let set h ctx l =
    Lwt_list.map_s (fun x -> hash_to_string h x) l >|= fun l ->
    let l = List.sort String.compare l in
    List.iter (str_ ctx) l

  let marshal: 'a t = fun ctx x ->
    str_ ctx (Marshal.to_string x [Marshal.Closures]); Lwt.return_unit
end

(** {2 Encoder/Decoder} *)

module Codec = struct
  type hash = string
  type encoded_value = string

  type 'a t = {
    descr: string;
    encode: 'a -> encoded_value * hash list;
    (** [encode x] should return a string encoding of [x], to be stored
        for the computation of some function, as well as a list of
        hash of sub-values used by [x] (for garbage collection purposes) *)
    decode: encoded_value -> 'a or_error;
    (** Decode the main value from its serialized representation *)
  }

  let make ~encode ~decode descr =
    { descr; encode; decode; }

  let encode v x = v.encode x
  let decode v s = v.decode s

  let make1 ~encode ~decode descr =
    make ~encode:(fun x -> encode x, []) ~decode descr

  let int =
    make1 "int"
      ~encode:string_of_int
      ~decode:(fun s ->
        begin
          try Ok (int_of_string s)
          with _ -> errorf "expected int, got `%s`" s
        end)

  let string =
    make1 "string" ~encode:(fun s->s) ~decode:(fun s->Ok s)

  let bool =
    make1 "bool"
      ~encode:string_of_bool
      ~decode:(fun s ->
        begin try Ok (bool_of_string s)
          with _ -> errorf "expected bool, got `%s`" s
        end)

  (* TODO: use the hexadecimal output of Printf when available ? *)
  let float =
    make1 "float"
      ~encode:(fun f -> Int64.((to_string @@ bits_of_float f)))
      ~decode:(fun s ->
        begin try Ok Int64.(float_of_bits @@ of_string s)
          with Failure _ -> errorf "expected float, got `%s`" s
        end)

  let marshal name =
    let flags = [Marshal.Closures; Marshal.No_sharing; Marshal.Compat_32] in
    make1 name
      ~encode:(fun x -> Marshal.to_string x flags)
      ~decode:(fun s -> Marshal.from_string s 0)

  let or_error (c:'a t): 'a or_error t =
    let name = c.descr ^ " or_error" in
    make name
      ~encode:(function
        | Ok x -> let s, deps = encode c x in "1" ^ s, deps
        | Error y -> "0" ^ y, [])
      ~decode:(fun s ->
        let open Res_ in
        if s<>"" && s.[0] = '0'
        then decode c (String.sub s 1 (String.length s-1)) >|= Res_.return
        else if s<>"" && s.[0] = '1'
        then String.sub s 1 (String.length s-1) |> Res_.fail
        else errorf "expected %s, got `%s`" name s)

  let to_hash (c:'a t): 'a Hash.t =
    fun ctx x ->
      let s, _ = encode c x in
      Hash.string ctx s
end

(** {2 Arguments} *)
module Arg = struct
  type t = A : 'a Hash.t * 'a -> t

  let make h x = A(h,x)

  let int = make Hash.int
  let unit = make Hash.unit
  let bool= make Hash.bool
  let string = make Hash.string
  let float = make Hash.float
  let list h = make (Hash.list h)
  let array h = make (Hash.array h)
  let file = make Hash.file
  let program = make Hash.program
  let set h = make (Hash.set h)
  let marshal x = make Hash.marshal x

  let pair x1 x2 = make (Hash.pair x1 x2)
  let triple x1 x2 x3 = make (Hash.triple x1 x2 x3)
  let quad  x1 x2 x3 x4 = make (Hash.quad x1 x2 x3 x4)
end

(** {2 Time Utils} *)

module Time = struct
  type t = time
  let seconds = float_of_int
  let hours n = float_of_int n *. 3600.
  let minutes n = float_of_int n *. 60.
  let days n = float_of_int n *. hours 24
  let now () = Unix.gettimeofday()
  let (++) = (+.)
end

(** {2 Memoized Functions} *)

type gc_info =
  | Keep
  | KeepUntil of time
  | CanDrop

module GC_info : sig
  type t = gc_info
  val lt : t -> t -> bool
  val to_bencode : t -> Bencode.t
  val of_bencode : Bencode.t -> t or_error
end = struct
  type t = gc_info

  (* a < b? *)
  let lt a b = match a, b with
    | Keep, Keep
    | _, CanDrop -> false
    | _, Keep
    | CanDrop, _ -> true
    | Keep, _ -> false
    | KeepUntil a, KeepUntil b -> a < b

  let to_bencode = function
    | Keep -> B.String "keep"
    | KeepUntil t -> B.List [B.String "keep_until"; B.String (string_of_float t)]
    | CanDrop -> B.String "drop"

  let of_bencode = function
    | B.String "keep" -> Ok Keep
    | B.List [B.String "keep_until"; B.String t] ->
      begin try Ok (KeepUntil (float_of_string t))
        with _ -> Error "expected float"
      end
    | B.String "drop" -> Ok CanDrop
    | b -> errorf "expected lifetime, got `%s`" (Bencode.encode_to_string b)
end

(* lifetime for a cached value *)
type lifetime =
  [ `Keep
  | `KeepFor of time (** Time delta *)
  | `KeepUntil of time (** Absolute deadline *)
  | `CanDrop
  ]

(** {2 On-Disk storage} *)

module Storage = Maki_storage

(** {2 Value Stored on Disk} *)

module On_disk_val = struct
  type 'a t = 'a Codec.t * hash

  let store ?(storage=Storage.get_default()) codec x =
    let s, _ = Codec.encode codec x in
    let k = Sha1.string s |> Sha1.to_hex in
    Storage.set storage k s >>|= fun () ->
    codec, k

  let find ?(storage=Storage.get_default()) (codec,k) =
    Storage.get storage k >>>= fun s ->
    begin match s with
      | None -> errorf "could not find value `%s` on storage" k
      | Some s -> Codec.decode codec s
    end |> Lwt.return

  let get ?(storage=Storage.get_default()) (codec,k) =
    Storage.get storage k >>>= fun s ->
    begin match s with
      | None -> Ok None
      | Some s ->
        let open Res_ in
        Codec.decode codec s >|= fun x -> Some x
    end |> Lwt.return
end

(* map computation_name -> future (serialized) result *)
type memo_table = (string, string or_error Lwt.t) Hashtbl.t

let memo_table_ : memo_table = Hashtbl.create 64

(* structure, stored on disk, representing the result of some
   computation. Dependencies are listed for the GC not to collect them *)
type cache_value = {
  cv_gc_info: gc_info; (* how long should the GC keep this value *)
  cv_key: hash; (* hash of the computation *)
  cv_children: hash list; (* hash of children *)
  cv_data: encoded_value; (* the actual data *)
  cv_tags: string list; (* tags attached to this particular value *)
}

module Cache_val : sig
  type t = cache_value
  val codec : t Codec.t
  val lifetime : t -> lifetime
  val key : t -> hash
  val data : t -> encoded_value
  val children : t -> hash list
  val tags : t -> string list
end = struct
  type t = cache_value

  (* serialize [c] into Bencode *)
  let encode c =
    B.Dict
      [ "gc_info", bencode_of_gc_info c.cv_gc_info;
        "data", BM.mk_str c.cv_data;
        "key", BM.mk_str c.cv_key;
        "tags", BM.mk_list (List.map BM.mk_str c.cv_tags);
      ] |> Bencode.encode_to_string

  (* [s] is a serialized cached value, turn it into a [cache_value] *)
  let decode (s:string) : cache_value or_error =
    let open Res_ in
    BM.decode_bencode s >>= function
    | B.Dict l ->
      BM.assoc "gc_info" l >>= gc_info_of_bencode >>= fun cv_gc_info ->
      BM.assoc "data" l >>= BM.as_str >>= fun cv_data ->
      BM.assoc_or (B.List []) "children" l
        |> BM.as_list >>= map_l BM.as_str >>= fun cv_children ->
      BM.assoc "key" l >>= BM.as_str >>= fun cv_key ->
      BM.assoc_or (B.List []) "tags" l
      |> BM.as_list >>= map_l BM.as_str >>= fun cv_tags ->
      return {cv_data; cv_gc_info; cv_key; cv_children; cv_tags}
    | _ -> Error "expected cache_value"

  let codec = Codec.make1 ~encode ~decode "cache_value"

  let lifetime c = match c.cv_gc_info with
    | Keep  -> `Keep
    | KeepUntil t -> `KeepUntil t
    | CanDrop  -> `CanDrop
  let key c = c.cv_key
  let children c = c.cv_children
  let data c = c.cv_data
  let tags c = c.cv_tags
end

(* compute the hash of the result of computing the application of
   the function named [fun_name] on dependencies [l] *)
let compute_name
    (fun_name:string)
    (args:Arg.t list): string Lwt.t =
  let ctx = Sha1.init() in
  Sha1.update_string ctx fun_name;
  Lwt_list.iter_s
    (fun (Arg.A(h,x)) -> h ctx x)
    args
  >|= fun () ->
  Sha1.finalize ctx |> Sha1.to_hex

(*
   - compute a string [s] out of computation and dependencies [to_string]
   - compute the content's hash (canonical form) of dependencies,
     OR use the cache if timestamp didn't change
   - compute the hash [h] of [s [hash(dep1), ..., [hash depn]]]
   - check if file named [h] exists
     * if it does, read its content, deserialize result with [op] and return it
     * otherwise, compute result, write it in [h], and return it
*)
let call_
    ?(storage=Storage.get_default ())
    ?(lifetime=`CanDrop)
    ?limit
    ?tags:(cv_tags=[])
    ~name:fun_name
    ~args
    ~returning
    f
  =
  (* compute the "name" of the computation *)
  compute_name fun_name args >>= fun computation_name ->
  let key = Sha1.string computation_name |> Sha1.to_hex in
  let cv_gc_info = match lifetime with
    | `CanDrop -> CanDrop
    | `Keep -> Keep
    | `KeepUntil t -> KeepUntil t
    | `KeepFor t ->
      let now = Unix.gettimeofday () in
      KeepUntil (now +. t)
  in
  (* save a value into the storage *)
  let save_cv storage cv =
    let res_serialized, _ = Codec.encode Cache_val.codec cv in
    Maki_log.logf 3
      (fun k->k "save result `%s` into storage %s (gc_info: %s)"
          computation_name (Storage.name storage)
          (bencode_of_gc_info cv.cv_gc_info |> B.encode_to_string));
    Storage.set storage key res_serialized
  in
  (* compute the result of calling the function, or retrieve the result in
     cache. This returns a [('a * string) or_error Lwt.t] where
     the string is the serialization of the ['a] *)
  let compute_memoized () =
    begin match limit with
      | None -> f ()
      | Some l -> Limit.acquire l f
    end
    >>>= fun res ->
    let cv_data, cv_children = Codec.encode returning res in
    let cv = {cv_gc_info; cv_data; cv_children; cv_key=key; cv_tags} in
    save_cv storage cv >>|= fun _ ->
    res, cv
  in
  (* check on-disk cache *)
  let check_cache_or_compute () : _ or_error Lwt.t =
    Storage.get storage key >>>=
    function
    | None ->
      (* not in cache, perform computation (possibly acquiring
         the "limit" first) *)
      Maki_log.logf 3
        (fun k->k "could not find `%s` in storage %s"
            computation_name (Storage.name storage));
      compute_memoized ()
    | Some s ->
      Maki_log.logf 3
        (fun k->k "found result of `%s` in storage %s"
            computation_name (Storage.name storage));
      (* read result from the raw data *)
      let res =
        let open Res_ in
        Codec.decode Cache_val.codec s >>= fun cv ->
        Codec.decode returning cv.cv_data >|= fun res -> res, cv
      in
      begin match res with
        | Error e ->
          Maki_log.logf 3
            (fun k->k "cached file for `%s` is invalid, delete it and recompute: %s"
                computation_name e);
          Storage.remove storage key >>= fun () ->
          compute_memoized ()
        | Ok ((_,cv) as res) ->
          (* if new lifetime extends longer than res.cv_gc_info, refresh *)
          begin
            if gc_info_lt cv.cv_gc_info cv_gc_info
            then (
              let cv = {cv with cv_gc_info} in
              save_cv storage cv
            ) else Lwt.return (Ok ())
          end
          >>|= fun _ ->
          res
      end
  in
  (* check memo table *)
  try
    let future_res = Hashtbl.find memo_table_ key in
    (* ok, some other thread is performing the computation, just wait
       for it to complete and deserialize the result *)
    future_res >|= fun s_or_err ->
    Res_.(s_or_err >>= Codec.decode returning)
  with Not_found ->
    let res_serialized, promise_serialized = Lwt.wait () in
    (* put future result in memo table in case another thread wants to
       compute the same value *)
    Hashtbl.add memo_table_ key res_serialized;
    (* compute result *)
    let res = check_cache_or_compute() in
    (* ensure that when [res] terminates, [res_serialized] is updated,
       and cleanup entry from hashtable to avoid clogging memory *)
    Lwt.on_any res
      (function
        | Ok (_,cv) ->
          Hashtbl.remove memo_table_ key;
          Lwt.wakeup promise_serialized (Ok cv.cv_data)
        | Error e ->
          Hashtbl.remove memo_table_ key;
          Lwt.wakeup promise_serialized (Error e))
      (fun e ->
         Lwt.wakeup promise_serialized
           (errorf "error when computing %s: %s" key (Printexc.to_string e)));
    res >>|= fst

let call
    ?(bypass=false) ?storage ?lifetime ?limit ?tags ~name ~args ~returning f
  =
  if bypass then (
    Lwt.catch
      (fun () ->
         begin match limit with
           | None -> f ()
           | Some l -> Limit.acquire l f
         end)
      (fun e -> Lwt.return (error (Printexc.to_string e)))
  ) else (
    call_ ?storage ?lifetime ?limit ?tags ~name ~args ~returning f
  )

let call_exn ?bypass ?storage ?lifetime ?limit ?tags ~name ~args ~returning f =
  call ?bypass ?storage ?lifetime ?limit ?tags ~name ~args ~returning f
  >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail (Failure e)

(** {2 GC} *)

type gc_stats = {
  gc_kept: int;
  gc_removed: int;
}

let string_of_gc_stats s =
  Printf.sprintf "kept %d entries, removed %d entries" s.gc_kept s.gc_removed

type gc_cell = {
  gc_children: hash list;
  mutable gc_status: [`Root | `Alive | `Dead];
}

type gc_state = (string, gc_cell) Hashtbl.t

(* find the set of roots, collect the graph in RAM *)
let gc_collect_roots now s : gc_state or_error Lwt.t =
  Maki_log.log 3 "gc: collecting roots...";
  let state = Hashtbl.create 256 in
  Storage.fold s ~x:()
    ~f:(fun () (key, value) ->
      (* decide whether to add [key] to [set], so it remains alive, or now *)
      (Codec.decode Cache_val.codec value |> Lwt.return) >>|= fun cv ->
      (* remember dependencies of [key] *)
      let gc_status = match cv.cv_gc_info with
        | Keep -> `Root
        | KeepUntil t -> if t >= now then `Root else `Dead
        | CanDrop -> `Dead
      in
      Hashtbl.add state key {gc_children=cv.cv_children; gc_status};
      ())
  >>|= fun () ->
  Maki_log.logf 3
    (fun k->k "root collection is done (%d entries)" (Hashtbl.length state));
  state

let gc_storage s =
  let now = Unix.gettimeofday () in
  gc_collect_roots now s >>>= fun st ->
  (* actually collect dead cells *)
  let n_kept = ref 0 in
  let n_removed = ref 0 in
  let err = ref None in
  Maki_log.log 3 "start collection of dead values";
  Hashtbl.iter
    (fun k c ->
       match c.gc_status with
         | `Alive | `Root ->
           Maki_log.logf 5 (fun f->f "gc: keep value %s" k);
           incr n_kept
         | `Dead ->
           Maki_log.logf 5 (fun f->f "gc: remove value %s" k);
           Lwt.async
             (fun () ->
                Lwt.catch
                  (fun () ->
                     incr n_removed;
                     Storage.remove s k)
                  (fun e ->
                     err :=
                       Some (errorf "error when removing `%s`: %s"
                           k (Printexc.to_string e));
                     Lwt.return_unit)))
    st;
  begin match !err with
    | Some e -> Lwt.return e
    | None ->
      let stats = { gc_kept= !n_kept; gc_removed= !n_removed } in
      Lwt.return (Ok stats)
  end
