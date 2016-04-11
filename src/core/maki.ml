
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

open Result
open Lwt.Infix

module B = Bencode
module BM = Maki_bencode

module LwtErr = Maki_lwt_err

let (>>>=) = LwtErr.(>>=)
let (>>|=) = LwtErr.(>|=)

type 'a or_error = ('a, exn) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t

module Res_ = struct
  let return x = Ok x
  let (<*>) f x = match f, x with
    | Ok f, Ok x -> Ok (f x)
    | Error e, _
    | _, Error e -> Error e
  let (>|=) r f = match r with
    | Ok x -> Ok (f x)
    | Error e -> Error e
  let (>>=) r f = match r with
    | Ok x -> f x
    | Error e -> Error e

  let map_l f l =
    try
      let res =
        List.map
        (fun x -> match f x with Ok y -> y | Error e -> raise e)
        l
      in
      Ok res
    with e -> Error e
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

(** {2 Utils} *)

let last_time_ f =
  let s = Unix.stat f in
  s.Unix.st_mtime

(* number of threads to use in parallel for computing Sha1 *)
let sha1_pool_ = Limit.create 6

(* fast sha1 on a file *)
let sha1 f =
  Limit.acquire sha1_pool_
    (fun () -> Lwt_preemptive.detach Sha1.file_fast f)

let abspath f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let sha1_of_string s = Sha1.string s

let last_mtime f : time or_error =
  try Ok (last_time_ f)
  with e -> Error e

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
       errcode >>= fun (Unix.WEXITED c | Unix.WSIGNALED c | Unix.WSTOPPED c) ->
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
  fs_hash: string; (* SHA1 in hex form *)
}

let fs_of_bencode = function
  | B.Dict l ->
    let open Res_ in
    BM.assoc "path" l >>= BM.as_str >>= fun fs_path ->
    BM.assoc "hash" l >>= BM.as_str >>= fun fs_hash ->
    Ok {fs_path; fs_hash}
  | b -> BM.expected_b "file_state" b

let bencode_of_fs fs =
  B.Dict
    [ "path", B.String fs.fs_path
    ; "hash", B.String fs.fs_hash
    ]

(* cache for files: maps file name to hash + last modif *)
type file_cache = (string, time * file_state) Hashtbl.t

let file_cache_ : file_cache = Hashtbl.create 128

let compute_file_state_ f =
  Lwt.catch
    (fun () ->
      let fs =
        try
          let time, fs = Hashtbl.find file_cache_ f in
          let time' = last_time_ f in
          ( if time' <> time
            then sha1 f >|= Sha1.to_hex
            else Lwt.return fs.fs_hash )
          >|= fun hash ->
          Ok (time', {fs with fs_hash=hash})
        with Not_found ->
          let time = last_time_ f in
          sha1 f >|= Sha1.to_hex >|= fun fs_hash ->
          Ok (time, { fs_path=f; fs_hash})
      in
      Lwt.on_success fs
        (function
          | Ok x -> Hashtbl.replace file_cache_ f x
          | Error _ -> ());
      fs)
    (fun e -> Lwt.return (Error e))

module Value = struct
  type 'a ops = {
    descr: string; (* description of 'a *)
    serialize :
      [`Fast of ('a -> Bencode.t)
      | `Slow of ('a -> Bencode.t Lwt.t)
      ];
    unserialize : Bencode.t -> 'a or_error;
  }

  type t = Value : 'a ops * 'a -> t
  (** Universal type, can be hashed, serialized, etc. *)

  let make ~serialize ~unserialize descr =
    { descr; serialize; unserialize; }

  let make_fast ~serialize ~unserialize descr =
    make ~serialize:(`Fast serialize) ~unserialize descr

  let make_slow ~serialize ~unserialize descr =
    make ~serialize:(`Slow serialize) ~unserialize descr

  let int =
    make_fast "int"
      ~serialize:(fun i -> B.Integer i)
      ~unserialize:(function
        | B.Integer i -> Ok i
        | B.String s ->
          begin
            try Ok (int_of_string s)
            with _ -> BM.expected_s "int" s
          end;
        | b -> BM.expected_b "int" b)

  let string =
    make_fast "string"
      ~serialize:(fun s -> Bencode.String s)
      ~unserialize:(function
          | B.String s -> Ok s
          | b -> BM.expected_b "string" b)

  let bool =
    make_fast "bool"
      ~serialize:(fun b -> if b then B.Integer 1 else B.Integer 0)
      ~unserialize:(function
            | B.Integer 0 -> Ok false
            | B.Integer n when n<>0 -> Ok true
            | B.String s ->
              begin try Ok (bool_of_string s)
                with _ -> BM.expected_s "bool" s
              end
            | b -> BM.expected_b "bool" b)

  (* special behavior for files: comparison is done be by timestamp+hash *)
  let file =
    (* serialize:
       look into cache for the file_state, compare timestamp,
       and compute hash if timestamp changed or file is not in cache *)
    make_slow "file"
      ~serialize:(fun f ->
        let f = abspath f in
        compute_file_state_ f
        >>= Maki_lwt_err.unwrap_res
        >|= fun (_, fs) -> bencode_of_fs fs)
      ~unserialize:(fun b ->
          let open Res_ in
          fs_of_bencode b >|= fun fs -> fs.fs_path)

  (* turn [f], a potentially relative path to a program, into an absolute path *)
  let find_program_path_ f =
    if Filename.is_relative f && not (Filename.is_implicit f)
    then
      shellf "which '%s'" f >>= fun (out,_,errcode) ->
      if errcode=0
      then Lwt.return (Ok (String.trim out))
      else Lwt.return (Error Not_found)
    else Lwt.return (Ok f)

  (* special behavior for programs: find the path to the binary,
     then behave like a file *)
  let program =
    make_slow "program"
      ~serialize:(fun f ->
          find_program_path_ f >>= function
          | Error e -> Lwt.fail e
          | Ok p ->
            compute_file_state_ p
            >>= Maki_lwt_err.unwrap_res
            >|= fun (_,fs) -> bencode_of_fs fs)
      ~unserialize:(fun b ->
          let open Res_ in
          fs_of_bencode b >|= fun fs -> fs.fs_path)

  let marshal name =
    let flags = [Marshal.Closures; Marshal.No_sharing; Marshal.Compat_32] in
    make_fast name
      ~serialize:(fun x -> BM.mk_str (Marshal.to_string x flags))
      ~unserialize:(function
          | B.String s -> Marshal.from_string s 0
          | b -> BM.expected_b "string" b)

  let list op =
    let descr = Printf.sprintf "(list %s)" op.descr in
    let serialize = match op.serialize with
      | `Fast f -> `Fast (fun l -> B.List (List.map f l))
      | `Slow f -> `Slow (fun l -> Lwt_list.map_p f l >|= BM.mk_list)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.List l -> Res_.map_l op.unserialize l
          | b -> BM.expected_b descr b)

  let set op =
    let descr = Printf.sprintf "(set %s)" op.descr in
    (* sort by ordering on Bencode, which is, Pervasives.compare *)
    let mk_list_sort l =
      List.sort Pervasives.compare l |> BM.mk_list
    in
    let serialize = match op.serialize with
      | `Fast f -> `Fast (fun l -> List.map f l |> mk_list_sort)
      | `Slow f -> `Slow (fun l -> Lwt_list.map_p f l >|= mk_list_sort)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.List l -> Res_.map_l op.unserialize l
          | b -> BM.expected_b descr b)

  let assoc op =
    let descr = Printf.sprintf "(assoc %s)" op.descr in
    let serialize = match op.serialize with
      | `Fast f ->
        `Fast
          (fun l -> B.Dict (List.map (fun (name,x) -> name, f x) l))
      | `Slow f ->
        `Slow
          (fun l ->
             Lwt_list.map_p (fun (name,x) -> f x >|= fun x' -> name,x') l
             >|= BM.mk_dict)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.Dict l ->
            Res_.map_l
              (fun (name,x) -> Res_.(op.unserialize x >|= fun x -> name, x))
              l
          | b -> BM.expected_b descr b)

  let map ?descr f g op =
    let descr = match descr with Some d -> d | None -> op.descr in
    let serialize = match op.serialize with
      | `Fast s -> `Fast (fun x -> s (f x))
      | `Slow s -> `Slow (fun x -> s (f x))
    in
    { descr;
      serialize;
      unserialize=(fun b -> Res_.(op.unserialize b >|= g));
    }

  let serialize op x = match op.serialize with
    | `Fast f -> Lwt.return (f x)
    | `Slow f -> f x

  let unserialize op x = op.unserialize x

  let to_string op x =
    serialize op x >|= B.encode_to_string

  let of_string op s =
    let open Res_ in
    BM.decode_bencode s >>= unserialize op

  let mk_pair a b = a,b
  let mk_triple a b c = a,b,c
  let mk_quad a b c d = a,b,c,d

  let pair a b =
    let descr = Printf.sprintf "(pair %s %s)" a.descr b.descr in
    let serialize = match a.serialize, b.serialize with
      | `Fast a, `Fast b ->
        `Fast (fun (x,y) -> B.List [a x; b y])
      | _ ->
        `Slow (fun (x,y) -> serialize a x
          >>= fun x -> serialize b y >|= BM.mk_pair x)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y] ->
            Res_.(return mk_pair <*> a.unserialize x <*> b.unserialize y)
        | b -> BM.expected_b descr b)

  let triple a b c =
    let descr = Printf.sprintf "(triple %s %s %s)" a.descr b.descr c.descr in
    let serialize = match a.serialize, b.serialize, c.serialize with
      | `Fast a, `Fast b, `Fast c ->
        `Fast (fun (x,y,z) -> B.List [a x; b y; c z])
      | _ ->
        `Slow (fun (x,y,z) ->
            serialize a x >>= fun x ->
            serialize b y >>= fun y ->
            serialize c z >|= BM.mk_triple x y)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y;z] ->
            Res_.(return mk_triple <*> a.unserialize x
                  <*> b.unserialize y <*> c.unserialize z)
        | b -> BM.expected_b descr b)

  let quad a b c d =
    let descr =
      Printf.sprintf "(quad %s %s %s %s)" a.descr b.descr c.descr d.descr in
    let serialize = match a.serialize, b.serialize, c.serialize, d.serialize with
      | `Fast a, `Fast b, `Fast c, `Fast d ->
        `Fast (fun (x,y,z,u) -> B.List [a x; b y; c z; d u])
      | _ ->
        `Slow (fun (x,y,z,u) ->
            serialize a x >>= fun x ->
            serialize b y >>= fun y ->
            serialize c z >>= fun z ->
            serialize d u >|= BM.mk_quad x y z)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y;z;u] ->
            Res_.(return mk_quad <*> a.unserialize x
                  <*> b.unserialize y <*> c.unserialize z <*> d.unserialize u)
        | b -> BM.expected_b descr b)

  let pack op x = Value (op,x)
  let pack_int = pack int
  let pack_bool = pack bool
  let pack_string = pack string
  let pack_file = pack file
  let pack_program = pack program
  let pack_list op = pack (list op)
  let pack_set op = pack (set op)
  let pack_assoc op = pack (assoc op)

  let argv0 = pack program Sys.argv.(0)

  let to_string_packed (Value (op,x)) = to_string op x
end

(** {2 On-Disk storage} *)
module Storage = Maki_storage

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

let bencode_of_gc_info = function
  | Keep -> B.String "keep"
  | KeepUntil t -> B.List [B.String "keep_until"; B.String (string_of_float t)]
  | CanDrop -> B.String "drop"

let gc_info_of_bencode = function
  | B.String "keep" -> Ok Keep
  | B.List [B.String "keep_until"; B.String t] ->
    begin try Ok (KeepUntil (float_of_string t))
      with _ -> Error (Maki_bencode.Maki_error "expected float")
    end
  | B.String "drop" -> Ok CanDrop
  | _ -> Error (Maki_bencode.Maki_error "expected lifetime")

(* lifetime for a cached value *)
type lifetime =
  [ `Keep
  | `KeepFor of time (** Time delta *)
  | `KeepUntil of time (** Absolute deadline *)
  | `CanDrop
  ]

(* map computation_name -> future (serialized) result *)
type memo_table = (string, string or_error Lwt.t) Hashtbl.t

let memo_table_ : memo_table = Hashtbl.create 64

(* structure, stored on disk, representing the result of some
   computation. Dependencies are listed for the GC not to collect them *)
type cache_value = {
  cv_gc_info: gc_info; (* how long should the GC keep this value *)
  cv_fun_name: string; (* computed from what? *)
  cv_deps: string list; (* dependencies used to compute this value *)
  cv_data: string; (* the actual data *)
  cv_tags: string list; (* tags attached to this particular value *)
}

(* serialize [c] into Bencode *)
let bencode_of_cache_value c =
  B.Dict
    [ "gc_info", bencode_of_gc_info c.cv_gc_info
    ; "deps", BM.mk_list (List.map BM.mk_str c.cv_deps)
    ; "data", BM.mk_str c.cv_data
    ; "name", BM.mk_str c.cv_fun_name
    ; "tags", BM.mk_list (List.map BM.mk_str c.cv_tags)
    ]

(* [s] is a serialized cached value, turn it into a [cache_value] *)
let cache_value_of_bencode b : cache_value or_error =
  let open Res_ in
  match b with
    | B.Dict l ->
      BM.assoc "gc_info" l >>= gc_info_of_bencode >>= fun cv_gc_info ->
      BM.assoc "deps" l >>= BM.as_list >>= map_l BM.as_str >>= fun cv_deps ->
      BM.assoc "data" l >>= BM.as_str >>= fun cv_data ->
      BM.assoc "name" l >>= BM.as_str >>= fun cv_fun_name ->
      BM.assoc_or (B.List []) "tags" l |> BM.as_list >>= map_l BM.as_str >>= fun cv_tags ->
      return {cv_data; cv_deps; cv_gc_info; cv_fun_name; cv_tags}
    | _ -> Error (BM.Maki_error "expected cache_value")

let cache_value_lifetime c =
  match c.cv_gc_info with
    | Keep  -> `Keep
    | KeepUntil t -> `KeepUntil t
    | CanDrop  -> `CanDrop
let cache_value_fun_name c = c.cv_fun_name
let cache_value_deps c = c.cv_deps
let cache_value_data c = c.cv_data
let cache_value_tags c = c.cv_tags

(* compute the hash of the result of computing the application of
   the function named [fun_name] on dependencies [l] *)
let compute_name fun_name l =
  Lwt_list.map_p
    (fun (Value.Value (op,x)) -> Value.to_string op x >|= BM.mk_str)
    l
  >|= fun l ->
  let b = B.List (B.String fun_name :: l) in
  B.encode_to_string b

(* TODO: generalize this, put the operation "validate" into
   the {!Value.ops} type *)
(* check if [f] is a [file_state] that doesn't correspond to the actual
   content of the disk *)
let is_invalid_file_ref f =
  let fs =
    let open Res_ in
    BM.decode_bencode f >>=
    fs_of_bencode
  in
  match fs with
  | Error _ -> Lwt.return_false (* not a file *)
  | Ok fs ->
    (* compare [fs] with actual current file state *)
    compute_file_state_ fs.fs_path
    >|= function
    | Error _ -> true  (* file not present, etc. *)
    | Ok (_,fs') -> fs'.fs_hash <> fs.fs_hash

(*
   - compute a string [s] out of computation and dependencies [to_string]
   - compute the content's hash (canonical form) of dependencies,
     OR use the cache if timestamp didn't change
   - compute the hash [h] of [s [hash(dep1), ..., [hash depn]]]
   - check if file named [h] exists
     * if it does, read its content, deserialize result with [op] and return it
     * otherwise, compute result, write it in [h], and return it
*)
let call
?(storage=Storage.get_default ())
?(lifetime=`CanDrop)
?limit
?tags:(cv_tags=[])
~name:fun_name
~deps
~op
f
  =
  (* compute the "name" of the computation *)
  compute_name fun_name deps >>= fun computation_name ->
  let h_computation_name = Sha1.string computation_name |> Sha1.to_hex in
  (* compute the result of calling the function, or retrieve the result in
     cache. This returns a [('a * string) or_error Lwt.t] where
     the string is the serialization of the ['a] *)
  let compute_memoized () =
    begin match limit with
      | None -> f ()
      | Some l -> Limit.acquire l f
    end >>= fun res ->
    Value.to_string op res >>= fun cv_data ->
    Lwt_list.map_p Value.to_string_packed deps >>= fun cv_deps ->
    let cv_gc_info = match lifetime with
      | `CanDrop -> CanDrop
      | `Keep -> Keep
      | `KeepUntil t -> KeepUntil t
      | `KeepFor t ->
        let now = Unix.gettimeofday () in
        KeepUntil (now +. t)
    in
    let cv = {cv_gc_info; cv_deps; cv_data; cv_fun_name=fun_name; cv_tags} in
    let res_serialized = bencode_of_cache_value cv |> B.encode_to_string in
    Maki_log.logf 3
      (fun k->k "save result `%s` into storage %s (gc_info: %s)"
          computation_name (Storage.name storage)
          (bencode_of_gc_info cv_gc_info |> B.encode_to_string));
    Storage.set storage h_computation_name res_serialized
    >>>= fun () ->
    Lwt.return (Ok (res, cv_data))
  in
  (* check on-disk cache *)
  let check_cache_or_compute () =
    Storage.get storage h_computation_name >>>=
    function
    | None ->
      (* not in cache, perform computation (possibly acquiring
         the "limit" first) *)
      Maki_log.logf 3
        (fun k->k "could not find `%s` in storage %s"
            computation_name (Storage.name storage));
      compute_memoized ()
    | Some s ->
      (* TODO: if cv.cv_gc_info = KeepUntil, refresh timestamp *)
      Maki_log.logf 3
        (fun k->k "found result of `%s` in storage %s"
            computation_name (Storage.name storage));
      (* read result from the raw data *)
      let res =
        let open Res_ in
        BM.decode_bencode s >>= fun b ->
        cache_value_of_bencode b >>= fun cv ->
        Value.of_string op cv.cv_data >|= fun res -> res, cv.cv_data
      in
      let fut_res = Lwt.return res in
      fut_res >>>= fun (_,cv_data) ->
      (* if result is a file, check that file exists, and that
         it's the same as expected. Otherwise recompute (calling
         compute_memoized recursively) *)
      is_invalid_file_ref cv_data >>= fun invalid ->
      if invalid
      then (
        Maki_log.logf 3
          (fun k->k "cached file %s is invalid, recompute" cv_data);
        compute_memoized ()
      ) else fut_res
  in
  (* check memo table *)
  try
    let future_res = Hashtbl.find memo_table_ h_computation_name in
    (* ok, some other thread is performing the computation, just wait
       for it to complete and deserialize the result *)
    future_res >|= fun s_or_err ->
    Res_.(s_or_err >>= Value.of_string op)
  with Not_found ->
    let res_serialized, promise_serialized = Lwt.wait () in
    (* put future result in memo table in case another thread wants to
       compute the same value *)
    Hashtbl.add memo_table_ h_computation_name res_serialized;
    (* compute result *)
    let res = check_cache_or_compute () in
    (* ensure that when [res] terminates, [res_serialized] is updated *)
    Lwt.on_any res
      (function
        | Ok (_,str) -> Lwt.wakeup promise_serialized (Ok str)
        | Error e -> Lwt.wakeup promise_serialized (Error e))
      (fun e -> Lwt.wakeup promise_serialized (Error e));
    res >>|= fst

let call_exn ?storage ?lifetime ?limit ?tags ~name ~deps ~op f =
  call ?storage ?lifetime ?limit ?tags ~name ~deps ~op f
  >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail e

(** {2 GC} *)

type gc_stats = {
  gc_kept: int;
  gc_removed: int;
}

let string_of_gc_stats s =
  Printf.sprintf "kept %d entries, removed %d entries" s.gc_kept s.gc_removed

type gc_cell = {
  gc_deps: string list;
  gc_path: path option; (* if file *)
  mutable gc_status: [`Root | `Alive | `Dead];
}

type gc_state = (string, gc_cell) Hashtbl.t

(* find the set of roots, collect the graph in RAM *)
let gc_collect_roots now s : gc_state Lwt.t =
  Maki_log.log 3 "gc: collecting roots...";
  let state = Hashtbl.create 256 in
  Storage.fold s ~x:()
    ~f:(fun () (key, value) ->
        (* decide whether to add [key] to [set], so it remains alive, or now *)
        let map_or_err =
          let open Res_ in
          BM.decode_bencode value >>=
          cache_value_of_bencode >|= fun cv ->
          (* might be a file path *)
          let gc_path =match Value.of_string Value.file cv.cv_data with
            | Ok f -> Some f
            | Error _ -> None
          in
          (* remember dependencies of [key] *)
          let gc_status = match cv.cv_gc_info with
            | Keep -> `Root
            | KeepUntil t -> if t >= now then `Root else `Dead
            | CanDrop -> `Root
          in
          Hashtbl.add state key {gc_deps=cv.cv_deps; gc_status; gc_path};
        in
        match map_or_err with
          | Error e -> Lwt.fail e
          | Ok () -> Lwt.return_unit
     )
  >|= fun () ->
  Maki_log.logf 3
    (fun k->k "root collection is done (%d entries)" (Hashtbl.length state));
  state

let gc_storage ?(remove_file=false) s =
  let now = Unix.gettimeofday () in
  gc_collect_roots now s >>= fun st ->
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
                     (* if [c] is a file and [remove_file=true], remove it *)
                     begin match c.gc_path with
                       | Some f when remove_file -> Sys.remove f
                       | _ -> ()
                     end;
                     incr n_removed;
                     Storage.remove s k
                  )
                  (fun e -> err := Some e; Lwt.return_unit)))
    st;
  match !err with
    | Some e -> Lwt.return (Error e)
    | None ->
      let stats = { gc_kept= !n_kept; gc_removed= !n_removed } in
      Lwt.return (Ok stats)
