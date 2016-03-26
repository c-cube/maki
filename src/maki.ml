
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

open Result
open Lwt.Infix

module B = Bencode

module LwtErr = Maki_lwt_err

let (>>>=) = LwtErr.(>>=)

type 'a or_error = ('a, exn) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t
type ('a,'rw) pipe = ('a,'rw) Maki_pipe.t

exception Maki_error of string

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

let decode_bencode s =
  try Ok (B.decode (`String s))
  with _ -> Error (Maki_error (s ^ " is not valid Bencode"))

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

let assoc_ k l =
  try Ok (List.assoc k l)
  with e -> Error e

let as_str_ = function
  | B.String s -> Ok s
  | _ -> Error (Maki_error "expected string")

let as_float_ = function
  | B.String s ->
    begin try Ok (float_of_string s) with e -> Error e end
  | _ -> Error (Maki_error "expected string")

let as_list_ = function
  | B.List l -> Ok l
  | _ -> Error (Maki_error "expected list")

let b_str s = B.String s
let b_list l = B.List l
let b_dict l = B.Dict l
let b_pair x y = B.List [x; y]
let b_triple x y z = B.List [x;y;z]
let b_quad x y z u = B.List [x;y;z;u]

let expected_s what s =
  Error (Maki_error (Printf.sprintf "expected %s, got %s" what s))

let expected_b what b = expected_s what (B.encode_to_string b)

(** {2 Values} *)

type file_state = {
  fs_path: path;
  fs_time: time;
  fs_hash: string; (* SHA1 in hex form *)
}

let fs_of_bencode = function
  | B.Dict l ->
    let open Res_ in
    assoc_ "path" l >>= as_str_ >>= fun fs_path ->
    assoc_ "time" l >>= as_float_ >>= fun fs_time ->
    assoc_ "hash" l >>= as_str_ >>= fun fs_hash ->
    Ok {fs_path; fs_time; fs_hash}
  | b -> expected_b "file_state" b

let bencode_of_fs fs =
  B.Dict
    [ "path", B.String fs.fs_path
    ; "time", B.String (string_of_float fs.fs_time)
    ; "hash", B.String fs.fs_hash
    ]

(* cache for files: maps file name to hash + last modif *)
type file_cache = (string, file_state) Hashtbl.t

let file_cache_ : file_cache = Hashtbl.create 128

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
            with _ -> expected_s "int" s
          end;
        | b -> expected_b "int" b)

  let string =
    make_fast "string"
      ~serialize:(fun s -> Bencode.String s)
      ~unserialize:(function
          | B.String s -> Ok s
          | b -> expected_b "string" b)

  let bool =
    make_fast "bool"
      ~serialize:(fun b -> if b then B.Integer 1 else B.Integer 0)
      ~unserialize:(function
            | B.Integer 0 -> Ok false
            | B.Integer n when n<>0 -> Ok true
            | B.String s ->
              begin try Ok (bool_of_string s)
                with _ -> expected_s "bool" s
              end
            | b -> expected_b "bool" b)

  (* special behavior for files: comparison is done be by timestamp+hash *)
  let file =
    (* serialize:
       look into cache for the file_state, compare timestamp,
       and compute hash if timestamp changed or file is not in cache *)
    let serialize f =
      begin try
        let fs = Hashtbl.find file_cache_ f in
        let time' = last_time_ f in
        ( if time' <> fs.fs_time
          then sha1 f >|= Sha1.to_hex
          else Lwt.return fs.fs_hash )
        >|= fun hash ->
        {fs with fs_time=time'; fs_hash=hash}
      with Not_found ->
        let fs_time = last_time_ f in
        sha1 f >|= Sha1.to_hex >|= fun fs_hash ->
        { fs_path=f; fs_time; fs_hash}
      end >>= fun fs ->
      Hashtbl.replace file_cache_ f fs;
      Lwt.return (bencode_of_fs fs)
    in
    make_slow "file"
      ~serialize
      ~unserialize:(fun b ->
          let open Res_ in
          fs_of_bencode b >|= fun fs -> fs.fs_path)

  let list op =
    let descr = Printf.sprintf "(list %s)" op.descr in
    let serialize = match op.serialize with
      | `Fast f -> `Fast (fun l -> B.List (List.map f l))
      | `Slow f -> `Slow (fun l -> Lwt_list.map_p f l >|= b_list)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.List l -> Res_.map_l op.unserialize l
          | b -> expected_b descr b)

  let set op =
    let descr = Printf.sprintf "(set %s)" op.descr in
    (* sort by ordering on Bencode, which is, Pervasives.compare *)
    let b_list_sort l =
      List.sort Pervasives.compare l |> b_list
    in
    let serialize = match op.serialize with
      | `Fast f -> `Fast (fun l -> List.map f l |> b_list_sort)
      | `Slow f -> `Slow (fun l -> Lwt_list.map_p f l >|= b_list_sort)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.List l -> Res_.map_l op.unserialize l
          | b -> expected_b descr b)

  let assoc op =
    let descr = Printf.sprintf "(assoc %s)" op.descr in
    let serialize = match op.serialize with
      | `Fast f ->
        `Fast
          (fun l -> B.Dict (List.map (fun (name,x) -> name, f x) l))
      | `Slow f ->
        `Slow
          (fun l -> Lwt_list.map_p (fun (name,x) -> f x >|= fun x' -> name,x') l >|= b_dict)
    in
    make descr
      ~serialize
      ~unserialize:(function
          | B.Dict l ->
            Res_.map_l
              (fun (name,x) -> Res_.(op.unserialize x >|= fun x -> name, x))
              l
          | b -> expected_b descr b)

  let serialize op x = match op.serialize with
    | `Fast f -> Lwt.return (f x)
    | `Slow f -> f x

  let unserialize op x = op.unserialize x

  let to_string op x =
    serialize op x >|= B.encode_to_string

  let of_string op s =
    let open Res_ in
    decode_bencode s >>= unserialize op

  let mk_pair a b = a,b
  let mk_triple a b c = a,b,c
  let mk_quad a b c d = a,b,c,d

  let pair a b =
    let descr = Printf.sprintf "(pair %s %s)" a.descr b.descr in
    let serialize = match a.serialize, b.serialize with
      | `Fast a, `Fast b ->
        `Fast (fun (x,y) -> B.List [a x; b y])
      | _ ->
        `Slow (fun (x,y) -> serialize a x >>= fun x -> serialize b y >|= b_pair x)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y] ->
            Res_.(return mk_pair <*> a.unserialize x <*> b.unserialize y)
        | b -> expected_b descr b)

  let triple a b c =
    let descr = Printf.sprintf "(triple %s %s %s)" a.descr b.descr c.descr in
    let serialize = match a.serialize, b.serialize, c.serialize with
      | `Fast a, `Fast b, `Fast c ->
        `Fast (fun (x,y,z) -> B.List [a x; b y; c z])
      | _ ->
        `Slow (fun (x,y,z) ->
            serialize a x >>= fun x ->
            serialize b y >>= fun y ->
            serialize c z >|= b_triple x y)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y;z] ->
            Res_.(return mk_triple <*> a.unserialize x
                  <*> b.unserialize y <*> c.unserialize z)
        | b -> expected_b descr b)

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
            serialize d u >|= b_quad x y z)
    in
    make descr
      ~serialize
      ~unserialize:(function
        | B.List [x;y;z;u] ->
            Res_.(return mk_quad <*> a.unserialize x
                  <*> b.unserialize y <*> c.unserialize z <*> d.unserialize u)
        | b -> expected_b descr b)

  let pack op x = Value (op,x)
  let pack_int = pack int
  let pack_bool = pack bool
  let pack_string = pack string
  let pack_file = pack file
  let pack_list op = pack (list op)
  let pack_set op = pack (set op)
  let pack_assoc op = pack (assoc op)

  let to_string_packed (Value (op,x)) = to_string op x
end

(** {2 On-Disk storage} *)
module Storage = Maki_storage

(** {2 Memoized Functions} *)

(* TODO: some state mapping computation_name -> 'a lwt,
   for avoiding race conditions where several threads would compute a value
   in parallel (instead, first one to start puts its own future in table)
   *)

type lifetime =
  | Keep
  | KeepUntil of time
  | CanDrop

let bencode_of_lifetime = function
  | Keep -> B.String "keep"
  | KeepUntil t -> B.List [B.String "keep_until"; B.String (string_of_float t)]
  | CanDrop -> B.String "drop"

let lifetime_of_bencode = function
  | B.String "keep" -> Ok Keep
  | B.List [B.String "keep_until"; B.String t] ->
    begin try Ok (KeepUntil (float_of_string t))
      with _ -> Error (Maki_error "expected float")
    end
  | B.String "drop" -> Ok CanDrop
  | _ -> Error (Maki_error "expected lifetime")

(* map computation_name -> future (serialized) result *)
type memo_table = (string, string Lwt.t) Hashtbl.t

let memo_table_ : memo_table = Hashtbl.create 64

(* structure, stored on disk, representing the result of some
   computation. Dependencies are listed for the GC not to collect them *)
type cache_value = {
  cv_lifetime: lifetime; (* how long should the GC keep this value *)
  cv_deps: string list; (* dependencies used to compute this value *)
  cv_data: string; (* the actual data *)
}

(* serialize [c] into Bencode *)
let bencode_of_cache_value c =
  B.Dict
    [ "lifetime", bencode_of_lifetime c.cv_lifetime
    ; "deps", b_list (List.map b_str c.cv_deps)
    ; "data", b_str c.cv_data
    ]

(* [s] is a serialized cached value, turn it into a [cache_value] *)
let cache_value_of_string s : cache_value or_error =
  let open Res_ in
  decode_bencode s >>= fun b ->
  match b with
    | B.Dict l ->
      let open Res_ in
      assoc_ "lifetime" l >>= lifetime_of_bencode >>= fun cv_lifetime ->
      assoc_ "deps" l >>= as_list_ >>= map_l as_str_ >>= fun cv_deps ->
      assoc_ "data" l >>= as_str_ >>= fun cv_data ->
      return {cv_data; cv_deps; cv_lifetime}
    | _ -> Error (Maki_error "expected cache_value")

(* compute the hash of the result of computing the application of
   the function named [fun_name] on dependencies [l] *)
let compute_name fun_name l =
  Lwt_list.map_p
    (fun (Value.Value (op,x)) -> Value.to_string op x >|= b_str)
    l
  >|= fun l ->
  let b = B.List (B.String fun_name :: l) in
  B.encode_to_string b

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
?(lifetime=CanDrop)
?limit
~name:fun_name
~deps
~op
f
  =
  (* compute the "name" of the computation *)
  compute_name fun_name deps >>= fun computation_name ->
  let h_computation_name = Sha1.string computation_name |> Sha1.to_hex in
  let result_of_string s =
    let open Res_ in
    cache_value_of_string s >>= fun cv ->
    (* TODO: if cv.cv_lifetime = KeepUntil, refresh timestamp *)
    Value.of_string op cv.cv_data
  in
  let do_job promise_serialized () =
    (* FIXME: promise_serialized might never wakeup if this thread fails *)
    (* check on-disk cache *)
    Storage.get storage h_computation_name >>>=
    function
    | None ->
      (* not in cache, perform computation (possibly acquiring the "limit" first *)
      begin match limit with
        | None -> f ()
        | Some l -> Limit.acquire l f
      end >>= fun res ->
      Value.to_string op res >>= fun cv_data ->
      Lwt_list.map_p Value.to_string_packed deps >>= fun cv_deps ->
      let cv = {cv_lifetime=lifetime; cv_deps; cv_data} in
      let res_serialized = bencode_of_cache_value cv |> B.encode_to_string in
      Lwt.wakeup promise_serialized res_serialized;
      Storage.set storage h_computation_name res_serialized
      >>>= fun () ->
      Lwt.return (Ok res)
    | Some s ->
      Lwt.wakeup promise_serialized s;
      Lwt.return (result_of_string s)
  in
  (* check memo table *)
  try
    let future_res = Hashtbl.find memo_table_ h_computation_name in
    (* ok, some other thread is performing the computation, just wait
       for it to complete and deserialize the result *)
    future_res >|= fun s ->
    result_of_string s
  with Not_found ->
    let res_serialized, promise_serialized = Lwt.wait () in
    (* put future result in memo table in case another thread wants to
       compute the same value *)
    Hashtbl.add memo_table_ h_computation_name res_serialized;
    Lwt.on_termination res_serialized
      (fun () -> Hashtbl.remove memo_table_ h_computation_name);
    (* compute result *)
    let res = do_job promise_serialized () in
    res

let call_exn ?storage ?lifetime ?limit ~name ~deps ~op f =
  call ?storage ?lifetime ?limit ~name ~deps ~op f
  >>= function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail e
