
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

open Lwt.Infix

module Util = Maki_utils
module Log = Maki_log

module Sha = Digestif.SHA1

module B = Bencode
module BM = Maki_bencode
module Ca = Maki_utils.Cache
module E = Maki_lwt_err

let (>>>=) = E.(>>=)
let (>>|=) = E.(>|=)

type 'a or_error = ('a, string) result
type 'a lwt_or_error = 'a or_error Lwt.t
type 'a printer = Format.formatter -> 'a -> unit

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

  let map_l lim f l : _ list Lwt.t =
    Lwt_list.map_p
      (fun x -> acquire lim (fun () -> f x))
      l
end

(** {2 Basic types} *)

type path = string
type program = string
type time = float
type hash = string
type encoded_value = string

(** {2 Utils} *)

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
       Maki_log.logf 5 (fun k->k "compute sha1 of `%s`" f);
       Lwt_preemptive.detach (fun () -> Sha.file_fast f |> Sha.to_hex) ())

let sha1 f =
  Lwt.catch
    (fun () -> sha1_exn f >|= Res_.return)
    (fun e ->
       errorf "error when computing sha1 of `%s`: %s"
         f (Printexc.to_string e)
       |> Lwt.return)

let abspath f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let sha1_of_string s = Sha.digest_string s |> Sha.to_hex

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

  let make_leaf ~encode ~decode descr =
    make ~encode:(fun x -> encode x, []) ~decode descr

  let make_bencode ~encode ~decode descr =
    make descr
      ~encode:(fun x ->
        let b, children = encode x in
        Bencode.encode_to_string b, children)
      ~decode:(fun s ->
        Res_.(BM.decode_bencode s >>= decode))

  let make_leaf_bencode ~encode ~decode descr =
    make_bencode ~encode:(fun x -> encode x, []) ~decode descr

  let int =
    make_leaf "int"
      ~encode:string_of_int
      ~decode:(fun s ->
        begin
          try Ok (int_of_string s)
          with _ -> errorf "expected int, got `%s`" s
        end)

  let string =
    make_leaf "string" ~encode:(fun s->s) ~decode:(fun s->Ok s)

  let bool =
    make_leaf "bool"
      ~encode:string_of_bool
      ~decode:(fun s ->
        begin try Ok (bool_of_string s)
          with _ -> errorf "expected bool, got `%s`" s
        end)

  (* TODO: use the hexadecimal output of Printf when available ? *)
  let float =
    make_leaf "float"
      ~encode:(fun f -> Int64.((to_string @@ bits_of_float f)))
      ~decode:(fun s ->
        begin try Ok Int64.(float_of_bits @@ of_string s)
          with Failure _ -> errorf "expected float, got `%s`" s
        end)

  let marshal name =
    let flags = [Marshal.Closures; Marshal.No_sharing; Marshal.Compat_32] in
    make_leaf name
      ~encode:(fun x -> Marshal.to_string (name,x) flags)
      ~decode:(fun s ->
        let name', x = Marshal.from_string s 0 in
        if name=name' then x
        else failwith (Format.asprintf "codec.marshal: mismatch (value is %S, expected %S)" name' name)
      )

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
end

(** {2 References to Files} *)

module File_ref : sig
  type t

  val path : t -> path
  val hash : t -> hash
  val to_string : t -> string

  val make : path -> t or_error Lwt.t
  val make_exn : path -> t Lwt.t
  val is_valid : t -> bool Lwt.t

  val of_bencode : Bencode.t -> t or_error
  val to_bencode : t -> Bencode.t
  val codec : t Codec.t
end = struct
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
      let open Res_ in
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
    Maki_log.logf 5 (fun k->k "check if file %s is up-to-date..." (to_string f));
    (* compare [fs] with actual current file state *)
    make (path f) >>= fun res ->
    begin match res with
      | Error _ -> Lwt.return_true (* file not present, etc. *)
      | Ok f' ->
        let res =
          path f = path f' &&
          hash f = hash f'
        in
        Maki_log.logf 5 (fun k->k "file %s up-to-date? %B" (to_string f) res);
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

  let make (f:path) = find f >>>= File_ref.make

  let to_string = File_ref.to_string
end

(** {2 Time Utils} *)

module Time = struct
  type t = time
  let seconds = float_of_int
  let hours n = float_of_int n *. 3600.
  let minutes n = float_of_int n *. 60.
  let days n = float_of_int n *. hours 24
  let weeks n = 7. *. days n
  let now () = Unix.gettimeofday()
  let (++) = (+.)
  let pp out t = Format.fprintf out "%.3fs" t
end

module Lifetime = struct
  type t =
    | Keep
    | KeepFor of time (** Time delta *)
    | KeepUntil of time (** Absolute deadline *)
    | CanDrop

  let keep = Keep
  let can_drop = CanDrop
  let keep_for t = KeepFor t
  let keep_until t = KeepUntil t

  let short = keep_for (Time.minutes 10)
  let one_minute = keep_for (Time.minutes 1)
  let one_hour = keep_for (Time.hours 1)
  let one_day = keep_for (Time.days 1)

  let pp out = function
    | Keep -> Format.pp_print_string out "keep"
    | CanDrop -> Format.pp_print_string out "can_drop"
    | KeepFor t -> Format.fprintf out "keep_for %a" Time.pp t
    | KeepUntil t -> Format.fprintf out "keep_until %a" Time.pp t

  let default : t = keep_for @@ Time.weeks 10
end

module GC_info : sig
  type t =
    | Keep
    | KeepUntil of time
    | CanDrop
  val lt : t -> t -> bool
  val to_bencode : t -> Bencode.t
  val of_bencode : Bencode.t -> t or_error
  val codec : t Codec.t
  val of_lifetime : Lifetime.t -> t
end = struct
  type t =
    | Keep
    | KeepUntil of time
    | CanDrop

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

  let codec = Codec.make_leaf_bencode ~encode:to_bencode ~decode:of_bencode "gc_info"

  let of_lifetime = function
    | Lifetime.CanDrop -> CanDrop
    | Lifetime.Keep -> Keep
    | Lifetime.KeepUntil t -> KeepUntil t
    | Lifetime.KeepFor t ->
      let now = Unix.gettimeofday () in
      KeepUntil (now +. t)
end

(** {2 On-Disk storage} *)

module Storage = Maki_storage

(** {2 Value Stored on Disk} *)

module On_disk_record : sig
  type t = {
    gc_info: GC_info.t; (* how long should the GC keep this value *)
    key: hash; (* hash of the computation *)
    children: hash list; (* hash of children *)
    data: encoded_value; (* the actual data *)
  }

  val make :
    ?lifetime:Lifetime.t ->
    ?children:hash list ->
    hash ->
    encoded_value ->
    t

  val gc_info : t -> GC_info.t
  val key : t -> hash
  val children : t -> hash list
  val data : t -> encoded_value
  val lifetime : t -> Lifetime.t

  val codec : t Codec.t
end = struct
  type t = {
    gc_info: GC_info.t; (* how long should the GC keep this value *)
    key: hash; (* hash of the computation *)
    children: hash list; (* hash of children *)
    data: encoded_value; (* the actual data *)
  }

  let key c = c.key
  let children c = c.children
  let data c = c.data
  let gc_info c = c.gc_info

  let make ?(lifetime=Lifetime.default) ?(children=[]) key data =
    let gc_info = GC_info.of_lifetime lifetime in
    { gc_info; children; key; data; }

  (* serialize [c] into Bencode *)
  let encode c =
    B.Dict
      [ "gc_info", GC_info.to_bencode c.gc_info;
        "data", BM.mk_str c.data;
        "children", BM.mk_list (List.map BM.mk_str c.children);
        "key", BM.mk_str c.key;
      ]

  (* [s] is a serialized cached value, turn it into a [cache_value] *)
  let decode (b:Bencode.t) : t or_error =
    let open Res_ in
    begin match b with
      | B.Dict l ->
        BM.assoc "gc_info" l >>= GC_info.of_bencode >>= fun gc_info ->
        BM.assoc "data" l >>= BM.as_str >>= fun data ->
        BM.assoc_or (B.List []) "children" l
        |> BM.as_list >>= map_l BM.as_str >>= fun children ->
        BM.assoc "key" l >>= BM.as_str >>= fun key ->
        return {data; gc_info; key; children; }
      | b ->
        errorf "expected on_disk_record, got `%s`"
          (Bencode.encode_to_string b)
    end

  let codec = Codec.make_leaf_bencode ~encode ~decode "on_disk_record"

  let lifetime c = match c.gc_info with
    | GC_info.Keep  -> Lifetime.Keep
    | GC_info.KeepUntil t -> Lifetime.KeepUntil t
    | GC_info.CanDrop  -> Lifetime.CanDrop
end

(** {2 Reference to On-Disk Value} *)

module Ref = struct
  type 'a t = 'a Codec.t * hash

  let hash = snd
  let codec = fst

  let store
      ?(storage=Storage.get_default())
      ?lifetime
      codec
      x =
    let data, children = Codec.encode codec x in
    let key = Sha.digest_string data |> Sha.to_hex in
    let record = On_disk_record.make ?lifetime ~children key data in
    let record_s, _ = Codec.encode On_disk_record.codec record in
    Storage.set storage key record_s >>|= fun () ->
    codec, key

  let find ?(storage=Storage.get_default()) (codec,key) =
    Storage.get storage key >>>= fun s ->
    begin match s with
      | None -> errorf "could not find value `%s` on storage" key
      | Some s ->
        begin match Codec.decode On_disk_record.codec s with
          | Error _ as e -> e
          | Ok record ->
            assert (On_disk_record.key record = key);
            let data = On_disk_record.data record in
            Codec.decode codec data
        end
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

module Hash = struct
  module Sha = Sha
  type 'a t = Sha.ctx -> 'a -> Sha.ctx

  let hash (h:_ t) x =
    let buf = Sha.init() in
    h buf x |> Sha.get

  let hash_to_string h x = hash h x |> Sha.to_hex

  let str_ ctx s = Sha.feed_string ctx s

  let unit ctx _ = ctx
  let int ctx x = str_ ctx (string_of_int x)
  let bool ctx x = str_ ctx (string_of_bool x)
  let float ctx x = str_ ctx (string_of_float x)
  let string ctx x = str_ ctx x

  let map f h ctx x = h ctx (f x)

  let list h ctx l = List.fold_left h ctx l
  let array h ctx a = list h ctx (Array.to_list a)

  let file_ref ctx (f:File_ref.t) =
    let h = File_ref.hash f in
    str_ ctx (File_ref.path f);
    str_ ctx h

  let program_ref ctx (p:Program_ref.t) =
    let h = Program_ref.as_file p in
    file_ref ctx h

  let pair h1 h2 ctx (x1,x2) =
    h2 (h1 ctx x1) x2

  let triple h1 h2 h3 ctx (x1,x2,x3) =
    h3 (h2 (h1 ctx x1) x2) x3

  let quad h1 h2 h3 h4 ctx (x1,x2,x3,x4) =
    h4 (h3 (h2 (h1 ctx x1) x2) x3) x4

  (* set: orderless. We compute all hashes, sort, then hash the resulting list *)
  let set h ctx l =
    begin
      List.map (fun x -> hash_to_string h x) l
      |> List.sort String.compare
      |> List.fold_left str_ ctx
    end

  let marshal: 'a t = fun ctx x ->
    str_ ctx (Marshal.to_string x [Marshal.Closures])

  let of_codec (c:'a Codec.t): 'a t =
    fun ctx x ->
      let s, _ = Codec.encode c x in
      string ctx s
end

(** {2 Arguments} *)
module Arg = struct

  type t = A : 'a Hash.t * 'a -> t

  let make h x = A(h,x)
  let of_codec c x = make (Hash.of_codec c) x

  module Infix = struct
    let (@::) h x = make h x
  end
  include Infix
end

(** {2 Memoized Functions} *)

(** {2 Result of Memoized Computation} *)
module Compute_res : sig
  type 'a t

  val computation_name : _ t -> hash
  val tags : _ t -> string list
  val result : 'a t -> 'a Ref.t
  val children : _ t -> hash list

  val make : ?tags:string list -> hash -> 'a Ref.t -> 'a t

  val save : ?storage:Storage.t -> ?lifetime:Lifetime.t -> 'a t -> unit or_error Lwt.t

  val get : ?storage:Storage.t -> ?lifetime:Lifetime.t -> hash -> 'a Codec.t -> 'a t option or_error Lwt.t

  val find : ?storage:Storage.t -> ?lifetime:Lifetime.t -> hash -> 'a Codec.t -> 'a t or_error Lwt.t

  val to_record : ?lifetime:Lifetime.t -> 'a t -> On_disk_record.t

  val codec : 'a Codec.t -> 'a t Codec.t
end = struct
  type 'a t = {
    computation_name: hash; (** computed value (hash of) *)
    result: 'a Ref.t; (** reference to the result *)
    tags: string list; (** Some metadata *)
  }

  let tags c = c.tags
  let result c = c.result
  let computation_name c = c.computation_name
  let children c = [c.result |> Ref.hash]

  let make ?(tags=[]) name result = {tags; computation_name=name; result}

  (* serialize [c] into Bencode *)
  let encode c =
    let b = B.Dict
        [ "name", BM.mk_str (computation_name c);
          "ref", BM.mk_str (result c |> Ref.hash);
          "tags", BM.mk_list (List.map BM.mk_str (tags c));
        ] in
    b, children c

  (* [s] is a serialized result, turn it into a result *)
  let decode (codec:'a Codec.t) (b:Bencode.t) : 'a t or_error =
    let open Res_ in
    begin match b with
      | B.Dict l ->
        BM.assoc "ref" l >>= BM.as_str >>= fun hash ->
        let result : _ Ref.t = codec, hash in
        BM.assoc "name" l >>= BM.as_str >>= fun name ->
        BM.assoc_or (B.List []) "tags" l
        |> BM.as_list >>= map_l BM.as_str >|= fun tags ->
        make ~tags name result
      | _ ->
        errorf "expected cache_value, got `%s`" (Bencode.encode_to_string b)
    end

  let codec (c:'a Codec.t): 'a t Codec.t =
    Codec.make_bencode
      ~encode
      ~decode:(decode c)
      "compute_res"

  let save_record_ ~storage key record =
    Maki_log.logf 3
      (fun k->k "save result `%s` into storage %s" key (Storage.name storage));
    let record_s, _ = Codec.encode On_disk_record.codec record in
    Storage.set storage key record_s

  let to_record ?lifetime (c:_ t): On_disk_record.t =
    let key = computation_name c in
    let c_bencode, children = encode c in
    let c_string = Bencode.encode_to_string c_bencode in
    On_disk_record.make ?lifetime ~children key c_string

  (* specific storage, indexed by [computation_name] rather than by the
     record's sha1 itself *)
  let save ?(storage=Storage.get_default()) ?lifetime (c:_ t) =
    let key = computation_name c in
    let record = to_record ?lifetime c in
    save_record_ ~storage key record

  let get ?(storage=Storage.get_default ()) ?(lifetime=Lifetime.CanDrop) key c =
    Storage.get storage key >>>=
    function
    | None -> E.return None
    | Some s ->
      let new_gc_info = GC_info.of_lifetime lifetime in
      Codec.decode On_disk_record.codec s |> Lwt.return >>>= fun record ->
      (* check if we should refresh the on-disk's value expiration date *)
      begin
        if GC_info.lt (On_disk_record.gc_info record) new_gc_info then (
          let record = {record with On_disk_record.gc_info=new_gc_info} in
          save_record_ ~storage key record
        ) else E.return_unit
      end >>>= fun () ->
      let s = On_disk_record.data record in
      Codec.decode (codec c) s |> Res_.map (fun x-> Some x) |> Lwt.return

  let find ?storage ?lifetime k c =
    get ?storage ?lifetime k c
    >|= function
    | Ok (Some x) -> Ok x
    | Ok None -> errorf "could not find key `%s`" k
    | Error _ as e -> e
end

(* compute the hash of the result of computing the application of
   the function named [fun_name] on dependencies [l] *)
let compute_name (fun_name:string) (args:Arg.t list): hash =
  let ctx = Sha.feed_string (Sha.init ()) fun_name in
  List.fold_left (fun ctx (Arg.A(h,x)) -> h ctx x) ctx args |>
  Sha.get |> Sha.to_hex

(* map computation_name -> future (serialized) result *)
type memo_table = (string, encoded_value lazy_t or_error Lwt.t) Hashtbl.t

let memo_table_ : memo_table = Hashtbl.create 64

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
    ?(storage=Storage.get_default())
    ?lifetime
    ?limit
    ?tags
    ~name:fun_name
    ~args
    ~(returning:'a Codec.t)
    (f:unit -> 'a or_error Lwt.t) : 'a or_error Lwt.t
  =
  (* compute the "name" of the computation *)
  let name = compute_name fun_name args in
  (* compute the result of calling the function, or retrieve the result in
     cache. This returns a [('a * string) or_error Lwt.t] where
     the string is the serialization of the ['a] *)
  let compute_memoized (): ('a * 'a Compute_res.t) or_error Lwt.t =
    begin match limit with
      | None -> f ()
      | Some l -> Limit.acquire l f
    end
    >>>= fun res ->
    (* store result *)
    Ref.store ~storage ?lifetime returning res
    >>>= fun res_ref ->
    let compute_res = Compute_res.make ?tags name res_ref in
    Compute_res.save ~storage ?lifetime compute_res >>|= fun () ->
    res, compute_res
  in
  (* check on-disk cache *)
  let check_cache_or_compute () : ('a * 'a Compute_res.t) or_error Lwt.t =
    Compute_res.get ~storage ?lifetime name returning >>>=
    function
    | None ->
      (* not in cache, perform computation (possibly acquiring
         the "limit" first) *)
      Maki_log.logf 3
        (fun k->k "could not find `%s` in storage %s" name (Storage.name storage));
      compute_memoized ()
    | Some compute_res ->
      Maki_log.logf 3
        (fun k->k "found result of `%s` in storage %s" name (Storage.name storage));
      (* read result from the raw data *)
      let data_ref = Compute_res.result compute_res in
      Ref.find ~storage data_ref >>= fun data ->
      begin match data with
        | Error e ->
          Maki_log.logf 3
            (fun k->k "cached file for `%s` is invalid, delete it and recompute: %s"
                name e);
          Storage.remove storage name >>= fun () ->
          compute_memoized ()
        | Ok res -> E.return (res, compute_res)
      end
  in
  (* check memo table *)
  try
    let future_res = Hashtbl.find memo_table_ name in
    (* ok, some other thread is performing the computation, just wait
         for it to complete and deserialize the result *)
    future_res >>>= fun (lazy encoded_value) ->
    (Codec.decode (Compute_res.codec returning) encoded_value |> Lwt.return)
    >>|= Compute_res.result
    >>>= Ref.find ~storage
  with Not_found ->
    let res_record, wait_record = Lwt.wait () in
    (* put future result in memo table in case another thread wants to
       compute the same value *)
    Hashtbl.add memo_table_ name res_record;
    (* compute result *)
    let res = check_cache_or_compute() in
    (* ensure that when [res] terminates, [res_serialized] is updated,
       and cleanup entry from hashtable to avoid clogging memory *)
    Lwt.on_any res
      (function
        | Ok (_, compute_res) ->
          Hashtbl.remove memo_table_ name;
          let data = lazy (
            Compute_res.to_record ?lifetime compute_res
            |> On_disk_record.data
          ) in
          Lwt.wakeup wait_record (Ok data)
        | Error e ->
          Hashtbl.remove memo_table_ name;
          Lwt.wakeup wait_record (Error e))
      (fun e ->
         Lwt.wakeup wait_record
           (errorf "error when computing %s: %s" name (Printexc.to_string e)));
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

let call_pure ?bypass ?storage ?lifetime ?limit ?tags ~name ~args ~returning f =
  call ?bypass ?storage ?lifetime ?limit ?tags ~name ~args ~returning
    (fun () -> f () >|= Res_.return)

module Fun = struct
  type (_, _, _) t =
    | Fun : 'a Hash.t * ('f, 'f2, 'ret) t -> ('a -> 'f, 'f2, 'ret) t
    | Ret : string * 'a Codec.t ->
      ('a or_error Lwt.t,
       ((unit -> 'a or_error Lwt.t) -> string -> Arg.t list -> 'a Codec.t -> 'a or_error Lwt.t),
       'a) t

  let returning ~name c = Ret (name,c)
  let (@->) h f = Fun (h,f)

  type 'f call_wrap =
    ?bypass:bool ->
    ?storage:Storage.t ->
    ?lifetime:Lifetime.t ->
    ?limit:Limit.t ->
    ?tags:string list ->
    'f

  let call : type f1 f2 ret. ((f1, f2, ret) t -> f1 -> f1) call_wrap =
    fun ?bypass ?storage ?lifetime ?limit ?tags def f ->
      let rec pass_args
        : type g1 g2 ret. Arg.t list -> (g1, g2, ret) t -> (unit -> g1) -> g1
        = fun args def f -> match def with
          | Ret (name,codec) ->
            let args = List.rev args in
            call ?bypass ?storage ?lifetime ?limit ?tags ~name ~args ~returning:codec f
          | Fun (h, sub_def) ->
            (fun x ->
               let f () = f () x in
               let a = Arg.make h x in
               pass_args (a::args) sub_def f)
      in
      pass_args [] def (fun () -> f)
end

let return_ok x = E.return x
let return_fail s = E.fail s

let mk1 ?bypass ?storage ?lifetime ?limit ?tags ~name h1 ret ~f =
  let open Fun in
  call ?bypass ?storage ?lifetime ?limit ?tags (h1 @-> returning ~name ret) f

let mk2 ?bypass ?storage ?lifetime ?limit ?tags ~name h1 h2 ret ~f =
  let open Fun in
  call ?bypass ?storage ?lifetime ?limit ?tags (h1 @-> h2 @-> returning ~name ret) f

let mk3 ?bypass ?storage ?lifetime ?limit ?tags ~name h1 h2 h3 ret ~f =
  let open Fun in
  call ?bypass ?storage ?lifetime ?limit ?tags (h1 @-> h2 @-> h3 @-> returning ~name ret) f

let mk4 ?bypass ?storage ?lifetime ?limit ?tags ~name h1 h2 h3 h4 ret ~f =
  let open Fun in
  call ?bypass ?storage ?lifetime ?limit ?tags (h1 @-> h2 @-> h3 @-> h4 @-> returning ~name ret) f

let mk5 ?bypass ?storage ?lifetime ?limit ?tags ~name h1 h2 h3 h4 h5 ret ~f =
  let open Fun in
  call ?bypass ?storage ?lifetime ?limit ?tags (h1 @-> h2 @-> h3 @-> h4 @-> h5 @-> returning ~name ret) f

(** {2 GC} *)

module GC = struct
  type stats = {
    roots: int;
    kept: int;
    removed: int;
  }

  let string_of_stats s =
    Printf.sprintf "kept %d entries (%d roots), removed %d entries"
      s.kept s.roots s.removed

  type gc_cell = {
    gc_children: hash list;
    mutable gc_status: [`Root | `Alive | `Dead];
  }

  type state = (string, gc_cell) Hashtbl.t

  (* find the set of roots, collect the graph in RAM *)
  let collect_roots now s : state or_error Lwt.t =
    Maki_log.log 3 "gc: collecting roots...";
    let state = Hashtbl.create 256 in
    Storage.fold s ~x:()
      ~f:(fun () (key, value) ->
        (* decide whether to add [key] to [set], so it remains alive, or now *)
        (Codec.decode On_disk_record.codec value |> Lwt.return)
        >>|= fun record ->
        (* remember dependencies of [key] *)
        let gc_status = match On_disk_record.gc_info record with
          | GC_info.Keep -> `Root
          | GC_info.KeepUntil t -> if t >= now then `Root else `Dead
          | GC_info.CanDrop -> `Dead
        in
        Hashtbl.add state key
          {gc_children=On_disk_record.children record; gc_status};
        ())
    >>|= fun () ->
    Maki_log.logf 3
      (fun k->k "root collection is done (%d entries)" (Hashtbl.length state));
    state

  let cleanup ?(force=false) s: stats or_error Lwt.t =
    let now = Unix.gettimeofday () in
    collect_roots now s >>>= fun st ->
    (* actually collect dead cells *)
    let n_roots = ref 0 in
    let n_kept = ref 0 in
    let n_removed = ref 0 in
    let err = ref None in
    Maki_log.log 3 "start collection of dead values";
    Hashtbl.iter
      (fun k c ->
         match c.gc_status with
         | (`Alive | `Root) when not force ->
           Maki_log.logf 5 (fun f->f "gc: keep value %s" k);
           if c.gc_status = `Root then incr n_roots;
           incr n_kept
         | _ ->
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
        let stats = {
          roots= !n_roots;
          kept= !n_kept;
          removed= !n_removed
        } in
        Lwt.return (Ok stats)
    end
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

include E.Infix
