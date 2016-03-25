
(* This file is free software. See file "license" for more details. *)

(** {1 Maki: Persistent Incremental Computations} *)

open Result
open Lwt.Infix

module B = Bencode

type 'a or_error = ('a, string) Result.result
type 'a lwt_or_error = 'a or_error Lwt.t
type ('a,'rw) pipe = ('a,'rw) Maki_pipe.t

module Res_ = struct
  let (>|=) r f = match r with
    | Ok x -> Ok (f x)
    | Error e -> Error e
end

(** {2 Basic types} *)

type path = string
type program = string

(** {2 Values} *)
module Value = struct
  type 'a ops = {
    descr: string; (* description of 'a *)
    serialize : 'a -> Bencode.t;
    unserialize : Bencode.t -> 'a or_error;
  }

  type t = Value : 'a ops * 'a -> t
  (** Universal type, can be hashed, serialized, etc. *)

  let expected_s what s = Error (Printf.sprintf "expected %s, got %s" what s)
  let expected_b what b = expected_s what (B.encode_to_string b)

  let int = {
    descr="int";
    serialize=(fun i -> B.Integer i);
    unserialize=(function
      | B.Integer i -> Ok i
      | B.String s ->
        begin
          try Ok (int_of_string s)
          with _ -> expected_s "int" s
        end;
      | b -> expected_b "int" b
      );
  }
 
  let string = {
    descr="string";
    serialize=(fun s -> Bencode.String s);
    unserialize=(function
        | B.String s -> Ok s
        | b -> expected_b "string" b
      );
  }

  let bool = {
    descr="bool";
    serialize=(fun b -> if b then B.Integer 1 else B.Integer 0);
    unserialize=(function
          | B.Integer 0 -> Ok false
          | B.Integer n when n<>0 -> Ok true
          | B.String s ->
            begin try Ok (bool_of_string s)
              with _ -> expected_s "bool" s
            end
          | b -> expected_b "bool" b
        );
  }

  (* FIXME: behavior is different, comparison should be by timestamp+hash *)
  let file = string

  exception ExitUn of string

  let map_res_l f l =
    try 
      let res =
        List.map
        (fun x -> match f x with Ok y -> y | Error s -> raise (ExitUn s))
        l
      in
      Ok res
    with ExitUn s -> Error s

  let list op =
    let descr = "list " ^ op.descr in
    { descr;
      serialize=(fun l -> B.List (List.map op.serialize l));
      unserialize=(function
          | B.List l -> map_res_l op.unserialize l
          | b -> expected_b descr b
        );
    }

  let assoc op =
    let descr = "assoc " ^ op.descr in
    { descr;
      serialize=(fun l -> B.Dict (List.map (fun (name,x) -> name, op.serialize x) l));
      unserialize=(function
          | B.Dict l ->
            map_res_l
              (fun (name,x) -> Res_.(op.unserialize x >|= fun x -> name, x))
              l
          | b -> expected_b descr b
        );
  }

  let pair a b = assert false
  let triple a b c = assert false
  let quad a b c d = assert false

  let serialize op x = op.serialize x
  let unserialize op x = op.unserialize x
  let to_string op x = B.encode_to_string (serialize op x)
  let of_string op s =
    try
      let b = B.decode (`String s) in
      unserialize op b
    with e -> Error (Printexc.to_string e)
  let pack op x = Value (op,x)
end

(** {2 Controlling Parallelism} *)

module J = struct
  let j_ = ref 1

  let get () = !j_
  let set n =
    if n<1 then invalid_arg "J.set";
    j_ := n

  (* pool used to limit concurrent access to cores, memory, etc. *)
  let pool_ =
    lazy (Lwt_pool.create !j_ (fun () -> Lwt.return_unit))

  let acquire f = Lwt_pool.use (Lazy.force pool_) f
end

(** {2 On-Disk storage} *)
module Storage = struct
  type t = {
    name: string;
    get: string -> string or_error;
    set: string -> string -> unit or_error;
    iter: unit -> (string * string, [`r]) Maki_pipe.t;
  }

  let env_var_ = "MAKI_DIR"

  (* TODO *)

  let default_get ~dir k = assert false
  let default_set ~dir k v = assert false
  let default_iter ~dir () = assert false

  let default ?dir () =
    let dir = match dir with
      | Some d -> d
      | None ->
        try Sys.getenv env_var_
        with Not_found ->
          ".maki/"
    in
    { name="shelf";
      get=default_get ~dir;
      set=default_set ~dir;
      iter=default_iter ~dir;
    }

  let storage_ = ref (default ())
  let set s = storage_ := s
  let cur () = !storage_
end

(** {2 Memoized Functions} *)

let abspath f =
  if Filename.is_relative f
  then Filename.concat (Sys.getcwd()) f
  else f

let sha1_of_string s = Sha1.string s |> Sha1.to_hex

let sha1 f =
  assert false (* TODO: open file, allocate buffer, use Sha1... maybe use mmap? *)

let last_mtime f = assert false (* TODO *)

let call
    ?(storage=Storage.cur ())
    ~name
    ~deps
    ~op
    f
  =
  assert false (* TODO *)

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
