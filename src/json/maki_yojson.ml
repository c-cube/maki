
(* This file is free software. See file "license" for more details. *)

(** {1 Simple wrapper for Yojson} *)

type 'a or_error = ('a, exn) Result.result

(** Yojson.Safe *)
type json = Yojson.Safe.json

let (>>=) r f = match r with
  | Result.Ok x -> f x
  | Result.Error e -> Result.Error e

let make ~to_yojson ~of_yojson name =
  Maki.Value.make_fast name
    ~serialize:(fun x ->
      let j = to_yojson x in
      Maki_bencode.mk_str (Yojson.Safe.to_string j))
    ~unserialize:(fun b ->
      Maki_bencode.as_str b >>= fun s ->
      (try Result.Ok (Yojson.Safe.from_string s) with e -> Result.Error e)
      >>= of_yojson)

let make_str ~to_yojson ~of_yojson name =
  make name ~to_yojson
    ~of_yojson:(
      fun j -> match of_yojson j with
        | Result.Ok x -> Result.Ok x
        | Result.Error e -> Result.Error (Failure e))

let make_err ~to_yojson ~of_yojson name =
  make name ~to_yojson
    ~of_yojson:(
      fun j -> match of_yojson j with
        | `Ok x -> Result.Ok x
        | `Error e -> Result.Error (Failure e))


