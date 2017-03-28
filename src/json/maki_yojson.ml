
(* This file is free software. See file "license" for more details. *)

(** {1 Simple wrapper for Yojson} *)

type 'a or_error = 'a Maki.or_error

(** Yojson.Safe *)
type json = Yojson.Safe.json

let (>>=) r f = match r with
  | Result.Ok x -> f x
  | Result.Error e -> Result.Error e

let decode_json s =
  try Result.Ok (Yojson.Safe.from_string s)
  with e ->
    Maki.errorf "expected json, got `%s`: %s" s (Printexc.to_string e)

let hash ctx j = Maki.Arg.Hash.string ctx (Yojson.Safe.to_string j)

let codec =
  Maki.Codec.make_leaf "json"
    ~encode:(fun j-> Yojson.Safe.to_string j)
    ~decode:decode_json

let make ~to_yojson ~of_yojson name =
  Maki.Codec.make name
    ~encode:(fun x ->
      let j = to_yojson x in
      Yojson.Safe.to_string j, [])
    ~decode:(fun s -> decode_json s >>= of_yojson)

let make_exn ~to_yojson ~of_yojson name =
  make name ~to_yojson
    ~of_yojson:(
      fun j -> match of_yojson j with
        | Result.Ok x -> Result.Ok x
        | Result.Error e -> Result.Error (Printexc.to_string e))

let make_err ~to_yojson ~of_yojson name =
  make name ~to_yojson
    ~of_yojson:(
      fun j -> match of_yojson j with
        | `Ok x -> Result.Ok x
        | `Error e -> Result.Error e)


