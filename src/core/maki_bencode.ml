
(* This file is free software. See file "license" for more details. *)

(** {1 Helpers for Bencode} *)

module B = Bencode
open Result

exception Maki_error of string

let decode_bencode s =
  try Ok (B.decode (`String s))
  with _ -> Error (Maki_error (s ^ " is not valid Bencode"))

let assoc k l =
  try Ok (List.assoc k l)
  with e -> Error e

let as_str = function
  | B.String s -> Ok s
  | _ -> Error (Maki_error "expected string")

let as_float = function
  | B.String s ->
    begin try Ok (float_of_string s) with e -> Error e end
  | _ -> Error (Maki_error "expected string")

let as_list = function
  | B.List l -> Ok l
  | _ -> Error (Maki_error "expected list")

let mk_str s = B.String s
let mk_list l = B.List l
let mk_dict l = B.Dict l
let mk_pair x y = B.List [x; y]
let mk_triple x y z = B.List [x;y;z]
let mk_quad x y z u = B.List [x;y;z;u]

let expected_s what s =
  Error (Maki_error (Printf.sprintf "expected %s, got %s" what s))

let expected_b what b = expected_s what (B.encode_to_string b)
