
(* This file is free software. See file "license" for more details. *)

(** {1 Helpers for Bencode} *)

module B = Bencode
open Result

let expected_s what s =
  Error (Printf.sprintf "maki: bencode: expected %s, got %s" what s)

let expected_b what b = expected_s what (B.encode_to_string b)

let decode_bencode s =
  try Ok (B.decode (`String s))
  with e ->
    Error (s ^ " is not valid Bencode: " ^ Printexc.to_string e)

let assoc k l =
  try Ok (List.assoc k l)
  with Not_found -> Error ("could not find key " ^ k)

let assoc_or default k l =
  try List.assoc k l
  with _ -> default

let as_str = function
  | B.String s -> Ok s
  | b -> expected_b "string" b

let as_float = function
  | B.String s as b ->
    begin try Ok (float_of_string s)
      with _ -> expected_b "float" b
    end
  | _ -> Error "expected string"

let as_list = function
  | B.List l -> Ok l
  | b -> expected_b "list" b

let mk_str s = B.String s
let mk_list l = B.List l
let mk_dict l =
  let l = List.sort (fun (n1,_)(n2,_) -> compare n1 n2) l in
  B.Dict l
let mk_pair x y = B.List [x; y]
let mk_triple x y z = B.List [x;y;z]
let mk_quad x y z u = B.List [x;y;z;u]
