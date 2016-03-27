
(* This file is free software. See file "license" for more details. *)

open Result

type 'a or_error = ('a, string) Result.result

type 'a t = 'a or_error Lwt.t

let return x = Lwt.return (Ok x)

let return_unit = Lwt.return (Ok ())

let fail msg = Lwt.return (Error msg)

let (>>=) x f =
  Lwt.bind x
    (function
      | Error msg -> fail msg
      | Ok y -> f y
    )

let (>|=) x f =
  Lwt.map
    (function
      | Error _ as e -> e
      | Ok x -> Ok (f x))
    x
