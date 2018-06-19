
(* This file is free software. See file "license" for more details. *)

open Result

type 'a or_error = ('a, string) Result.result

type 'a t = 'a or_error Lwt.t

let return x = Lwt.return (Ok x)

let return_unit = Lwt.return (Ok ())

let fail msg = Lwt.return (Error msg)

let unwrap_res = function
  | Ok x -> Lwt.return x
  | Error e -> Lwt.fail e

let lift_ok m = Lwt.(m >|= fun x -> Ok x)

let lift_err m = Lwt.(m >|= fun x -> Error x)

module Infix = struct
  let (<*>) f x =
    Lwt.bind f
      (function
        | Error msg -> fail msg
        | Ok f ->
          Lwt.map
            (function
              | Ok x -> Ok (f x)
              | Error msg -> Error msg)
            x)

  let (>>=) x f =
    Lwt.bind x
      (function
        | Error msg -> fail msg
        | Ok y -> f y)

  let (>|=) x f =
    Lwt.map
      (function
        | Error _ as e -> e
        | Ok x -> Ok (f x))
      x

  let (<$>) f x = x >|= f
end

include Infix
