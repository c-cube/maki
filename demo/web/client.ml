open Js_of_ocaml_lwt

let f () =
  let%lwt () = Lwt_js.sleep 10. in
  Lwt.return 42

let f () =
  let storage = Maki_jsoo.local_storage in
  Maki.call_pure ~storage ~name:"pouet" ~args:[] ~returning:Maki.Codec.int f


let () = Lwt.async @@ fun () ->
  match%lwt f () with
  | Error _ -> print_endline "err"; Lwt.return ()
  | Ok r -> Printf.printf "maki %d\n" r; Lwt.return ()
