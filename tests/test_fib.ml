(* Naive fibonacci function *)

open Maki.E

let rec fib n =
  if n<= 1 then return 1
  else
    let n1 = fib_memo (n-1) in
    let n2 = fib_memo (n-2) in
    n1 >>= fun n1 -> n2 >|= fun n2 -> n1+n2

and fib_memo n =
  Maki.call
    ~lifetime:(`KeepFor (Maki.Time.minutes 20))
    ~name:"fib"
    ~returning:Maki.Codec.int
    ~args:Maki.Arg.([Hash.int @:: n])
    (fun () -> fib n)

let main n =
  let open Lwt.Infix in
  fib n >|= function
  | Error msg -> Printf.printf "error: %s\n" msg
  | Ok n -> Printf.printf "result: %d\n" n

let () =
  let n = ref 20 in
  Arg.parse
    [ "-n", Arg.Set_int n, " set number (default 20)"
    ; "--debug", Arg.Int Maki_log.set_level, " set debug level"
    ] (fun _ -> failwith "no arguments") "usage: test_fib";
  Lwt_main.run (main !n)
