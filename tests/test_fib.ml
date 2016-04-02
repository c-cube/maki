(* Naive fibonacci function *)

open Lwt.Infix

let rec fib n =
  if n<= 1 then Lwt.return 1
  else
    let n1 = fib_memo (n-1) in
    let n2 = fib_memo (n-2) in
    n1 >>= fun n1 -> n2 >|= fun n2 -> n1+n2

and fib_memo n =
  Maki.call_exn
    ~lifetime:(`KeepFor (Maki.Time.minutes 20))
    ~name:"fib"
    ~op:Maki.Value.int
    ~deps:[Maki.Value.pack_int n]
    (fun () -> fib n)

let main n =
  fib n >|= fun n ->
  Printf.printf "result: %d\n" n

let () =
  let n = ref 20 in
  Arg.parse
    [ "-n", Arg.Set_int n, " set number (default 20)"
    ; "--debug", Arg.Int Maki_log.set_level, " set debug level"
    ] (fun _ -> failwith "no arguments") "usage: test_fib";
  Lwt_main.run (main !n)
