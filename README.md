# maki

Persistent incremental computations, for repeatable tests and benchmarks.

**Status**: beta

[![build status](https://api.travis-ci.org/c-cube/maki.svg?branch=master)](https://travis-ci.org/c-cube/maki)

For more details, see [the initial design document (obsolete)](doc/maki_design.md)
and the [blog post](https://cedeela.fr/maki-on-disk-memoization-for-deterministic-fun-and-profit.html)

## Examples

### Simple memoizing of a recursive function

```ocaml
let fib =
  let rec fib n = Maki.(
      mk1 ~name:"fib" Hash.int Codec.int ~lifetime:Lifetime.one_minute
        ~f:(fun x -> if x <= 1
          then return_ok 1
          else (fib (x-1) >>= fun x1 ->
            fib (x-2) >|= fun x2 -> x1+x2))
        n
    ) in
  fib;;

fib 42 ;;
(* returns [Ok 42] *)
```

### Concatenating file, but memoizing the result as long as they do not change

```ocaml

open Lwt.Infix;;

let concat =
  Maki.(mk2 ~name:"concat" Hash.file_ref Hash.file_ref Codec.string ~lifetime:Lifetime.one_hour
    ~f:(fun f1 f2 ->
      let open E in
      read_file f1 >>= fun content1 ->
      read_file f2 >>= fun content2 ->
      return_ok (content1 ^ content2)))
;;

let x1 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;

(* cached *)
let x2 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;

(* now change contnet of file "foo1", so this should change too *)
let x3 = Maki.(File_ref.make "foo1" >>= fun f1 -> File_ref.make "foo2" >>= concat f1);;


```

## Documentation

See http://c-cube.github.io/maki/
