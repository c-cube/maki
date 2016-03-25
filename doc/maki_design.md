
# Maki

Persistent incremental computations for OCaml, to subsume `make`, run
deterministic benchmarks, etc.

The main idea is to make some **pure** OCaml functions memoized, by storing
their result on disk for every tuple of inputs it is called on. The inputs
are instances of a type the interpreter knows about, and in particular
we have a notion of **pointer**, or **symbol** (a OCaml value that actually
refers to something outside of the system, for instance a path to refer
to a file's content, or an URL to refer to some remote content). This
type would look like the `Maki.value_` type below.
Such a memoized function can also **depend** on some other `value_`s that
are fixed, but that Maki still needs to know about because they might have
different versions (e.g. different versions of the same compiler, or tool,
or different versions of a config file whose path is fixed).

```ocaml
type value_ =
  | File of path (* equality is actually sha1(file) *)
  | Url of url (* equality is actually sha1(get url) *)
  | Program of program (* equality is actually sha1(which program) *)
  | Int of int
  | String of string (* raw content, typically after serialization *)
  | List of value_ list
  | Assoc of (string * value_) list
```

We should have a type `('a, 'ret) Maki.fun_`, that represents a memoized
function of type `'a` (returning `'ret`). For instance a function to run
"wc" on a file would have the type `(path -> int, int) Maki.fun_`,
and it would also declare a dependency on `Program "wc"`.

Maki's primary goal is to compute the result of functions applied to
arguments, **or** retrieve the result if it was already computed (with the
exact same set of inputs+dependencies).

## Storage

Instances of `value_` can be stored on disk (in JSON: use `ppx_deriving_yojson`
as much as possible). Memoized functions need to have unique names, because it
allows Maki to describe values on disk by their AST.

## Computation

To compute `f x_1...x_n`, where `f : (a_1 -> ... -> a_n -> ret, ret)`
and `x_i : a_i` (corresponding to `value_` or special cases of
values, such as `path`), assuming that the `x_i` are already computed (possibly
by previous invocations of Maki), the steps are:

1. make a string representation `computation := f x_1 ... x_n`
   of the computation to do
2. hash this representation into `h := sha1(computation)`
3. make a string representation
   `computation_instance := h sha1(x_1) ... sha1(x_n)` of the
   particular instance to compute (note that this is not the same as
   `computation` because, if `x_1 = File "/foo/bar"`, `h` will depend
   on the _name_ "/foo/bar", but `computation_instance` will depend
   on `sha1(/foo/bar)`, the hash of the current string content.
4. hash this particular instance `h_instance := sha1(computation_instance)`
5. if a file `h_instance` exists on disk, it must contain the result
   of the computation, so we unserialized the result and return it
6. otherwise, we do the computation, obtaining value `res : ret`;
   we store it into a file `h_instance` and then return it, so
   the next computation will stop at step 5.

Maki should provide a function `Maki.call : ('a, _) fun_ -> 'a Lwt.t`
that takes parameters of a given function and runs through the previous
steps. Several calls to `Maki.call` may be computed in parallel, hence
the use of `Lwt`.

Of course Maki should provide many helpers to read/write files, run
subprocess and obtain their results, etc. but `call` is the main
entry point.

## Design challenges

- garbage collection: need a set of root computations, compute transitive
  closure of inner computations, remove the rest?
- export to a "regular" result: shoud be quite easy, just compute a function
  that outputs a CSV or JSON file somewhere, it barely adds complexity to
  the whole computation
- distributed computation: `-j 5` is easy, running on remote nodes is not.
  But since we know the entire set of dependencies of every computation, at
  least we know what to copy on the remote machine.
- dynamic dependencies: when processing a TPTP file, say, we might discover
  it `include` some other file. This dependency should be expressible
  in the OCaml library (see above, need to add a dependency on the
  included file in some cases).
- central resource management for `-j 5` (costly functions should
  access a pseudo "semaphore" structure and run inside it, to limit
  parallelism).
