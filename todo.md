# TODO

- applicative (++) framework for memoized concurrent computations
  * deps: `sha1`, `lwt.unix`, `bencode`
  * type `'a t` for computations returning `a`
  * easy import from `'a Cmdliner.term` to `'a t`
  * need some notion of config (non commutative config monoid with `++`
    for overriding right with left, empty config as neutral)
    → inspiration from cmdliner
  * not a monad (imprevisible control flow), but many combinators to help:
    + `<*> : ('a -> 'b) t -> 'a t -> 'b t`
    + `map_l : ?j:int t -> ('a -> 'b) t -> 'a list t -> 'b list t`
    + `flat_map_l : ?j:int t -> ('a t -> 'b list) t -> 'a list t -> 'b list t`
    + `fold_l : ('a -> 'b -> 'a) t -> 'a t -> 'b list t -> 'a t`
    + `if_ : bool t -> 'a t -> 'a t -> 'a t`
  * basic building blocks for plugging computations in there:
    + favor typed combinators
      (pack the value with printer + sha1 methods):
      `string : string -> string t`
      `int : int -> int t`
      `opt : 'a operations -> 'a option -> 'a option t`?
      `file : string t -> string t` (find absolute path + hash)
      `program : string t -> string t` (look in PATH + hash)
      `fun_ : 'b operations -> string -> ('a -> 'b) -> ('a t -> 'b t) t` (trust the name is unique)
      `fun1`, `fun2`, … ?
      for large-arity functions,
      use `pure` with a dummy operation (name+sha1 of name),
      or `pure_fun : string -> ('a -> 'b) -> ('a -> 'b) t`
    + polymorphic `pure : 'a operations -> 'a -> 'a t`
      needs the explicit printer+sha1
    + `catch : 'a or_error -> 'a t` (error handling baked-in)
    + `lwt : 'a Lwt.t -> 'a t`
    + `lwt_catch : 'a or_error Lwt.t -> 'a t`
    + `get_conf : conf t` (probably parametrized?)
      OR: `get : 'a Het_map.key -> 'a t`
          `set : 'a Het_map.key -> 'a t -> unit t`
          so that we can carry arbitrary config
    + `call_process : ?timeout -> ?max_mem -> string t -> some_process_type t`
      with `stdout : some_process_type t -> string t`
           `wait : some_process_type t -> unit t`
      need to think about how to make it convenient (process string
      is usually quite complicated: provide `string list -> string` with
      escaping, and format4-based utils?)
    + `>> : unit t -> 'a t -> 'a t` to chain computations sequentially
      along with `sequence_l : unit t list -> unit t`
    + `print : 'a printer -> 'a t -> 'a t` to print sth (maybe with format4, too)
  * favor overloading and hooks with sane default to providing everything early,
    in a `|>`-friendly way:
    + `set_printer : 'a Fmt.t -> 'a t -> 'a t`
    + `on_success : ('a -> unit Lwt.t) -> 'a t -> 'a t`
      `before_start : (unit -> unit Lwt.t) -> 'a t -> 'a t` (logging, etc.)
      `on_done: ('a or_error -> unit Lwt.t) -> 'a t -> 'a t`  (typically, for reporting)
    + `report_debug : 'a t -> 'a t` (if debug enabled, report result)
  * `run : 'a t -> 'a or_error Lwt.t` main runner, will take a while
  * need something to handle dynamic dependencies… 
  * context manager (for lock, containers, etc.)
    + `in_ctx : 'ctx context -> ('ctx t -> 'a t) -> 'ctx t -> 'a t`
      where `'a context` provides a pre-hook and post-hook
    + lock (froglock, file lock, etc.)
    + plugguable scheduler (to run somewhere else) as a context, to
      do only the expensive computations within the context

- `Maki_memo` as optional module on the side for checkpointing these computations
  * combinator:
    `memo : deps:Maki_memo.deps -> 'a Maki_memo.op -> (unit -> 'a) t -> (unit -> 'a) t`
  * move storage, etc. inside this, with simple API (1st class module for
    storage with `get/set/iter`). Keep default storage.
  * OR: maybe, do it only for string,
    i.e `memo : (unit -> string) t -> (unit -> string) t`,
        `memo1 : ('a -> string) t -> 'a t -> string t`,
        `memo2`, etc. only requiring that the input parameters are hashable,
        not parsable (and anything is hashable!)
