
(* This file is free software. See file "license" for more details. *)

type 'a equal = 'a -> 'a -> bool
type 'a hash = 'a -> int

let default_eq_ = Pervasives.(=)
let default_hash_ = Hashtbl.hash

(** {2 Value interface} *)

(** Invariants:
    - after [cache.set x y], [get cache x] must return [y] or raise [Not_found]
    - [cache.set x y] is only called if [get cache x] fails, never if [x] is already bound
    - [cache.size()] must be positive and correspond to the number of items in [cache.iter]
    - [cache.iter f] calls [f x y] with every [x] such that [cache.get x = y]
    - after [cache.clear()], [cache.get x] fails for every [x]
*)
type ('a,'b) t = {
  set : 'a -> 'b -> unit;
  get : 'a -> 'b;  (* or raise Not_found *)
  size : unit -> int;
  iter : ('a -> 'b -> unit) -> unit;
  clear : unit -> unit;
}

let clear c = c.clear ()

let with_cache c f x =
  try
    c.get x
  with Not_found ->
    let y = f x in
    c.set x y;
    y

let get c x = try Some (c.get x) with Not_found -> None
let set c x y = c.set x y

let size c = c.size ()

let iter c f = c.iter f

module Replacing = struct
  type ('a,'b) bucket =
    | Empty
    | Pair of 'a * 'b

  type ('a,'b) t = {
    eq : 'a equal;
    hash : 'a hash;
    arr : ('a,'b) bucket array;
    mutable c_size : int;
  }

  let make eq hash size =
    assert (size>0);
    {arr=Array.make size Empty; eq; hash; c_size=0 }

  let clear c =
    c.c_size <- 0;
    Array.fill c.arr 0 (Array.length c.arr) Empty

  let get c x =
    let i = c.hash x mod Array.length c.arr in
    match c.arr.(i) with
    | Pair (x', y) when c.eq x x' -> y
    | Pair _
    | Empty -> raise Not_found

  let set c x y =
    let i = c.hash x mod Array.length c.arr in
    if c.arr.(i) = Empty then c.c_size <- c.c_size + 1;
    c.arr.(i) <- Pair (x,y)

  let iter c f =
    Array.iter (function Empty -> () | Pair (x,y) -> f x y) c.arr

  let size c () = c.c_size
end

let replacing ?(eq=default_eq_) ?(hash=default_hash_) size =
  let c = Replacing.make eq hash size in
  { get=(fun x -> Replacing.get c x);
    set=(fun x y -> Replacing.set c x y);
    clear=(fun () -> Replacing.clear c);
    size=Replacing.size c;
    iter=Replacing.iter c;
  }
