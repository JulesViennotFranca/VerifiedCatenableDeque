(* This code is borrowed from Jean-Christophe Filliâtre, author of the PQueue
   module in OCaml's standard library, version 5.3. *)

(* A priority queue is implemented as a "min heap" data structure, that is, as
   a binary tree, stored inside a vector. *)

module[@inline] Make
(E: sig
  type t
  val compare: t -> t -> int
end)
(V : sig
  type element = E.t
  type vector
  val create : unit -> vector
  val length : vector -> int
  val is_empty : vector -> bool
  val clear : vector -> unit
  val get : vector -> int -> element
  val set : vector -> int -> element -> unit
  val push : vector -> element -> unit
  val pop : vector -> element
  val iter : (element -> unit) -> vector -> unit
end)
= struct

type element =
  E.t

type queue =
  V.vector

type t =
  queue

let create =
  V.create

let length =
  V.length

let is_empty =
  V.is_empty

let clear =
  V.clear

(* The node at index [i] has children nodes at indices [2 * i + 1] and
   [2 * i + 2] -- provided these are valid indices into the vector. *)

let[@inline] left_child i = 2 * i + 1
let[@inline] right_child i = 2 * i + 2
let[@inline] parent_node i = (i - 1) / 2

(* A heap respects the "heap ordering" if the value of each node is no greater
   than the value of its children. The algorithm manipulates arrays that
   respect the heap ordering, except for one node whose value may be too small
   or too large. The auxiliary functions [sift_up] and [sift_down] move such a
   misplaced value "up" or "down" until the heap ordering is restored. *)

(* [sift_up h i x] stores [x] at index [i], moving it up if necessary. *)

let rec sift_up h i x =
  if i = 0 then V.set h 0 x else
  let p = parent_node i in
  let y = V.get h p in
  if E.compare x y < 0 then (
    V.set h i y;
    sift_up h p x
  ) else
    V.set h i x

let add h x =
  let i = V.length h in
  V.push h x;
  if i > 0 then sift_up h i x

let add_list h xs =
  List.iter (add h) xs

exception Empty

let peek h =
  if V.is_empty h then raise Empty;
  V.get h 0

(* [sift_down h ~len i x] stores [x] at index [i],
   moving it down if necessary.
   [len] is the length of the vector. *)

let rec sift_down h ~len i x =
  let left = left_child i in
  if left >= len then V.set h i x (* no child, stop *) else
  let smallest =
    let right = right_child i in
    if right >= len then left (* no right child *) else
    if E.compare (V.get h left) (V.get h right) < 0 then left else right
  in
  let y = V.get h smallest in
  if E.compare y x < 0 then (
    V.set h i y;
    sift_down h ~len smallest x
  ) else
    V.set h i x

let extract h =
  let n = V.length h in
  if n = 0 then raise Empty;
  let x = V.pop h in
  if n = 1 then x else (
    let r = V.get h 0 in
    sift_down h ~len:(n - 1) 0 x;
    r
  )

let iter =
  V.iter

exception Exists

let exists p q =
  try
    iter (fun x -> if p x then raise Exists) q;
    false
  with Exists ->
    true

end
