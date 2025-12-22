(**This module offers an implementation of priority queues. It is
   parameterized over a totally ordered type of elements [E] and
   over an implementation [V] of vectors for this type of elements. *)
module Make
(E: sig
  type t
  val compare: t -> t -> int
end)
(_ : sig
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
: sig

  (** The type of priority queues. *)
  type queue
  type t = queue

  (** The type of elements. *)
  type element = E.t

  (**[create()] returns a fresh empty priority queue.*)
  val create : unit -> queue

  (**[length q] returns the current number of elements of the queue [q]. *)
  val length : queue -> int

  (**[is_empty q] is equivalent to [length q = 0]. *)
  val is_empty : queue -> bool

  (**[add q x] inserts the element [x] into the queue [q]. *)
  val add : queue -> element -> unit

  (**[add_list q xs] inserts the elements of the list [xs] into
     the queue [q]. *)
  val add_list : queue -> element list -> unit

  exception Empty

  (** [extract q] extracts and returns an element of the queue [q] that has
      minimum priority. If the queue is empty, {!Empty} is raised. *)
  val extract : queue -> element

  (**[peek q] returns an element of the queue [q] that has minimum priority.
     This element is not extracted out of the queue. If the queue is empty,
     {!Empty} is raised. *)
  val peek : queue -> element

  (**[clear q] discards all elements of the queue [q]. *)
  val clear : queue -> unit

  (**[iter f q] applies the function [f] successively to each element
     of the queue [q], in an unspecified order. *)
  val iter : (element -> unit) -> queue -> unit

  (**[exists p q] returns [true] if and only if the queue [q] contains
     an element [x] such that [p x] is [true]. *)
  val exists : (element -> bool) -> queue -> bool

end
