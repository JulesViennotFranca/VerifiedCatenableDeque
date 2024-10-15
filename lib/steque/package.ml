module Core = MakeCore.Make(Deque.Package)
open Core

module Base = struct

  type 'a t = { core : 'a steque ; length : int }

  let empty = { core = Core.empty ; length = 0 }

  let is_empty t = t.length = 0

  let push x t = { core = Core.push x t.core ; length = t.length + 1 }

  let inject t x = { core = Core.inject t.core x ; length = t.length + 1}

  let pop { core ; length } = match Core.pop core with
    | None -> None
    | Some (x, core) -> Some (x, { core ; length = length - 1 })

  let fold_left_steque
  : type a z. (z -> a -> z) -> z -> a steque -> z
  = fun f z (T c) ->

    let fold_left_prefix
    : type a c. (z -> a -> z) -> z -> (a, c) prefix -> z
    = fun f z p ->
      match p with
      | P2 (a, b) -> f (f z a) b
      | P3 (a, b, c) -> f (f (f z a) b) c
      | P4 (a, b, c, d, e) ->
        Deque.Package.fold_left f (f (f (f (f z a) b) c) d) e
    in

    let fold_left_suffix = Deque.Package.fold_left in

    let rec fold_left_pair
    : type a. (z -> a -> z) -> z -> a pair -> z
    = fun f z (Pair (p, c)) ->
      let z = fold_left_prefix f z p in
      fold_left_chain (fold_left_pair f) z c

    and fold_left_packet
    : type a b c1 c2.
      (z -> a -> z) -> z -> (a, b, c1) packet * (b, c2) chain -> z
    = fun f z (pkt, c) ->
      match pkt with
      | Hole -> fold_left_chain f z c
      | Packet (p, pkt, s) ->
        let z = fold_left_prefix f z p in
        let z = fold_left_packet (fold_left_pair f) z (pkt, c) in
        fold_left_suffix f z s

    and fold_left_chain
    : type a c. (z -> a -> z) -> z -> (a, c) chain -> z
    = fun f z c ->
      match c with
      | Ending d -> Deque.Package.fold_left f z d
      | Chain (_, pkt, c) -> fold_left_packet f z (pkt, c)
    in

    fold_left_chain f z c

  let fold_right_steque
  : type a z. (a -> z -> z) -> a steque -> z -> z
  = fun f (T c) z ->

    let fold_right_prefix
    : type a c. (a -> z -> z) -> (a, c) prefix -> z -> z
    = fun f p z ->
      match p with
      | P2 (a, b) -> f a (f b z)
      | P3 (a, b, c) -> f a (f b (f c z))
      | P4 (a, b, c, d, e) ->
        f a (f b (f c (f d (Deque.Package.fold_right f e z))))
    in

    let fold_right_suffix = Deque.Package.fold_right in

    let rec fold_right_pair
    : type a. (a -> z -> z) -> a pair -> z -> z
    = fun f (Pair (p, c)) z ->
      let z = fold_right_chain (fold_right_pair f) c z in
      fold_right_prefix f p z

    and fold_right_packet
    : type a b c1 c2.
      (a -> z -> z) -> (a, b, c1) packet * (b, c2) chain -> z -> z
    = fun f (pkt, c) z ->
      match pkt with
      | Hole -> fold_right_chain f c z
      | Packet (p, pkt, s) ->
        let z = fold_right_suffix f s z in
        let z = fold_right_packet (fold_right_pair f) (pkt, c) z in
        fold_right_prefix f p z

    and fold_right_chain
    : type a c. (a -> z -> z) -> (a, c) chain -> z -> z
    = fun f c z ->
      match c with
      | Ending d -> Deque.Package.fold_right f d z
      | Chain (_, pkt, c) -> fold_right_packet f (pkt, c) z
    in

    fold_right_chain f c z

  let fold_left f z t = fold_left_steque f z t.core

  let fold_right f t z = fold_right_steque f t.core z

  let rev t = fold_left (fun t x -> push x t) empty t

  let append t1 t2 =
    { core = Core.concat t1.core t2.core ; length = t1.length + t2.length }

  let length t = t.length

  let name = "Steque"

end

include Base
include ListLike.OfSteque(Base)

let make n a = { core = T (Ending (Deque.Package.make n a)) ; length = n }

let singleton x = { core = T (Ending (Deque.Package.singleton x)) ; length = 1 }