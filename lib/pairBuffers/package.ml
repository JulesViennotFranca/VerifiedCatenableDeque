module Core = MakeCore.Make(Deque.Package)
open Core

module Base = struct

  type 'a t = { core : 'a cadeque ; length : int }

  let empty = { core = Core.empty ; length = 0 }

  let is_empty t = t.length = 0

  let push x { core ; length } =
    { core = Core.push x core ; length = length + 1 }

  let inject { core ; length } x =
    { core = Core.inject core x ; length = length + 1 }

  let pop { core ; length } =
    match Core.pop core with
    | None -> None
    | Some (x, core) -> Some (x, { core ; length = length - 1 })

  let eject { core ; length } =
    match Core.eject core with
    | None -> None
    | Some (core, x) -> Some ({ core ; length = length - 1 }, x)

  let fold_left_cadeque
  : type a z. (z -> a -> z) -> z -> a cadeque -> z
  = fun f z (T c) ->

    let fold_left_buffer = Buffer.fold_left in

    let fold_left_node
    : type a nc k c fol. (z -> fol -> z) -> fol ->
                         (z -> a -> z) -> z -> (a, nc, k, c) node -> z
    = fun fold_left follow f z n ->
      match n with
      | Only_end b -> fold_left_buffer f z b
      | Only (_, p, s) ->
        let z = fold_left_buffer f z p in
        let z = fold_left z follow in
        fold_left_buffer f z s
      | Left (_, p, a, b) ->
        let z = fold_left_buffer f z p in
        let z = fold_left z follow in
        f (f z a) b
      | Right (_, a, b, s) ->
        let z = f (f z a) b in
        let z = fold_left z follow in
        fold_left_buffer f z s
    in

    let rec fold_left_stored_triple
    : type a. (z -> a -> z) -> z -> a stored_triple -> z
    = fun f z st ->
      match st with
      | Small b -> fold_left_buffer f z b
      | Big (p, child, s) ->
        let z = fold_left_buffer f z p in
        let z = fold_left_chain (fold_left_stored_triple f) z child in
        fold_left_buffer f z s

    and fold_left_packet
    : type a b nc k c cl cr.
         (z -> a -> z)
      -> z
      -> (a, b, nc, k, c) packet * (b, nc, only, cl, cr) chain
      -> z
    = fun f z (Packet (bd, tl), c) ->
      match bd with
      | Hole ->
        fold_left_node
          (fold_left_chain (fold_left_stored_triple f)) c
          f z tl
      | Single_child (hd, bd) ->
        fold_left_node
          (fold_left_packet (fold_left_stored_triple f)) (Packet (bd, tl), c)
          f z hd
      | Pair_yellow (hd, bd, cr) ->
        let local_fold_left z (pkt, c, cr) =
          let z = fold_left_packet (fold_left_stored_triple f) z (pkt, c) in
          fold_left_chain (fold_left_stored_triple f) z cr
        in
        fold_left_node local_fold_left (Packet (bd, tl), c, cr) f z hd
      | Pair_orange (hd, cl, bd) ->
        let local_fold_left z (cl, pkt, c) =
          let z = fold_left_chain (fold_left_stored_triple f) z cl in
          fold_left_packet (fold_left_stored_triple f) z (pkt, c)
        in
        fold_left_node local_fold_left (cl, Packet (bd, tl), c) f z hd

    and fold_left_chain
    : type a ck nk cl cr. (z -> a -> z) -> z -> (a, ck, nk, cl, cr) chain -> z
    = fun f z c ->
      match c with
      | Empty -> z
      | Single (_, pkt, c) -> fold_left_packet f z (pkt, c)
      | Pair (cl, cr) ->
        let z = fold_left_chain f z cl in
        fold_left_chain f z cr
    in

    fold_left_chain f z c

  let fold_right_cadeque
  : type a z. (a -> z -> z) -> a cadeque -> z -> z
  = fun f (T c) z ->

    let fold_right_buffer = Buffer.fold_right in

    let fold_right_node
    : type a nc k c fol. (fol -> z -> z) -> fol ->
                         (a -> z -> z) -> (a, nc, k, c) node -> z -> z
    = fun fold_right follow f n z ->
      match n with
      | Only_end b -> fold_right_buffer f b z
      | Only (_, p, s) ->
        let z = fold_right_buffer f s z in
        let z = fold_right follow z in
        fold_right_buffer f p z
      | Left (_, p, a, b) ->
        let z = f a (f b z) in
        let z = fold_right follow z in
        fold_right_buffer f p z
      | Right (_, a, b, s) ->
        let z = fold_right_buffer f s z in
        let z = fold_right follow z in
        f a (f b z)
    in

    let rec fold_right_stored_triple
    : type a. (a -> z -> z) -> a stored_triple -> z -> z
    = fun f st z ->
      match st with
      | Small b -> fold_right_buffer f b z
      | Big (p, child, s) ->
        let z = fold_right_buffer f s z in
        let z = fold_right_chain (fold_right_stored_triple f) child z in
        fold_right_buffer f p z

    and fold_right_packet
    : type a b nc k c cl cr.
         (a -> z -> z)
      -> (a, b, nc, k, c) packet * (b, nc, only, cl, cr) chain
      -> z
      -> z
    = fun f (Packet (bd, tl), c) z ->
      match bd with
      | Hole ->
        fold_right_node
          (fold_right_chain (fold_right_stored_triple f)) c
          f tl z
      | Single_child (hd, bd) ->
        fold_right_node
          (fold_right_packet (fold_right_stored_triple f)) (Packet (bd, tl), c)
          f hd z
      | Pair_yellow (hd, bd, cr) ->
        let local_fold_right (pkt, c, cr) z =
          let z = fold_right_chain (fold_right_stored_triple f) cr z in
          fold_right_packet (fold_right_stored_triple f) (pkt, c) z
        in
        fold_right_node local_fold_right (Packet (bd, tl), c, cr) f hd z
      | Pair_orange (hd, cl, bd) ->
        let local_fold_right (cl, pkt, c) z =
          let z = fold_right_packet (fold_right_stored_triple f) (pkt, c) z in
          fold_right_chain (fold_right_stored_triple f) cl z
        in
        fold_right_node local_fold_right (cl, Packet (bd, tl), c) f hd z

    and fold_right_chain
    : type a ck nk cl cr. (a -> z -> z) -> (a, ck, nk, cl, cr) chain -> z -> z
    = fun f c z ->
      match c with
      | Empty -> z
      | Single (_, pkt, c) -> fold_right_packet f (pkt, c) z
      | Pair (cl, cr) ->
        let z = fold_right_chain f cr z in
        fold_right_chain f cl z
    in

    fold_right_chain f c z

  let fold_left f z t = fold_left_cadeque f z t.core

  let fold_right f t z = fold_right_cadeque f t.core z

  let rev t = fold_left (fun t x -> push x t) empty t

  let append t1 t2 =
    { core = Core.concat t1.core t2.core ; length = t1.length + t2.length }

  let length t = t.length

  let name = "Cadeque"
end

include Base
include ListLike.OfDeque(Base)

let make n a = match Buffer.make n a with
  | Exact_0 -> { core = T Empty ; length = 0 }
  | At_least_1 b ->
    let core = T (Single (G, Packet (Hole, Only_end b), Empty)) in
    { core ; length = n }

let singleton x = push x empty
