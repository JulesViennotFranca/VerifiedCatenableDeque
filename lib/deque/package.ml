open Core

module Base = struct

  type 'a t = { core : 'a deque ; length : int ; rev : bool }

  let empty = { core = Core.empty ; length = 0 ; rev = false }

  let is_empty t = t.length = 0

  let push x { core ; length ; rev } =
    if rev
      then { core = Core.inject core x ; length = length + 1 ; rev }
      else { core = Core.push   x core ; length = length + 1 ; rev }

  let pop { core ; length ; rev } =
    if rev
      then match Core.eject core with
        | None -> None
        | Some (core, x) -> Some (x, { core ; length = length - 1 ; rev })
      else match Core.pop core with
        | None -> None
        | Some (x, core) -> Some (x, { core ; length = length - 1 ; rev })

  let inject { core ; length ; rev } x =
    if rev
      then { core = Core.push   x core ; length = length + 1 ; rev }
      else { core = Core.inject core x ; length = length + 1 ; rev }

  let eject { core ; length ; rev } =
    if rev
      then match Core.pop core with
        | None -> None
        | Some (x, core) -> Some ({ core ; length = length - 1 ; rev }, x)
      else match Core.eject core with
        | None -> None
        | Some (core, x) -> Some ({ core ; length = length - 1 ; rev }, x)

  let fold_left_deque
  : type a z. (z -> a -> z) -> z -> a deque -> z
  = fun f z (T c) ->

    let fold_left_buffer
    : type a c. (z -> a -> z) -> z -> (a, c) buffer -> z
    = fun f z b ->
      match b with
      | B0 -> z
      | B1 a -> f z a
      | B2 (a, b) -> f (f z a) b
      | B3 (a, b, c) -> f (f (f z a) b) c
      | B4 (a, b, c, d) -> f (f (f (f z a) b) c) d
      | B5 (a, b, c, d, e) -> f (f (f (f (f z a) b) c) d) e
    in

    let fold_pair f z (a, b) = f (f z a) b in

    let rec fold_left_packet
    : type a b c1 c2.
      (z -> a -> z) -> z -> (a, b, c1) packet -> (b, c2) chain -> z
    = fun f z pkt c ->
      match pkt with
      | Hole -> fold_left_chain f z c
      | Packet (p, pkt, s) ->
        let z = fold_left_buffer f z p in
        let z = fold_left_packet (fold_pair f) z pkt c in
        fold_left_buffer f z s

    and fold_left_chain
    : type a c. (z -> a -> z) -> z -> (a, c) chain -> z
    = fun f z c ->
      match c with
      | Ending b -> fold_left_buffer f z b
      | Chain (_, pkt, c) -> fold_left_packet f z pkt c
    in

    fold_left_chain f z c

  let fold_right_deque
  : type a z. (a -> z -> z) -> a deque -> z -> z
  = fun f (T c) z ->

    let fold_right_buffer
    : type a c. (a -> z -> z) -> (a, c) buffer -> z -> z
    = fun f b z ->
      match b with
      | B0 -> z
      | B1 a -> f a z
      | B2 (a, b) -> f a (f b z)
      | B3 (a, b, c) -> f a (f b (f c z))
      | B4 (a, b, c, d) -> f a (f b (f c (f d z)))
      | B5 (a, b, c, d, e) -> f a (f b (f c (f d (f e z))))
    in

    let fold_pair f (a, b) z = f a (f b z) in

    let rec fold_right_packet
    : type a b c1 c2.
      (a -> z -> z) -> (a, b, c1) packet -> (b, c2) chain -> z -> z
    = fun f pkt c z ->
      match pkt with
      | Hole -> fold_right_chain f c z
      | Packet (p, pkt, s) ->
        let z = fold_right_buffer f s z in
        let z = fold_right_packet (fold_pair f) pkt c z in
        fold_right_buffer f p z

    and fold_right_chain
    : type a c. (a -> z -> z) -> (a, c) chain -> z -> z
    = fun f c z ->
      match c with
      | Ending b -> fold_right_buffer f b z
      | Chain (_, pkt, c) -> fold_right_packet f pkt c z
    in

    fold_right_chain f c z

  let fold_left f z t =
    if t.rev
      then fold_right_deque (Fun.flip f) t.core z
      else fold_left_deque f z t.core

  let fold_right f t z =
    if t.rev
      then fold_left_deque (Fun.flip f) z t.core
      else fold_right_deque f t.core z

  let rev t = { t with rev = not t.rev }

  let append t1 t2 =
    if t1.length <= t2.length
      then fold_right push t1 t2
      else fold_left inject t1 t2

  let length t = t.length

  let name = "Deque"

end

include Base
include ListLike.OfDeque(Base)

let nth_deque
: type a. a deque -> int -> int -> a
= fun (T c) i j ->

  let buffer_length : type a c. (a, c) buffer -> int = function
    | B0 -> 0 | B1 _ -> 1 | B2 _ -> 2 | B3 _ -> 3 | B4 _ -> 4 | B5 _ -> 5
  in

  let list_of_buffer : type a c. (a, c) buffer -> a list = function
  | B0 -> []
  | B1 a -> [a]
  | B2 (a, b) -> [a; b]
  | B3 (a, b, c) -> [a; b; c]
  | B4 (a, b, c, d) -> [a; b; c; d]
  | B5 (a, b, c, d, e) -> [a; b; c; d; e]
  in

  let nth_buffer
  : type a c. (a, c) buffer -> int -> a
  = fun b i -> List.nth (list_of_buffer b) i
  in

  let nth_pair
  : type a b. (b -> int -> int -> a) -> b * b -> int -> int -> a
  = fun nth_elm (b1, b2) lvl i ->
    let pred_lvl = lvl / 2 in
    if i < pred_lvl
      then nth_elm b1 pred_lvl i
      else nth_elm b2 pred_lvl (i - pred_lvl)
  in

  let rec nth_packet
  : type a b d c1 c2.
    (b -> int -> int -> a) ->
    (b, d, c1) packet -> (d, c2) chain -> int -> int -> int -> a
  = fun nth_elm pkt c lvl i j ->
    match pkt with
    | Hole -> nth_chain nth_elm c lvl i j
    | Packet (p, pkt, s) ->
      if i < lvl * buffer_length p then begin
        let k = i - lvl * (i / lvl) in
        nth_elm (nth_buffer p (i / lvl)) lvl k end
      else if j < lvl * buffer_length s then begin
        let k = lvl + lvl * (j / lvl) - j - 1 in
        nth_elm (nth_buffer s (buffer_length s - 1 - j / lvl)) lvl k end
      else
        let i = i - lvl * buffer_length p in
        let j = j - lvl * buffer_length s in
        let lvl = 2 * lvl in
        nth_packet (nth_pair nth_elm) pkt c lvl i j

  and nth_chain
  : type a b c. (b -> int -> int -> a) -> (b, c) chain -> int -> int -> int -> a
  = fun nth_elm c lvl i j ->
    match c with
    | Ending b ->
      let k = i - lvl * (i / lvl) in
      nth_elm (nth_buffer b (i / lvl)) lvl k
    | Chain (_, pkt, c) -> nth_packet nth_elm pkt c lvl i j
  in

  nth_chain (fun a _ _ -> a) c 1 i j

let nth t i =
  if i < 0 then invalid_arg "Deque.nth" ;
  let j = length t - i - 1 in
  if j < 0 then failwith "Deque.nth" ;
  if t.rev
    then nth_deque t.core j i
    else nth_deque t.core i j

let nth_opt t i =
  try Some (nth t i) with Failure _ -> None

let rec make_chain
: type a. int -> a -> (a, Color.GYR.green) chain
= fun n a ->
  match n with
  | 0 -> Ending B0
  | 1 -> Ending (B1 a)
  | 2 -> Ending (B2 (a, a))
  | 3 -> Ending (B3 (a, a, a))
  | 4 -> Ending (B4 (a, a, a, a))
  | _ ->
    let n = n - 5 in
    let rest = make_chain (n / 2) (a, a) in
    if Int.equal (n mod 2) 0 then
      Chain (G, Packet (B3 (a, a, a), Hole, B2 (a, a)), rest)
    else
      Chain (G, Packet (B3 (a, a, a), Hole, B3 (a, a, a)), rest)

let make n a = { core = T (make_chain n a) ; length = n ; rev = false }

let rec chain_of_list
: type a. a list -> (a, Color.GYR.green) chain
= function
  | [] -> Ending B0
  | [a] -> Ending (B1 a)
  | [a; b] -> Ending (B2 (a, b))
  | [a; b; c] -> Ending (B3 (a, b, c))
  | [a; b; c; d] -> Ending (B4 (a, b, c, d))
  | a :: b :: c :: d :: e :: l ->
    let p = B3 (a, b, c) in
    let rec next_list acc = function
      | d, e, [] -> List.rev acc, B2 (d, e)
      | d, e, [f] -> List.rev acc, B3 (d, e, f)
      | d, e, f :: g :: l -> next_list ((d, e) :: acc) (f, g, l)
    in
    let l, s = next_list [] (d, e, l) in
    Chain (G, Packet (p, Hole, s), chain_of_list l)

let of_list l =
  { core = T (chain_of_list l) ; length = List.length l ; rev = false }

let singleton x =
  { core = T (Ending (B1 x)) ; length = 1 ; rev = false }