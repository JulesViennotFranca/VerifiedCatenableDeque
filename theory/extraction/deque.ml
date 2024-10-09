open Datatypes
open GYR
open Nat

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type 'a prodN =
| Coq_prodZ of 'a
| Coq_prodS of nat * 'a prodN * 'a prodN

type 'a optionN =
| NoneN
| SomeN of 'a prodN

type 'a buffer =
| B0
| B1 of yellow_hue * red_hue * 'a prodN
| B2 of green_hue * yellow_hue * red_hue * 'a prodN * 'a prodN
| B3 of green_hue * yellow_hue * red_hue * 'a prodN * 'a prodN * 'a prodN
| B4 of yellow_hue * red_hue * 'a prodN * 'a prodN * 'a prodN * 'a prodN
| B5 of 'a prodN * 'a prodN * 'a prodN * 'a prodN * 'a prodN

type 'a packet =
| Hole of nat
| Packet of nat * nat * nat * nat * nat * color * yellow_hue * 'a buffer
   * 'a packet * 'a buffer

type regularity =
| G of green_hue * red_hue
| Y
| R

type 'a chain =
| Ending of nat * color * 'a buffer
| Chain of nat * nat * nat * color * color * regularity * 'a packet * 'a chain

type 'a decompose =
| Underflow of nat * 'a optionN
| Ok of nat * 'a buffer
| Overflow of nat * 'a buffer * 'a prodN

type 'a sandwich =
| Alone of nat * 'a optionN
| Sandwich of nat * color * 'a prodN * 'a buffer * 'a prodN

type 'a deque =
| T of green_hue * yellow_hue * 'a chain

(** val concat_map_prodN_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> 'a1 prodN -> 'a2 list **)

let concat_map_prodN_seq f lvlt lvl =
  let local =
    let rec local _ = function
    | Coq_prodZ ta -> f __ lvlt ta
    | Coq_prodS (n, p1, p2) -> app (local n p1) (local n p2)
    in local
  in
  (Obj.magic f:_ -> _ -> _ -> _) local lvl (* # mod # -> added f:_ -> _ -> _ -> _ *)

(** val concat_map_buffer_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> color -> 'a1 buffer
    -> 'a2 list **)

let concat_map_buffer_seq f lvlt lvl _ _ = function
| B0 -> Coq_nil
| B1 (_, _, a) -> concat_map_prodN_seq f lvlt lvl a
| B2 (_, _, _, a, b0) ->
  app (concat_map_prodN_seq f lvlt lvl a) (concat_map_prodN_seq f lvlt lvl b0)
| B3 (_, _, _, a, b0, c) ->
  app (concat_map_prodN_seq f lvlt lvl a)
    (app (concat_map_prodN_seq f lvlt lvl b0)
      (concat_map_prodN_seq f lvlt lvl c))
| B4 (_, _, a, b0, c, d) ->
  app (concat_map_prodN_seq f lvlt lvl a)
    (app (concat_map_prodN_seq f lvlt lvl b0)
      (app (concat_map_prodN_seq f lvlt lvl c)
        (concat_map_prodN_seq f lvlt lvl d)))
| B5 (a, b0, c, d, e) ->
  app (concat_map_prodN_seq f lvlt lvl a)
    (app (concat_map_prodN_seq f lvlt lvl b0)
      (app (concat_map_prodN_seq f lvlt lvl c)
        (app (concat_map_prodN_seq f lvlt lvl d)
          (concat_map_prodN_seq f lvlt lvl e))))

(** val concat_map_packet_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> nat -> nat -> color
    -> 'a1 packet -> 'a2 list -> 'a2 list **)

let rec concat_map_packet_seq f lvlt lvl _ _ _ _ pkt l =
  match pkt with
  | Hole _ -> l
  | Packet (hlvl0, psize, pktsize0, ssize, hsize0, c0, y, p, pkt0, s) ->
    app (concat_map_buffer_seq f lvlt lvl psize c0 p)
      (app
        (concat_map_packet_seq f lvlt (S lvl) hlvl0 pktsize0 hsize0 (Mix
          (NoGreen, y, NoRed)) pkt0 l)
        (concat_map_buffer_seq f lvlt lvl ssize c0 s))

(** val concat_map_chain_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nat -> color -> 'a1 chain
    -> 'a2 list **)

let rec concat_map_chain_seq f lvlt lvl _ _ = function
| Ending (size0, c0, b) -> concat_map_buffer_seq f lvlt lvl size0 c0 b
| Chain (hlvl, size0, hsize, c1, c2, _, pkt, c0) ->
  concat_map_packet_seq f lvlt lvl hlvl size0 hsize c1 pkt
    (concat_map_chain_seq f lvlt hlvl hsize c2 c0)

(** val concat_map_deque_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> 'a1 deque -> 'a2 list **)

let concat_map_deque_seq f lvlt size = function
| T (g, y, c) -> concat_map_chain_seq f lvlt O size (Mix (g, y, NoRed)) c

(** val empty : 'a1 deque **)

let empty =
  T (SomeGreen, NoYellow, (Ending (O, (Mix (NoGreen, NoYellow, SomeRed)),
    B0)))

(** val green_push : nat -> nat -> 'a1 prodN -> 'a1 buffer -> 'a1 buffer **)

let green_push _ _ x = function
| B2 (_, _, _, p, p0) -> B3 (NoGreen, SomeYellow, NoRed, x, p, p0)
| B3 (_, _, _, p, p0, p1) -> B4 (SomeYellow, NoRed, x, p, p0, p1)
| _ -> assert false (* absurd case *)

(** val green_inject : nat -> nat -> 'a1 buffer -> 'a1 prodN -> 'a1 buffer **)

let green_inject _ _ b x =
  match b with
  | B2 (_, _, _, p, p0) -> B3 (NoGreen, SomeYellow, NoRed, p, p0, x)
  | B3 (_, _, _, p, p0, p1) -> B4 (SomeYellow, NoRed, p, p0, p1, x)
  | _ -> assert false (* absurd case *)

(** val green_pop :
    nat -> nat -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod **)

let green_pop _ _ = function
| B2 (_, _, _, p, p0) -> Coq_pair (p, (B1 (SomeYellow, NoRed, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair (p, (B2 (NoGreen, SomeYellow, NoRed, p0, p1)))
| _ -> assert false (* absurd case *)

(** val green_eject :
    nat -> nat -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod **)

let green_eject _ _ = function
| B2 (_, _, _, p, p0) -> Coq_pair ((B1 (SomeYellow, NoRed, p)), p0)
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair ((B2 (NoGreen, SomeYellow, NoRed, p, p0)), p1)
| _ -> assert false (* absurd case *)

(** val yellow_push : nat -> nat -> 'a1 prodN -> 'a1 buffer -> 'a1 buffer **)

let yellow_push _ _ x = function
| B1 (_, _, p) -> B2 (NoGreen, NoYellow, SomeRed, x, p)
| B2 (_, _, _, p, p0) -> B3 (NoGreen, NoYellow, SomeRed, x, p, p0)
| B3 (_, _, _, p, p0, p1) -> B4 (NoYellow, SomeRed, x, p, p0, p1)
| B4 (_, _, p, p0, p1, p2) -> B5 (x, p, p0, p1, p2)
| _ -> assert false (* absurd case *)

(** val yellow_inject :
    nat -> nat -> 'a1 buffer -> 'a1 prodN -> 'a1 buffer **)

let yellow_inject _ _ b x =
  match b with
  | B1 (_, _, p) -> B2 (NoGreen, NoYellow, SomeRed, p, x)
  | B2 (_, _, _, p, p0) -> B3 (NoGreen, NoYellow, SomeRed, p, p0, x)
  | B3 (_, _, _, p, p0, p1) -> B4 (NoYellow, SomeRed, p, p0, p1, x)
  | B4 (_, _, p, p0, p1, p2) -> B5 (p, p0, p1, p2, x)
  | _ -> assert false (* absurd case *)

(** val yellow_pop :
    nat -> nat -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod **)

let yellow_pop _ _ = function
| B1 (_, _, p) -> Coq_pair (p, B0)
| B2 (_, _, _, p, p0) -> Coq_pair (p, (B1 (NoYellow, SomeRed, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair (p, (B2 (NoGreen, NoYellow, SomeRed, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Coq_pair (p, (B3 (NoGreen, NoYellow, SomeRed, p0, p1, p2)))
| _ -> assert false (* absurd case *)

(** val yellow_eject :
    nat -> nat -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod **)

let yellow_eject _ _ = function
| B1 (_, _, p) -> Coq_pair (B0, p)
| B2 (_, _, _, p, p0) -> Coq_pair ((B1 (NoYellow, SomeRed, p)), p0)
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair ((B2 (NoGreen, NoYellow, SomeRed, p, p0)), p1)
| B4 (_, _, p, p0, p1, p2) ->
  Coq_pair ((B3 (NoGreen, NoYellow, SomeRed, p, p0, p1)), p2)
| _ -> assert false (* absurd case *)

(** val buffer_push_obligations_obligation_1 : yellow_hue **)

let buffer_push_obligations_obligation_1 =
  SomeYellow

(** val buffer_push_obligations_obligation_2 : red_hue **)

let buffer_push_obligations_obligation_2 =
  SomeRed

(** val buffer_push_obligations_obligation_4 : green_hue **)

let buffer_push_obligations_obligation_4 =
  SomeGreen

(** val buffer_push_obligations_obligation_5 : yellow_hue -> yellow_hue **)

let buffer_push_obligations_obligation_5 y =
  y

(** val buffer_push_obligations_obligation_6 : red_hue -> red_hue **)

let buffer_push_obligations_obligation_6 r =
  r

(** val buffer_push_obligations_obligation_8 : green_hue -> green_hue **)

let buffer_push_obligations_obligation_8 g =
  g

(** val buffer_push_obligations_obligation_9 : yellow_hue -> yellow_hue **)

let buffer_push_obligations_obligation_9 y0 =
  y0

(** val buffer_push_obligations_obligation_10 : red_hue -> red_hue **)

let buffer_push_obligations_obligation_10 r0 =
  r0

(** val buffer_push_obligations_obligation_12 : yellow_hue -> yellow_hue **)

let buffer_push_obligations_obligation_12 y1 =
  y1

(** val buffer_push_obligations_obligation_13 : red_hue -> red_hue **)

let buffer_push_obligations_obligation_13 r1 =
  r1

(** val buffer_push :
    nat -> nat -> color -> 'a1 prodN -> 'a1 buffer -> 'a1 chain **)

let buffer_push lvl _ _ x = function
| B0 ->
  Ending ((S O), (Mix (NoGreen, buffer_push_obligations_obligation_1,
    buffer_push_obligations_obligation_2)), (B1
    (buffer_push_obligations_obligation_1,
    buffer_push_obligations_obligation_2, x)))
| B1 (y, r, p) ->
  Ending ((S (S O)), (Mix (buffer_push_obligations_obligation_4,
    (buffer_push_obligations_obligation_5 y),
    (buffer_push_obligations_obligation_6 r))), (B2
    (buffer_push_obligations_obligation_4,
    (buffer_push_obligations_obligation_5 y),
    (buffer_push_obligations_obligation_6 r), x, p)))
| B2 (g, y, r, p, p0) ->
  Ending ((S (S (S O))), (Mix ((buffer_push_obligations_obligation_8 g),
    (buffer_push_obligations_obligation_9 y),
    (buffer_push_obligations_obligation_10 r))), (B3
    ((buffer_push_obligations_obligation_8 g),
    (buffer_push_obligations_obligation_9 y),
    (buffer_push_obligations_obligation_10 r), x, p, p0)))
| B3 (_, y, r, p, p0, p1) ->
  Ending ((S (S (S (S O)))), (Mix (NoGreen,
    (buffer_push_obligations_obligation_12 y),
    (buffer_push_obligations_obligation_13 r))), (B4
    ((buffer_push_obligations_obligation_12 y),
    (buffer_push_obligations_obligation_13 r), x, p, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Ending ((S (S (S (S (S O))))), (Mix (NoGreen, NoYellow, SomeRed)), (B5 (x,
    p, p0, p1, p2)))
| B5 (p, p0, p1, p2, p3) ->
  Chain ((S lvl), (add (add (add (S (S (S O))) O) O) (S (S (S O)))), O, (Mix
    (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)), (G
    (SomeGreen, NoRed)), (Packet ((S lvl), (S (S (S O))), O, (S (S (S O))),
    O, (Mix (SomeGreen, NoYellow, NoRed)), NoYellow, (B3 (SomeGreen,
    NoYellow, NoRed, x, p, p0)), (Hole O), (B3 (SomeGreen, NoYellow, NoRed,
    p1, p2, p3)))), (Ending (O, (Mix (NoGreen, NoYellow, SomeRed)), B0)))

(** val buffer_inject_obligations_obligation_1 : yellow_hue **)

let buffer_inject_obligations_obligation_1 =
  SomeYellow

(** val buffer_inject_obligations_obligation_2 : red_hue **)

let buffer_inject_obligations_obligation_2 =
  SomeRed

(** val buffer_inject_obligations_obligation_4 : green_hue **)

let buffer_inject_obligations_obligation_4 =
  SomeGreen

(** val buffer_inject_obligations_obligation_5 : yellow_hue -> yellow_hue **)

let buffer_inject_obligations_obligation_5 y =
  y

(** val buffer_inject_obligations_obligation_6 : red_hue -> red_hue **)

let buffer_inject_obligations_obligation_6 r =
  r

(** val buffer_inject_obligations_obligation_8 : green_hue -> green_hue **)

let buffer_inject_obligations_obligation_8 g =
  g

(** val buffer_inject_obligations_obligation_9 : yellow_hue -> yellow_hue **)

let buffer_inject_obligations_obligation_9 y0 =
  y0

(** val buffer_inject_obligations_obligation_10 : red_hue -> red_hue **)

let buffer_inject_obligations_obligation_10 r0 =
  r0

(** val buffer_inject_obligations_obligation_12 : yellow_hue -> yellow_hue **)

let buffer_inject_obligations_obligation_12 y1 =
  y1

(** val buffer_inject_obligations_obligation_13 : red_hue -> red_hue **)

let buffer_inject_obligations_obligation_13 r1 =
  r1

(** val buffer_inject :
    nat -> nat -> color -> 'a1 buffer -> 'a1 prodN -> 'a1 chain **)

let buffer_inject lvl _ _ b x =
  match b with
  | B0 ->
    Ending ((S O), (Mix (NoGreen, buffer_inject_obligations_obligation_1,
      buffer_inject_obligations_obligation_2)), (B1
      (buffer_inject_obligations_obligation_1,
      buffer_inject_obligations_obligation_2, x)))
  | B1 (y, r, p) ->
    Ending ((S (S O)), (Mix (buffer_inject_obligations_obligation_4,
      (buffer_inject_obligations_obligation_5 y),
      (buffer_inject_obligations_obligation_6 r))), (B2
      (buffer_inject_obligations_obligation_4,
      (buffer_inject_obligations_obligation_5 y),
      (buffer_inject_obligations_obligation_6 r), p, x)))
  | B2 (g, y, r, p, p0) ->
    Ending ((S (S (S O))), (Mix ((buffer_inject_obligations_obligation_8 g),
      (buffer_inject_obligations_obligation_9 y),
      (buffer_inject_obligations_obligation_10 r))), (B3
      ((buffer_inject_obligations_obligation_8 g),
      (buffer_inject_obligations_obligation_9 y),
      (buffer_inject_obligations_obligation_10 r), p, p0, x)))
  | B3 (_, y, r, p, p0, p1) ->
    Ending ((S (S (S (S O)))), (Mix (NoGreen,
      (buffer_inject_obligations_obligation_12 y),
      (buffer_inject_obligations_obligation_13 r))), (B4
      ((buffer_inject_obligations_obligation_12 y),
      (buffer_inject_obligations_obligation_13 r), p, p0, p1, x)))
  | B4 (_, _, p, p0, p1, p2) ->
    Ending ((S (S (S (S (S O))))), (Mix (NoGreen, NoYellow, SomeRed)), (B5
      (p, p0, p1, p2, x)))
  | B5 (p, p0, p1, p2, p3) ->
    Chain ((S lvl), (add (add (add (S (S (S O))) O) O) (S (S (S O)))), O,
      (Mix (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)),
      (G (SomeGreen, NoRed)), (Packet ((S lvl), (S (S (S O))), O, (S (S (S
      O))), O, (Mix (SomeGreen, NoYellow, NoRed)), NoYellow, (B3 (SomeGreen,
      NoYellow, NoRed, p, p0, p1)), (Hole O), (B3 (SomeGreen, NoYellow,
      NoRed, p2, p3, x)))), (Ending (O, (Mix (NoGreen, NoYellow, SomeRed)),
      B0)))

(** val buffer_pop :
    nat -> nat -> color -> 'a1 buffer -> ('a1 prodN, 'a1 buffer) prod option **)

let buffer_pop _ _ _ = function
| B0 -> None
| B1 (_, _, p) -> Some (Coq_pair (p, B0))
| B2 (_, _, _, p, p0) -> Some (Coq_pair (p, (B1 (NoYellow, SomeRed, p0))))
| B3 (_, _, _, p, p0, p1) ->
  Some (Coq_pair (p, (B2 (NoGreen, NoYellow, SomeRed, p0, p1))))
| B4 (_, _, p, p0, p1, p2) ->
  Some (Coq_pair (p, (B3 (NoGreen, NoYellow, SomeRed, p0, p1, p2))))
| B5 (p, p0, p1, p2, p3) ->
  Some (Coq_pair (p, (B4 (NoYellow, SomeRed, p0, p1, p2, p3))))

(** val buffer_eject :
    nat -> nat -> color -> 'a1 buffer -> ('a1 buffer, 'a1 prodN) prod option **)

let buffer_eject _ _ _ = function
| B0 -> None
| B1 (_, _, p) -> Some (Coq_pair (B0, p))
| B2 (_, _, _, p, p0) -> Some (Coq_pair ((B1 (NoYellow, SomeRed, p)), p0))
| B3 (_, _, _, p, p0, p1) ->
  Some (Coq_pair ((B2 (NoGreen, NoYellow, SomeRed, p, p0)), p1))
| B4 (_, _, p, p0, p1, p2) ->
  Some (Coq_pair ((B3 (NoGreen, NoYellow, SomeRed, p, p0, p1)), p2))
| B5 (p, p0, p1, p2, p3) ->
  Some (Coq_pair ((B4 (NoYellow, SomeRed, p, p0, p1, p2)), p3))

(** val prefix_rot :
    nat -> nat -> color -> 'a1 prodN -> 'a1 buffer -> ('a1 buffer, 'a1 prodN)
    prod **)

let prefix_rot _ _ _ x = function
| B0 -> Coq_pair (B0, x)
| B1 (y, r, p) -> Coq_pair ((B1 (y, r, x)), p)
| B2 (g, y, r, p, p0) -> Coq_pair ((B2 (g, y, r, x, p)), p0)
| B3 (g, y, r, p, p0, p1) -> Coq_pair ((B3 (g, y, r, x, p, p0)), p1)
| B4 (y, r, p, p0, p1, p2) -> Coq_pair ((B4 (y, r, x, p, p0, p1)), p2)
| B5 (p, p0, p1, p2, p3) -> Coq_pair ((B5 (x, p, p0, p1, p2)), p3)

(** val suffix_rot :
    nat -> nat -> color -> 'a1 buffer -> 'a1 prodN -> ('a1 prodN, 'a1 buffer)
    prod **)

let suffix_rot _ _ _ b y =
  match b with
  | B0 -> Coq_pair (y, B0)
  | B1 (y0, r, p) -> Coq_pair (p, (B1 (y0, r, y)))
  | B2 (g, y0, r, p, p0) -> Coq_pair (p, (B2 (g, y0, r, p0, y)))
  | B3 (g, y0, r, p, p0, p1) -> Coq_pair (p, (B3 (g, y0, r, p0, p1, y)))
  | B4 (y0, r, p, p0, p1, p2) -> Coq_pair (p, (B4 (y0, r, p0, p1, p2, y)))
  | B5 (p, p0, p1, p2, p3) -> Coq_pair (p, (B5 (p0, p1, p2, p3, y)))

(** val prefix23 : nat -> nat -> 'a1 optionN -> 'a1 prodN -> 'a1 buffer **)

let prefix23 _ _ o p =
  match o with
  | NoneN ->
    (match p with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p0, p1) -> B2 (SomeGreen, NoYellow, NoRed, p0, p1))
  | SomeN p0 ->
    (match p with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p1, p2) -> B3 (SomeGreen, NoYellow, NoRed, p0, p1, p2))

(** val suffix23 : nat -> nat -> 'a1 prodN -> 'a1 optionN -> 'a1 buffer **)

let suffix23 _ _ p o =
  match p with
  | Coq_prodZ _ -> assert false (* absurd case *)
  | Coq_prodS (_, p0, p1) ->
    (match o with
     | NoneN -> B2 (SomeGreen, NoYellow, NoRed, p0, p1)
     | SomeN p2 -> B3 (SomeGreen, NoYellow, NoRed, p0, p1, p2))

(** val suffix12 : nat -> nat -> 'a1 prodN -> 'a1 optionN -> 'a1 buffer **)

let suffix12 _ _ x = function
| NoneN -> B1 (SomeYellow, NoRed, x)
| SomeN p -> B2 (NoGreen, SomeYellow, NoRed, x, p)

(** val prefix_decompose :
    nat -> nat -> color -> 'a1 buffer -> 'a1 decompose **)

let prefix_decompose lvl _ _ = function
| B0 -> Underflow (O, NoneN)
| B1 (_, _, p) -> Underflow ((S O), (SomeN p))
| B2 (_, _, _, p, p0) ->
  Ok ((S (S O)), (B2 (SomeGreen, NoYellow, NoRed, p, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Ok ((S (S (S O))), (B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Overflow ((S (S O)), (B2 (SomeGreen, NoYellow, NoRed, p, p0)), (Coq_prodS
    (lvl, p1, p2)))
| B5 (p, p0, p1, p2, p3) ->
  Overflow ((S (S (S O))), (B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)),
    (Coq_prodS (lvl, p2, p3)))

(** val suffix_decompose :
    nat -> nat -> color -> 'a1 buffer -> 'a1 decompose **)

let suffix_decompose lvl _ _ = function
| B0 -> Underflow (O, NoneN)
| B1 (_, _, p) -> Underflow ((S O), (SomeN p))
| B2 (_, _, _, p, p0) ->
  Ok ((S (S O)), (B2 (SomeGreen, NoYellow, NoRed, p, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Ok ((S (S (S O))), (B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Overflow ((S (S O)), (B2 (SomeGreen, NoYellow, NoRed, p1, p2)), (Coq_prodS
    (lvl, p, p0)))
| B5 (p, p0, p1, p2, p3) ->
  Overflow ((S (S (S O))), (B3 (SomeGreen, NoYellow, NoRed, p1, p2, p3)),
    (Coq_prodS (lvl, p, p0)))

(** val buffer_unsandwich_obligations_obligation_4 :
    yellow_hue -> yellow_hue **)

let buffer_unsandwich_obligations_obligation_4 y1 =
  y1

(** val buffer_unsandwich_obligations_obligation_5 : red_hue -> red_hue **)

let buffer_unsandwich_obligations_obligation_5 r1 =
  r1

(** val buffer_unsandwich_obligations_obligation_7 : green_hue **)

let buffer_unsandwich_obligations_obligation_7 =
  SomeGreen

(** val buffer_unsandwich_obligations_obligation_8 :
    yellow_hue -> yellow_hue **)

let buffer_unsandwich_obligations_obligation_8 y2 =
  y2

(** val buffer_unsandwich_obligations_obligation_9 : red_hue -> red_hue **)

let buffer_unsandwich_obligations_obligation_9 r2 =
  r2

(** val buffer_unsandwich_obligations_obligation_11 : green_hue **)

let buffer_unsandwich_obligations_obligation_11 =
  SomeGreen

(** val buffer_unsandwich_obligations_obligation_12 : yellow_hue **)

let buffer_unsandwich_obligations_obligation_12 =
  SomeYellow

(** val buffer_unsandwich_obligations_obligation_13 : red_hue **)

let buffer_unsandwich_obligations_obligation_13 =
  SomeRed

(** val buffer_unsandwich :
    nat -> nat -> color -> 'a1 buffer -> 'a1 sandwich **)

let buffer_unsandwich _ _ _ = function
| B0 -> Alone (O, NoneN)
| B1 (_, _, p) -> Alone ((S O), (SomeN p))
| B2 (_, _, _, p, p0) ->
  Sandwich (O, (Mix (NoGreen, NoYellow, SomeRed)), p, B0, p0)
| B3 (_, y, r, p, p0, p1) ->
  Sandwich ((S O), (Mix (NoGreen,
    (buffer_unsandwich_obligations_obligation_4 y),
    (buffer_unsandwich_obligations_obligation_5 r))), p, (B1
    ((buffer_unsandwich_obligations_obligation_4 y),
    (buffer_unsandwich_obligations_obligation_5 r), p0)), p1)
| B4 (y, r, p, p0, p1, p2) ->
  Sandwich ((S (S O)), (Mix (buffer_unsandwich_obligations_obligation_7,
    (buffer_unsandwich_obligations_obligation_8 y),
    (buffer_unsandwich_obligations_obligation_9 r))), p, (B2
    (buffer_unsandwich_obligations_obligation_7,
    (buffer_unsandwich_obligations_obligation_8 y),
    (buffer_unsandwich_obligations_obligation_9 r), p0, p1)), p2)
| B5 (p, p0, p1, p2, p3) ->
  Sandwich ((S (S (S O))), (Mix (buffer_unsandwich_obligations_obligation_11,
    buffer_unsandwich_obligations_obligation_12,
    buffer_unsandwich_obligations_obligation_13)), p, (B3
    (buffer_unsandwich_obligations_obligation_11,
    buffer_unsandwich_obligations_obligation_12,
    buffer_unsandwich_obligations_obligation_13, p0, p1, p2)), p3)

(** val buffer_halve :
    nat -> nat -> color -> 'a1 buffer -> ('a1 optionN, 'a1 buffer) prod **)

let buffer_halve lvl _ _ = function
| B0 -> Coq_pair (NoneN, B0)
| B1 (_, _, p) -> Coq_pair ((SomeN p), B0)
| B2 (_, _, _, p, p0) ->
  Coq_pair (NoneN, (B1 (NoYellow, SomeRed, (Coq_prodS (lvl, p, p0)))))
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair ((SomeN p), (B1 (NoYellow, SomeRed, (Coq_prodS (lvl, p0, p1)))))
| B4 (_, _, p, p0, p1, p2) ->
  Coq_pair (NoneN, (B2 (NoGreen, NoYellow, SomeRed, (Coq_prodS (lvl, p, p0)),
    (Coq_prodS (lvl, p1, p2)))))
| B5 (p, p0, p1, p2, p3) ->
  Coq_pair ((SomeN p), (B2 (NoGreen, NoYellow, SomeRed, (Coq_prodS (lvl, p0,
    p1)), (Coq_prodS (lvl, p2, p3)))))

(** val to_yellow :
    nat -> nat -> green_hue -> yellow_hue -> 'a1 buffer -> 'a1 buffer **)

let to_yellow _ _ _ _ = function
| B1 (_, _, p) -> B1 (SomeYellow, NoRed, p)
| B2 (_, _, _, p, p0) -> B2 (NoGreen, SomeYellow, NoRed, p, p0)
| B3 (_, _, _, p, p0, p1) -> B3 (NoGreen, SomeYellow, NoRed, p, p0, p1)
| B4 (_, _, p, p0, p1, p2) -> B4 (SomeYellow, NoRed, p, p0, p1, p2)
| _ -> assert false (* absurd case *)

(** val to_red : nat -> nat -> color -> 'a1 buffer -> 'a1 buffer **)

let to_red _ _ _ = function
| B1 (_, _, p) -> B1 (NoYellow, SomeRed, p)
| B2 (_, _, _, p, p0) -> B2 (NoGreen, NoYellow, SomeRed, p, p0)
| B3 (_, _, _, p, p0, p1) -> B3 (NoGreen, NoYellow, SomeRed, p, p0, p1)
| B4 (_, _, p, p0, p1, p2) -> B4 (NoYellow, SomeRed, p, p0, p1, p2)
| x -> x

(** val green_prefix_concat :
    nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer,
    'a1 buffer) prod **)

let green_prefix_concat lvl _ size2 _ b1 b2 =
  match b1 with
  | B0 ->
    let Coq_pair (p, b) = green_pop (S lvl) size2 b2 in
    (match p with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p0, p1) ->
       Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p0, p1)), b))
  | B1 (_, _, p) ->
    let Coq_pair (p0, b) = green_pop (S lvl) size2 b2 in
    (match p0 with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p1, p2) ->
       Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p1, p2)), b))
  | B2 (_, _, _, p, p0) ->
    Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p, p0)),
      (to_yellow (S lvl) size2 SomeGreen NoYellow b2))
  | B3 (_, _, _, p, p0, p1) ->
    Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)),
      (to_yellow (S lvl) size2 SomeGreen NoYellow b2))
  | B4 (_, _, p, p0, p1, p2) ->
    Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p, p0)),
      (green_push (S lvl) size2 (Coq_prodS (lvl, p1, p2)) b2))
  | B5 (p, p0, p1, p2, p3) ->
    Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)),
      (green_push (S lvl) size2 (Coq_prodS (lvl, p2, p3)) b2))

(** val green_suffix_concat :
    nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer,
    'a1 buffer) prod **)

let green_suffix_concat lvl size1 _ _ b1 = function
| B0 ->
  let Coq_pair (b, p) = green_eject (S lvl) size1 b1 in
  (match p with
   | Coq_prodZ _ -> assert false (* absurd case *)
   | Coq_prodS (_, p0, p1) ->
     Coq_pair (b, (B2 (SomeGreen, NoYellow, NoRed, p0, p1))))
| B1 (_, _, p) ->
  let Coq_pair (b, p0) = green_eject (S lvl) size1 b1 in
  (match p0 with
   | Coq_prodZ _ -> assert false (* absurd case *)
   | Coq_prodS (_, p1, p2) ->
     Coq_pair (b, (B3 (SomeGreen, NoYellow, NoRed, p1, p2, p))))
| B2 (_, _, _, p, p0) ->
  Coq_pair ((to_yellow (S lvl) size1 SomeGreen NoYellow b1), (B2 (SomeGreen,
    NoYellow, NoRed, p, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair ((to_yellow (S lvl) size1 SomeGreen NoYellow b1), (B3 (SomeGreen,
    NoYellow, NoRed, p, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Coq_pair ((green_inject (S lvl) size1 b1 (Coq_prodS (lvl, p, p0))), (B2
    (SomeGreen, NoYellow, NoRed, p1, p2)))
| B5 (p, p0, p1, p2, p3) ->
  Coq_pair ((green_inject (S lvl) size1 b1 (Coq_prodS (lvl, p, p0))), (B3
    (SomeGreen, NoYellow, NoRed, p1, p2, p3)))

(** val yellow_prefix_concat :
    nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer,
    'a1 buffer) prod **)

let yellow_prefix_concat lvl _ size2 _ b1 b2 =
  match b1 with
  | B0 ->
    let Coq_pair (p, b) = yellow_pop (S lvl) size2 b2 in
    (match p with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p0, p1) ->
       Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p0, p1)), b))
  | B1 (_, _, p) ->
    let Coq_pair (p0, b) = yellow_pop (S lvl) size2 b2 in
    (match p0 with
     | Coq_prodZ _ -> assert false (* absurd case *)
     | Coq_prodS (_, p1, p2) ->
       Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p1, p2)), b))
  | B2 (_, _, _, p, p0) ->
    Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p, p0)),
      (to_red (S lvl) size2 (Mix (NoGreen, SomeYellow, NoRed)) b2))
  | B3 (_, _, _, p, p0, p1) ->
    Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)),
      (to_red (S lvl) size2 (Mix (NoGreen, SomeYellow, NoRed)) b2))
  | B4 (_, _, p, p0, p1, p2) ->
    Coq_pair ((B2 (SomeGreen, NoYellow, NoRed, p, p0)),
      (yellow_push (S lvl) size2 (Coq_prodS (lvl, p1, p2)) b2))
  | B5 (p, p0, p1, p2, p3) ->
    Coq_pair ((B3 (SomeGreen, NoYellow, NoRed, p, p0, p1)),
      (yellow_push (S lvl) size2 (Coq_prodS (lvl, p2, p3)) b2))

(** val yellow_suffix_concat :
    nat -> nat -> nat -> color -> 'a1 buffer -> 'a1 buffer -> ('a1 buffer,
    'a1 buffer) prod **)

let yellow_suffix_concat lvl size1 _ _ b1 = function
| B0 ->
  let Coq_pair (b, p) = yellow_eject (S lvl) size1 b1 in
  (match p with
   | Coq_prodZ _ -> assert false (* absurd case *)
   | Coq_prodS (_, p0, p1) ->
     Coq_pair (b, (B2 (SomeGreen, NoYellow, NoRed, p0, p1))))
| B1 (_, _, p) ->
  let Coq_pair (b, p0) = yellow_eject (S lvl) size1 b1 in
  (match p0 with
   | Coq_prodZ _ -> assert false (* absurd case *)
   | Coq_prodS (_, p1, p2) ->
     Coq_pair (b, (B3 (SomeGreen, NoYellow, NoRed, p1, p2, p))))
| B2 (_, _, _, p, p0) ->
  Coq_pair ((to_red (S lvl) size1 (Mix (NoGreen, SomeYellow, NoRed)) b1), (B2
    (SomeGreen, NoYellow, NoRed, p, p0)))
| B3 (_, _, _, p, p0, p1) ->
  Coq_pair ((to_red (S lvl) size1 (Mix (NoGreen, SomeYellow, NoRed)) b1), (B3
    (SomeGreen, NoYellow, NoRed, p, p0, p1)))
| B4 (_, _, p, p0, p1, p2) ->
  Coq_pair ((yellow_inject (S lvl) size1 b1 (Coq_prodS (lvl, p, p0))), (B2
    (SomeGreen, NoYellow, NoRed, p1, p2)))
| B5 (p, p0, p1, p2, p3) ->
  Coq_pair ((yellow_inject (S lvl) size1 b1 (Coq_prodS (lvl, p, p0))), (B3
    (SomeGreen, NoYellow, NoRed, p1, p2, p3)))

(** val green_opt_push_obligations_obligation_2 : green_hue **)

let green_opt_push_obligations_obligation_2 =
  SomeGreen

(** val green_opt_push_obligations_obligation_3 : yellow_hue **)

let green_opt_push_obligations_obligation_3 =
  SomeYellow

(** val green_opt_push_obligations_obligation_4 : red_hue **)

let green_opt_push_obligations_obligation_4 =
  SomeRed

(** val green_opt_push_obligations_obligation_6 : yellow_hue **)

let green_opt_push_obligations_obligation_6 =
  SomeYellow

(** val green_opt_push_obligations_obligation_7 : red_hue **)

let green_opt_push_obligations_obligation_7 =
  SomeRed

(** val green_opt_push :
    nat -> nat -> nat -> 'a1 optionN -> 'a1 buffer -> 'a1 chain **)

let green_opt_push lvl sizeb _ o b =
  match o with
  | NoneN ->
    Ending (sizeb, (Mix (NoGreen, NoYellow, SomeRed)),
      (to_red lvl sizeb (Mix (SomeGreen, NoYellow, NoRed)) b))
  | SomeN p ->
    (match b with
     | B2 (_, _, _, p0, p1) ->
       Ending ((S (S (S O))), (Mix (green_opt_push_obligations_obligation_2,
         green_opt_push_obligations_obligation_3,
         green_opt_push_obligations_obligation_4)), (B3
         (green_opt_push_obligations_obligation_2,
         green_opt_push_obligations_obligation_3,
         green_opt_push_obligations_obligation_4, p, p0, p1)))
     | B3 (_, _, _, p0, p1, p2) ->
       Ending ((S (S (S (S O)))), (Mix (NoGreen,
         green_opt_push_obligations_obligation_6,
         green_opt_push_obligations_obligation_7)), (B4
         (green_opt_push_obligations_obligation_6,
         green_opt_push_obligations_obligation_7, p, p0, p1, p2)))
     | _ -> assert false (* absurd case *))

(** val green_opt_inject_obligations_obligation_2 : green_hue **)

let green_opt_inject_obligations_obligation_2 =
  SomeGreen

(** val green_opt_inject_obligations_obligation_3 : yellow_hue **)

let green_opt_inject_obligations_obligation_3 =
  SomeYellow

(** val green_opt_inject_obligations_obligation_4 : red_hue **)

let green_opt_inject_obligations_obligation_4 =
  SomeRed

(** val green_opt_inject_obligations_obligation_6 : yellow_hue **)

let green_opt_inject_obligations_obligation_6 =
  SomeYellow

(** val green_opt_inject_obligations_obligation_7 : red_hue **)

let green_opt_inject_obligations_obligation_7 =
  SomeRed

(** val green_opt_inject :
    nat -> nat -> nat -> 'a1 buffer -> 'a1 optionN -> 'a1 chain **)

let green_opt_inject lvl sizeb _ b = function
| NoneN ->
  Ending (sizeb, (Mix (NoGreen, NoYellow, SomeRed)),
    (to_red lvl sizeb (Mix (SomeGreen, NoYellow, NoRed)) b))
| SomeN p ->
  (match b with
   | B2 (_, _, _, p0, p1) ->
     Ending ((S (S (S O))), (Mix (green_opt_inject_obligations_obligation_2,
       green_opt_inject_obligations_obligation_3,
       green_opt_inject_obligations_obligation_4)), (B3
       (green_opt_inject_obligations_obligation_2,
       green_opt_inject_obligations_obligation_3,
       green_opt_inject_obligations_obligation_4, p0, p1, p)))
   | B3 (_, _, _, p0, p1, p2) ->
     Ending ((S (S (S (S O)))), (Mix (NoGreen,
       green_opt_inject_obligations_obligation_6,
       green_opt_inject_obligations_obligation_7)), (B4
       (green_opt_inject_obligations_obligation_6,
       green_opt_inject_obligations_obligation_7, p0, p1, p2, p)))
   | _ -> assert false (* absurd case *))

(** val chain_of_opt3_obligations_obligation_2 : yellow_hue **)

let chain_of_opt3_obligations_obligation_2 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_3 : red_hue **)

let chain_of_opt3_obligations_obligation_3 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_5 : green_hue **)

let chain_of_opt3_obligations_obligation_5 =
  SomeGreen

(** val chain_of_opt3_obligations_obligation_6 : yellow_hue **)

let chain_of_opt3_obligations_obligation_6 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_7 : red_hue **)

let chain_of_opt3_obligations_obligation_7 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_9 : green_hue **)

let chain_of_opt3_obligations_obligation_9 =
  SomeGreen

(** val chain_of_opt3_obligations_obligation_10 : yellow_hue **)

let chain_of_opt3_obligations_obligation_10 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_11 : red_hue **)

let chain_of_opt3_obligations_obligation_11 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_13 : yellow_hue **)

let chain_of_opt3_obligations_obligation_13 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_14 : red_hue **)

let chain_of_opt3_obligations_obligation_14 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_16 : green_hue **)

let chain_of_opt3_obligations_obligation_16 =
  SomeGreen

(** val chain_of_opt3_obligations_obligation_17 : yellow_hue **)

let chain_of_opt3_obligations_obligation_17 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_18 : red_hue **)

let chain_of_opt3_obligations_obligation_18 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_20 : green_hue **)

let chain_of_opt3_obligations_obligation_20 =
  SomeGreen

(** val chain_of_opt3_obligations_obligation_21 : yellow_hue **)

let chain_of_opt3_obligations_obligation_21 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_22 : red_hue **)

let chain_of_opt3_obligations_obligation_22 =
  SomeRed

(** val chain_of_opt3_obligations_obligation_24 : yellow_hue **)

let chain_of_opt3_obligations_obligation_24 =
  SomeYellow

(** val chain_of_opt3_obligations_obligation_25 : red_hue **)

let chain_of_opt3_obligations_obligation_25 =
  SomeRed

(** val chain_of_opt3 :
    nat -> nat -> nat -> nat -> 'a1 optionN -> 'a1 optionN -> 'a1 optionN ->
    'a1 chain **)

let chain_of_opt3 _ _ _ _ o1 o2 o3 =
  match o1 with
  | NoneN ->
    (match o2 with
     | NoneN ->
       (match o3 with
        | NoneN -> Ending (O, (Mix (NoGreen, NoYellow, SomeRed)), B0)
        | SomeN p ->
          Ending ((S O), (Mix (NoGreen,
            chain_of_opt3_obligations_obligation_2,
            chain_of_opt3_obligations_obligation_3)), (B1
            (chain_of_opt3_obligations_obligation_2,
            chain_of_opt3_obligations_obligation_3, p))))
     | SomeN p ->
       (match p with
        | Coq_prodZ _ -> assert false (* absurd case *)
        | Coq_prodS (_, p0, p1) ->
          (match o3 with
           | NoneN ->
             Ending ((S (S O)), (Mix (chain_of_opt3_obligations_obligation_5,
               chain_of_opt3_obligations_obligation_6,
               chain_of_opt3_obligations_obligation_7)), (B2
               (chain_of_opt3_obligations_obligation_5,
               chain_of_opt3_obligations_obligation_6,
               chain_of_opt3_obligations_obligation_7, p0, p1)))
           | SomeN p2 ->
             Ending ((S (S (S O))), (Mix
               (chain_of_opt3_obligations_obligation_9,
               chain_of_opt3_obligations_obligation_10,
               chain_of_opt3_obligations_obligation_11)), (B3
               (chain_of_opt3_obligations_obligation_9,
               chain_of_opt3_obligations_obligation_10,
               chain_of_opt3_obligations_obligation_11, p0, p1, p2))))))
  | SomeN p ->
    (match o2 with
     | NoneN ->
       (match o3 with
        | NoneN ->
          Ending ((S O), (Mix (NoGreen,
            chain_of_opt3_obligations_obligation_13,
            chain_of_opt3_obligations_obligation_14)), (B1
            (chain_of_opt3_obligations_obligation_13,
            chain_of_opt3_obligations_obligation_14, p)))
        | SomeN p0 ->
          Ending ((S (S O)), (Mix (chain_of_opt3_obligations_obligation_16,
            chain_of_opt3_obligations_obligation_17,
            chain_of_opt3_obligations_obligation_18)), (B2
            (chain_of_opt3_obligations_obligation_16,
            chain_of_opt3_obligations_obligation_17,
            chain_of_opt3_obligations_obligation_18, p, p0))))
     | SomeN p0 ->
       (match p0 with
        | Coq_prodZ _ -> assert false (* absurd case *)
        | Coq_prodS (_, p1, p2) ->
          (match o3 with
           | NoneN ->
             Ending ((S (S (S O))), (Mix
               (chain_of_opt3_obligations_obligation_20,
               chain_of_opt3_obligations_obligation_21,
               chain_of_opt3_obligations_obligation_22)), (B3
               (chain_of_opt3_obligations_obligation_20,
               chain_of_opt3_obligations_obligation_21,
               chain_of_opt3_obligations_obligation_22, p, p1, p2)))
           | SomeN p3 ->
             Ending ((S (S (S (S O)))), (Mix (NoGreen,
               chain_of_opt3_obligations_obligation_24,
               chain_of_opt3_obligations_obligation_25)), (B4
               (chain_of_opt3_obligations_obligation_24,
               chain_of_opt3_obligations_obligation_25, p, p1, p2, p3))))))

(** val translate : nat -> nat -> nat -> color -> 'a1 chain -> 'a1 chain **)

let translate _ _ _ _ c =
  c

(** val make_small :
    nat -> nat -> nat -> nat -> color -> color -> color -> 'a1 buffer -> 'a1
    buffer -> 'a1 buffer -> 'a1 chain **)

let make_small lvl size1 size2 size3 c1 c2 c3 b1 b2 b3 =
  match prefix_decompose lvl size1 c1 b1 with
  | Underflow (size, o) ->
    (match suffix_decompose lvl size3 c3 b3 with
     | Underflow (size0, o0) ->
       (match buffer_unsandwich (S lvl) size2 c2 b2 with
        | Alone (s, o1) -> chain_of_opt3 lvl size s size0 o o1 o0
        | Sandwich (s, c, p, b, p0) ->
          translate lvl
            (add (add (add (add (S (S O)) size) s) s) (add (S (S O)) size0))
            (add (add (add size (S (S s))) (S (S s))) size0) (Mix (SomeGreen,
            NoYellow, NoRed)) (Chain ((S lvl),
            (add (add (add (add (S (S O)) size) s) s) (add (S (S O)) size0)),
            s, (Mix (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow,
            NoRed)), (G (SomeGreen, NoRed)), (Packet ((S lvl),
            (add (S (S O)) size), s, (add (S (S O)) size0), s, (Mix
            (SomeGreen, NoYellow, NoRed)), NoYellow, (prefix23 lvl size o p),
            (Hole s), (suffix23 lvl size0 p0 o0))), (Ending (s, c, b)))))
     | Ok (size0, b) ->
       (match buffer_pop (S lvl) size2 c2 b2 with
        | Some p ->
          let Coq_pair (p0, b0) = p in
          translate lvl
            (add
              (add (add (add (S (S O)) size) (PeanoNat.Nat.pred size2))
                (PeanoNat.Nat.pred size2)) size0)
            (add (add (add size size2) size2) size0) (Mix (SomeGreen,
            NoYellow, NoRed)) (Chain ((S lvl),
            (add
              (add (add (add (S (S O)) size) (PeanoNat.Nat.pred size2))
                (PeanoNat.Nat.pred size2)) size0), (PeanoNat.Nat.pred size2),
            (Mix (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow,
            NoRed)), (G (SomeGreen, NoRed)), (Packet ((S lvl),
            (add (S (S O)) size), (PeanoNat.Nat.pred size2), size0,
            (PeanoNat.Nat.pred size2), (Mix (SomeGreen, NoYellow, NoRed)),
            NoYellow, (prefix23 lvl size o p0), (Hole
            (PeanoNat.Nat.pred size2)), b)), (Ending
            ((PeanoNat.Nat.pred size2), (Mix (NoGreen, NoYellow, SomeRed)),
            b0))))
        | None ->
          translate lvl (add size size0)
            (add (add (add size size2) size2) size0) (Mix (SomeGreen,
            NoYellow, NoRed)) (green_opt_push lvl size0 size o b))
     | Overflow (size0, b, p) ->
       let Coq_pair (p0, b0) = suffix_rot (S lvl) size2 c2 b2 p in
       translate lvl (add (add (add (add (S (S O)) size) size2) size2) size0)
         (add (add (add size size2) size2) (S (S size0))) (Mix (SomeGreen,
         NoYellow, NoRed)) (Chain ((S lvl),
         (add (add (add (add (S (S O)) size) size2) size2) size0), size2,
         (Mix (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow,
         NoRed)), (G (SomeGreen, NoRed)), (Packet ((S lvl),
         (add (S (S O)) size), size2, size0, size2, (Mix (SomeGreen,
         NoYellow, NoRed)), NoYellow, (prefix23 lvl size o p0), (Hole size2),
         b)), (Ending (size2, (Mix (NoGreen, NoYellow, SomeRed)),
         (to_red (S lvl) size2 c2 b0))))))
  | Ok (size, b) ->
    (match suffix_decompose lvl size3 c3 b3 with
     | Underflow (size0, o) ->
       (match buffer_eject (S lvl) size2 c2 b2 with
        | Some p ->
          let Coq_pair (b0, p0) = p in
          translate lvl
            (add
              (add (add size (PeanoNat.Nat.pred size2))
                (PeanoNat.Nat.pred size2)) (add (S (S O)) size0))
            (add (add (add size size2) size2) size0) (Mix (SomeGreen,
            NoYellow, NoRed)) (Chain ((S lvl),
            (add
              (add (add size (PeanoNat.Nat.pred size2))
                (PeanoNat.Nat.pred size2)) (add (S (S O)) size0)),
            (PeanoNat.Nat.pred size2), (Mix (SomeGreen, NoYellow, NoRed)),
            (Mix (SomeGreen, NoYellow, NoRed)), (G (SomeGreen, NoRed)),
            (Packet ((S lvl), size, (PeanoNat.Nat.pred size2),
            (add (S (S O)) size0), (PeanoNat.Nat.pred size2), (Mix
            (SomeGreen, NoYellow, NoRed)), NoYellow, b, (Hole
            (PeanoNat.Nat.pred size2)), (suffix23 lvl size0 p0 o))), (Ending
            ((PeanoNat.Nat.pred size2), (Mix (NoGreen, NoYellow, SomeRed)),
            b0))))
        | None ->
          translate lvl (add size0 size)
            (add (add (add size size2) size2) size0) (Mix (SomeGreen,
            NoYellow, NoRed)) (green_opt_inject lvl size size0 b o))
     | Ok (size0, b0) ->
       Chain ((S lvl), (add (add (add size size2) size2) size0), size2, (Mix
         (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)),
         (G (SomeGreen, NoRed)), (Packet ((S lvl), size, size2, size0, size2,
         (Mix (SomeGreen, NoYellow, NoRed)), NoYellow, b, (Hole size2), b0)),
         (Ending (size2, (Mix (NoGreen, NoYellow, SomeRed)),
         (to_red (S lvl) size2 c2 b2))))
     | Overflow (size0, b0, p) ->
       translate lvl (add (add (add size (S size2)) (S size2)) size0)
         (add (add (add size size2) size2) (S (S size0))) (Mix (SomeGreen,
         NoYellow, NoRed)) (Chain ((S lvl),
         (add (add (add size (S size2)) (S size2)) size0), (S size2), (Mix
         (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)),
         (G (SomeGreen, NoRed)), (Packet ((S lvl), size, (S size2), size0, (S
         size2), (Mix (SomeGreen, NoYellow, NoRed)), NoYellow, b, (Hole (S
         size2)), b0)), (buffer_inject (S lvl) size2 c2 b2 p))))
  | Overflow (size, b, p) ->
    (match suffix_decompose lvl size3 c3 b3 with
     | Underflow (size0, o) ->
       let Coq_pair (b0, p0) = prefix_rot (S lvl) size2 c2 p b2 in
       translate lvl (add (add (add size size2) size2) (add (S (S O)) size0))
         (S (S (add (add (add size size2) size2) size0))) (Mix (SomeGreen,
         NoYellow, NoRed)) (Chain ((S lvl),
         (add (add (add size size2) size2) (add (S (S O)) size0)), size2,
         (Mix (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow,
         NoRed)), (G (SomeGreen, NoRed)), (Packet ((S lvl), size, size2,
         (add (S (S O)) size0), size2, (Mix (SomeGreen, NoYellow, NoRed)),
         NoYellow, b, (Hole size2), (suffix23 lvl size0 p0 o))), (Ending
         (size2, (Mix (NoGreen, NoYellow, SomeRed)),
         (to_red (S lvl) size2 c2 b0)))))
     | Ok (size0, b0) ->
       translate lvl (add (add (add size (S size2)) (S size2)) size0) (S (S
         (add (add (add size size2) size2) size0))) (Mix (SomeGreen,
         NoYellow, NoRed)) (Chain ((S lvl),
         (add (add (add size (S size2)) (S size2)) size0), (S size2), (Mix
         (SomeGreen, NoYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)),
         (G (SomeGreen, NoRed)), (Packet ((S lvl), size, (S size2), size0, (S
         size2), (Mix (SomeGreen, NoYellow, NoRed)), NoYellow, b, (Hole (S
         size2)), b0)), (buffer_push (S lvl) size2 c2 p b2)))
     | Overflow (size0, b0, p0) ->
       let Coq_pair (o, b4) = buffer_halve (S lvl) size2 c2 b2 in
       let c = Chain ((S (S lvl)),
         (add
           (add
             (add size
               (add
                 (add
                   (add (S (PeanoNat.Nat.modulo size2 (S (S O))))
                     (PeanoNat.Nat.div size2 (S (S O))))
                   (PeanoNat.Nat.div size2 (S (S O)))) (S O)))
             (add
               (add
                 (add (S (PeanoNat.Nat.modulo size2 (S (S O))))
                   (PeanoNat.Nat.div size2 (S (S O))))
                 (PeanoNat.Nat.div size2 (S (S O)))) (S O))) size0),
         (PeanoNat.Nat.div size2 (S (S O))), (Mix (SomeGreen, NoYellow,
         NoRed)), (Mix (SomeGreen, NoYellow, NoRed)), (G (SomeGreen, NoRed)),
         (Packet ((S (S lvl)), size,
         (add
           (add
             (add (S (PeanoNat.Nat.modulo size2 (S (S O))))
               (PeanoNat.Nat.div size2 (S (S O))))
             (PeanoNat.Nat.div size2 (S (S O)))) (S O)), size0,
         (PeanoNat.Nat.div size2 (S (S O))), (Mix (SomeGreen, NoYellow,
         NoRed)), SomeYellow, b, (Packet ((S (S lvl)), (S
         (PeanoNat.Nat.modulo size2 (S (S O)))),
         (PeanoNat.Nat.div size2 (S (S O))), (S O),
         (PeanoNat.Nat.div size2 (S (S O))), (Mix (NoGreen, SomeYellow,
         NoRed)), NoYellow,
         (suffix12 (S lvl) (PeanoNat.Nat.modulo size2 (S (S O))) p o), (Hole
         (PeanoNat.Nat.div size2 (S (S O)))), (B1 (SomeYellow, NoRed, p0)))),
         b0)), (Ending ((PeanoNat.Nat.div size2 (S (S O))), (Mix (NoGreen,
         NoYellow, SomeRed)), b4)))
       in
       translate lvl
         (add
           (add
             (add size
               (add
                 (add
                   (add (S (PeanoNat.Nat.modulo size2 (S (S O))))
                     (PeanoNat.Nat.div size2 (S (S O))))
                   (PeanoNat.Nat.div size2 (S (S O)))) (S O)))
             (add
               (add
                 (add (S (PeanoNat.Nat.modulo size2 (S (S O))))
                   (PeanoNat.Nat.div size2 (S (S O))))
                 (PeanoNat.Nat.div size2 (S (S O)))) (S O))) size0) (S (S
         (add (add (add size size2) size2) (S (S size0))))) (Mix (SomeGreen,
         NoYellow, NoRed)) c)

(** val green_of_red : nat -> nat -> 'a1 chain -> 'a1 chain **)

let green_of_red lvl _ = function
| Ending (_, _, _) -> assert false (* absurd case *)
| Chain (_, _, _, _, _, r, p, c0) ->
  (match r with
   | R ->
     (match p with
      | Hole _ -> assert false (* absurd case *)
      | Packet (_, psize, _, ssize, _, _, y, b, p0, b0) ->
        (match p0 with
         | Hole _ ->
           (match c0 with
            | Ending (size, c1, b1) ->
              make_small lvl psize size ssize (Mix (NoGreen, NoYellow,
                SomeRed)) c1 (Mix (NoGreen, NoYellow, SomeRed)) b b1 b0
            | Chain (_, _, _, _, _, r0, p1, c1) ->
              (match r0 with
               | G (g, r1) ->
                 (match p1 with
                  | Hole _ -> assert false (* absurd case *)
                  | Packet (hlvl, psize0, pktsize, ssize0, hsize, _, y0, b1,
                            p2, b2) ->
                    let Coq_pair (b3, b4) =
                      green_prefix_concat lvl psize psize0 (Mix (NoGreen,
                        NoYellow, SomeRed)) b b1
                    in
                    let Coq_pair (b5, b6) =
                      green_suffix_concat lvl ssize0 ssize (Mix (NoGreen,
                        NoYellow, SomeRed)) b2 b0
                    in
                    let c2 = Chain (hlvl,
                      (add
                        (add
                          (add
                            (add (S (S O))
                              (PeanoNat.Nat.modulo psize (S (S O))))
                            (add
                              (add
                                (add
                                  (PeanoNat.Nat.pred
                                    (add (PeanoNat.Nat.div psize (S (S O)))
                                      psize0)) pktsize) pktsize)
                              (PeanoNat.Nat.pred
                                (add (PeanoNat.Nat.div ssize (S (S O)))
                                  ssize0))))
                          (add
                            (add
                              (add
                                (PeanoNat.Nat.pred
                                  (add (PeanoNat.Nat.div psize (S (S O)))
                                    psize0)) pktsize) pktsize)
                            (PeanoNat.Nat.pred
                              (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
                        (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O))))),
                      hsize, (Mix (SomeGreen, NoYellow, NoRed)), (Mix (g,
                      NoYellow, r1)), (G (g, r1)), (Packet (hlvl,
                      (add (S (S O)) (PeanoNat.Nat.modulo psize (S (S O)))),
                      (add
                        (add
                          (add
                            (PeanoNat.Nat.pred
                              (add (PeanoNat.Nat.div psize (S (S O))) psize0))
                            pktsize) pktsize)
                        (PeanoNat.Nat.pred
                          (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))),
                      (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O)))),
                      hsize, (Mix (SomeGreen, NoYellow, NoRed)), SomeYellow,
                      b3, (Packet (hlvl,
                      (PeanoNat.Nat.pred
                        (add (PeanoNat.Nat.div psize (S (S O))) psize0)),
                      pktsize,
                      (PeanoNat.Nat.pred
                        (add (PeanoNat.Nat.div ssize (S (S O))) ssize0)),
                      hsize, (Mix (NoGreen, SomeYellow, NoRed)), y0, b4, p2,
                      b5)), b6)), c1)
                    in
                    translate lvl
                      (add
                        (add
                          (add
                            (add (S (S O))
                              (PeanoNat.Nat.modulo psize (S (S O))))
                            (add
                              (add
                                (add
                                  (PeanoNat.Nat.pred
                                    (add (PeanoNat.Nat.div psize (S (S O)))
                                      psize0)) pktsize) pktsize)
                              (PeanoNat.Nat.pred
                                (add (PeanoNat.Nat.div ssize (S (S O)))
                                  ssize0))))
                          (add
                            (add
                              (add
                                (PeanoNat.Nat.pred
                                  (add (PeanoNat.Nat.div psize (S (S O)))
                                    psize0)) pktsize) pktsize)
                            (PeanoNat.Nat.pred
                              (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
                        (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O)))))
                      (add
                        (add
                          (add psize
                            (add (add (add psize0 pktsize) pktsize) ssize0))
                          (add (add (add psize0 pktsize) pktsize) ssize0))
                        ssize) (Mix (SomeGreen, NoYellow, NoRed)) c2)
               | _ -> assert false (* absurd case *)))
         | Packet (hlvl, psize0, pktsize, ssize0, hsize, _, y0, b1, p1, b2) ->
           let Coq_pair (b3, b4) =
             yellow_prefix_concat lvl psize psize0 (Mix (NoGreen, NoYellow,
               SomeRed)) b (to_yellow (S lvl) psize0 NoGreen y b1)
           in
           let Coq_pair (b5, b6) =
             yellow_suffix_concat lvl ssize0 ssize (Mix (NoGreen, NoYellow,
               SomeRed)) (to_yellow (S lvl) ssize0 NoGreen y b2) b0
           in
           let c1 = Chain ((S lvl),
             (add
               (add
                 (add (add (S (S O)) (PeanoNat.Nat.modulo psize (S (S O))))
                   (add
                     (add
                       (add
                         (PeanoNat.Nat.pred
                           (add (PeanoNat.Nat.div psize (S (S O))) psize0))
                         pktsize) pktsize)
                     (PeanoNat.Nat.pred
                       (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
                 (add
                   (add
                     (add
                       (PeanoNat.Nat.pred
                         (add (PeanoNat.Nat.div psize (S (S O))) psize0))
                       pktsize) pktsize)
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
               (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O))))),
             (add
               (add
                 (add
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div psize (S (S O))) psize0)) pktsize)
                 pktsize)
               (PeanoNat.Nat.pred
                 (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))), (Mix
             (SomeGreen, NoYellow, NoRed)), (Mix (NoGreen, NoYellow,
             SomeRed)), (G (NoGreen, SomeRed)), (Packet ((S lvl),
             (add (S (S O)) (PeanoNat.Nat.modulo psize (S (S O)))),
             (add
               (add
                 (add
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div psize (S (S O))) psize0)) pktsize)
                 pktsize)
               (PeanoNat.Nat.pred
                 (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))),
             (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O)))),
             (add
               (add
                 (add
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div psize (S (S O))) psize0)) pktsize)
                 pktsize)
               (PeanoNat.Nat.pred
                 (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))), (Mix
             (SomeGreen, NoYellow, NoRed)), NoYellow, b3, (Hole
             (add
               (add
                 (add
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div psize (S (S O))) psize0)) pktsize)
                 pktsize)
               (PeanoNat.Nat.pred
                 (add (PeanoNat.Nat.div ssize (S (S O))) ssize0)))), b6)),
             (Chain (hlvl,
             (add
               (add
                 (add
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div psize (S (S O))) psize0)) pktsize)
                 pktsize)
               (PeanoNat.Nat.pred
                 (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))), hsize,
             (Mix (NoGreen, NoYellow, SomeRed)), (Mix (SomeGreen, NoYellow,
             NoRed)), R, (Packet (hlvl,
             (PeanoNat.Nat.pred
               (add (PeanoNat.Nat.div psize (S (S O))) psize0)), pktsize,
             (PeanoNat.Nat.pred
               (add (PeanoNat.Nat.div ssize (S (S O))) ssize0)), hsize, (Mix
             (NoGreen, NoYellow, SomeRed)), y0, b4, p1, b5)), c0)))
           in
           translate lvl
             (add
               (add
                 (add (add (S (S O)) (PeanoNat.Nat.modulo psize (S (S O))))
                   (add
                     (add
                       (add
                         (PeanoNat.Nat.pred
                           (add (PeanoNat.Nat.div psize (S (S O))) psize0))
                         pktsize) pktsize)
                     (PeanoNat.Nat.pred
                       (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
                 (add
                   (add
                     (add
                       (PeanoNat.Nat.pred
                         (add (PeanoNat.Nat.div psize (S (S O))) psize0))
                       pktsize) pktsize)
                   (PeanoNat.Nat.pred
                     (add (PeanoNat.Nat.div ssize (S (S O))) ssize0))))
               (add (S (S O)) (PeanoNat.Nat.modulo ssize (S (S O)))))
             (add
               (add
                 (add psize (add (add (add psize0 pktsize) pktsize) ssize0))
                 (add (add (add psize0 pktsize) pktsize) ssize0)) ssize) (Mix
             (SomeGreen, NoYellow, NoRed)) c1))
   | _ -> assert false (* absurd case *))

(** val ensure_green :
    nat -> nat -> green_hue -> red_hue -> 'a1 chain -> 'a1 chain **)

let ensure_green lvl _ _ _ = function
| Ending (size, c, b) -> Ending (size, c, b)
| Chain (hlvl, size, hsize, _, _, r, p, c) ->
  (match r with
   | G (g, r0) ->
     Chain (hlvl, size, hsize, (Mix (SomeGreen, NoYellow, NoRed)), (Mix (g,
       NoYellow, r0)), (G (g, r0)), p, c)
   | Y -> assert false (* absurd case *)
   | R ->
     green_of_red lvl size (Chain (hlvl, size, hsize, (Mix (NoGreen,
       NoYellow, SomeRed)), (Mix (SomeGreen, NoYellow, NoRed)), R, p, c)))

(** val make_yellow :
    nat -> nat -> nat -> nat -> nat -> nat -> green_hue -> yellow_hue ->
    yellow_hue -> green_hue -> yellow_hue -> green_hue -> red_hue -> 'a1
    buffer -> 'a1 packet -> 'a1 buffer -> 'a1 chain -> 'a1 deque **)

let make_yellow lvl ps pkts ss cs _ gp yp ypkt gs ys gc rc p pkt s c =
  T (NoGreen, SomeYellow, (Chain (lvl, (add (add (add ps pkts) pkts) ss), cs,
    (Mix (NoGreen, SomeYellow, NoRed)), (Mix (SomeGreen, NoYellow, NoRed)),
    Y, (Packet (lvl, ps, pkts, ss, cs, (Mix (NoGreen, SomeYellow, NoRed)),
    ypkt, (to_yellow O ps gp yp p), pkt, (to_yellow O ss gs ys s))),
    (ensure_green lvl cs gc rc c))))

(** val make_red :
    nat -> nat -> nat -> nat -> nat -> nat -> color -> yellow_hue -> color ->
    'a1 buffer -> 'a1 packet -> 'a1 buffer -> 'a1 chain -> 'a1 deque **)

let make_red lvl ps pkts ss cs _ cp ypkt cs0 p pkt s c =
  T (SomeGreen, NoYellow,
    (green_of_red O (add (add (add ps pkts) pkts) ss) (Chain (lvl,
      (add (add (add ps pkts) pkts) ss), cs, (Mix (NoGreen, NoYellow,
      SomeRed)), (Mix (SomeGreen, NoYellow, NoRed)), R, (Packet (lvl, ps,
      pkts, ss, cs, (Mix (NoGreen, NoYellow, SomeRed)), ypkt,
      (to_red O ps cp p), pkt, (to_red O ss cs0 s))), c))))

(** val push : nat -> 'a1 -> 'a1 deque -> 'a1 deque **)

let push _ x = function
| T (_, _, c) ->
  (match c with
   | Ending (size, c0, b) ->
     T (SomeGreen, NoYellow, (buffer_push O size c0 (Coq_prodZ x) b))
   | Chain (_, _, _, _, _, r, p, c0) ->
     (match r with
      | G (g, r0) ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           make_yellow hlvl (S psize) pktsize ssize hsize
             (add (add (add (S psize) pktsize) pktsize) ssize) NoGreen
             SomeYellow y SomeGreen NoYellow g r0
             (green_push O psize (Coq_prodZ x) b) p0 b0 c0)
      | Y ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           make_red hlvl (S psize) pktsize ssize hsize
             (add (add (add (S psize) pktsize) pktsize) ssize) (Mix (NoGreen,
             NoYellow, SomeRed)) y (Mix (NoGreen, SomeYellow, NoRed))
             (yellow_push O psize (Coq_prodZ x) b) p0 b0 c0)
      | R -> assert false (* absurd case *)))

(** val inject : nat -> 'a1 deque -> 'a1 -> 'a1 deque **)

let inject _ d x =
  let T (_, _, c) = d in
  (match c with
   | Ending (size, c0, b) ->
     T (SomeGreen, NoYellow, (buffer_inject O size c0 b (Coq_prodZ x)))
   | Chain (_, _, _, _, _, r, p, c0) ->
     (match r with
      | G (g, r0) ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           make_yellow hlvl psize pktsize (S ssize) hsize (S
             (add (add (add psize pktsize) pktsize) ssize)) SomeGreen
             NoYellow y NoGreen SomeYellow g r0 b p0
             (green_inject O ssize b0 (Coq_prodZ x)) c0)
      | Y ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           make_red hlvl psize pktsize (S ssize) hsize (S
             (add (add (add psize pktsize) pktsize) ssize)) (Mix (NoGreen,
             SomeYellow, NoRed)) y (Mix (NoGreen, NoYellow, SomeRed)) b p0
             (yellow_inject O ssize b0 (Coq_prodZ x)) c0)
      | R -> assert false (* absurd case *)))

(** val option_pop : nat -> 'a1 deque -> ('a1, 'a1 deque) prod option **)

let option_pop _ = function
| T (_, _, c) ->
  (match c with
   | Ending (size, c0, b) ->
     (match buffer_pop O size c0 b with
      | Some p ->
        let Coq_pair (p0, b0) = p in
        (match p0 with
         | Coq_prodZ y ->
           Some (Coq_pair (y, (T (SomeGreen, NoYellow, (Ending
             ((PeanoNat.Nat.pred size), (Mix (NoGreen, NoYellow, SomeRed)),
             b0))))))
         | Coq_prodS (_, _, _) -> assert false (* absurd case *))
      | None -> None)
   | Chain (_, _, _, _, _, r, p, c0) ->
     (match r with
      | G (g, r0) ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           let Coq_pair (p1, b1) = green_pop O psize b in
           (match p1 with
            | Coq_prodZ y0 ->
              Some (Coq_pair (y0,
                (make_yellow hlvl (PeanoNat.Nat.pred psize) pktsize ssize
                  hsize
                  (PeanoNat.Nat.pred
                    (add (add (add psize pktsize) pktsize) ssize)) NoGreen
                  SomeYellow y SomeGreen NoYellow g r0 b1 p0 b0 c0)))
            | Coq_prodS (_, _, _) -> assert false (* absurd case *)))
      | Y ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           let Coq_pair (p1, b1) = yellow_pop O psize b in
           (match p1 with
            | Coq_prodZ y0 ->
              Some (Coq_pair (y0,
                (make_red hlvl (PeanoNat.Nat.pred psize) pktsize ssize hsize
                  (PeanoNat.Nat.pred
                    (add (add (add psize pktsize) pktsize) ssize)) (Mix
                  (NoGreen, NoYellow, SomeRed)) y (Mix (NoGreen, SomeYellow,
                  NoRed)) b1 p0 b0 c0)))
            | Coq_prodS (_, _, _) -> assert false (* absurd case *)))
      | R -> assert false (* absurd case *)))

(** val pop_obligations_obligation_2 :
    nat -> 'a1 deque -> ('a1, 'a1 deque) prod **)

let pop_obligations_obligation_2 _ _ =
  assert false (* absurd case *)

(** val pop : nat -> 'a1 deque -> ('a1, 'a1 deque) prod **)

let pop size d =
  match option_pop (S size) d with
  | Some p -> p
  | None -> pop_obligations_obligation_2 size d

(** val option_eject : nat -> 'a1 deque -> ('a1 deque, 'a1) prod option **)

let option_eject _ = function
| T (_, _, c) ->
  (match c with
   | Ending (size, c0, b) ->
     (match buffer_eject O size c0 b with
      | Some p ->
        let Coq_pair (b0, p0) = p in
        (match p0 with
         | Coq_prodZ y ->
           Some (Coq_pair ((T (SomeGreen, NoYellow, (Ending
             ((PeanoNat.Nat.pred size), (Mix (NoGreen, NoYellow, SomeRed)),
             b0)))), y))
         | Coq_prodS (_, _, _) -> assert false (* absurd case *))
      | None -> None)
   | Chain (_, _, _, _, _, r, p, c0) ->
     (match r with
      | G (g, r0) ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           let Coq_pair (b1, p1) = green_eject O ssize b0 in
           (match p1 with
            | Coq_prodZ y0 ->
              Some (Coq_pair
                ((make_yellow hlvl psize pktsize (PeanoNat.Nat.pred ssize)
                   hsize
                   (PeanoNat.Nat.pred
                     (add (add (add psize pktsize) pktsize) ssize)) SomeGreen
                   NoYellow y NoGreen SomeYellow g r0 b p0 b1 c0), y0))
            | Coq_prodS (_, _, _) -> assert false (* absurd case *)))
      | Y ->
        (match p with
         | Hole _ -> assert false (* absurd case *)
         | Packet (hlvl, psize, pktsize, ssize, hsize, _, y, b, p0, b0) ->
           let Coq_pair (b1, p1) = yellow_eject O ssize b0 in
           (match p1 with
            | Coq_prodZ y0 ->
              Some (Coq_pair
                ((make_red hlvl psize pktsize (PeanoNat.Nat.pred ssize) hsize
                   (PeanoNat.Nat.pred
                     (add (add (add psize pktsize) pktsize) ssize)) (Mix
                   (NoGreen, SomeYellow, NoRed)) y (Mix (NoGreen, NoYellow,
                   SomeRed)) b p0 b1 c0), y0))
            | Coq_prodS (_, _, _) -> assert false (* absurd case *)))
      | R -> assert false (* absurd case *)))

(** val eject_obligations_obligation_2 :
    nat -> 'a1 deque -> ('a1 deque, 'a1) prod **)

let eject_obligations_obligation_2 _ _ =
  assert false (* absurd case *)

(** val eject : nat -> 'a1 deque -> ('a1 deque, 'a1) prod **)

let eject size d =
  match option_eject (S size) d with
  | Some p -> p
  | None -> eject_obligations_obligation_2 size d
