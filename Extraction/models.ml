(* # mod # -> commented open Classes *)
open Datatypes
open GYOR
open List
open Nat
open Buffer
open Types

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val concat_map_node'_seq :
    (__ -> nat -> 'a1 -> __ list) -> nat -> nat -> nkind -> color -> 'a1
    node' -> 'a2 list -> 'a2 list **)

let concat_map_node'_seq f lvlt _ _ _ n l =
  match n with
  | Only_end (q, p) -> concat_map_seq f lvlt (S q) p
  | Only (qp, qs, _, _, _, p, s) ->
    app (concat_map_seq f lvlt (add (S (S (S (S (S O))))) qp) p)
      (app l (concat_map_seq f lvlt (add (S (S (S (S (S O))))) qs) s))
  | Left (qp, _, _, _, _, p, s) ->
    app (concat_map_seq f lvlt (add (S (S (S (S (S O))))) qp) p)
      (app l (concat_map_seq f lvlt (S (S O)) s))
  | Right (_, qs, _, _, _, p, s) ->
    app (concat_map_seq f lvlt (S (S O)) p)
      (app l (concat_map_seq f lvlt (add (S (S (S (S (S O))))) qs) s))

(** val stored_triple_seq_functional :
    (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind
    -> nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
    nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat
    -> nkind -> color -> color -> __ chain -> __ list) -> nat -> 'a1
    stored_triple -> 'a1 list **)

let stored_triple_seq_functional stored_triple_seq0 _ _ chain_seq0 _ = function
| Ground y -> Coq_cons (y, Coq_nil)
| Small (lvl, q, s) ->
  concat_map_seq (Obj.magic stored_triple_seq0) lvl (add (S (S (S O))) q) s
| Big (lvl, qp, qs, ck, cl, cr, p, c, s) ->
  app
    (concat_map_seq (Obj.magic stored_triple_seq0) lvl (add (S (S (S O))) qp)
      p)
    (app (Obj.magic chain_seq0 __ (S lvl) ck Coq_only cl cr c)
      (concat_map_seq (Obj.magic stored_triple_seq0) lvl
        (add (S (S (S O))) qs) s))

(** val body_seq_functional :
    (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind
    -> nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
    nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat
    -> nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nkind
    -> nkind -> 'a1 body -> 'a1 list -> 'a1 list **)

let body_seq_functional stored_triple_seq0 body_seq0 _ chain_seq0 _ _ _ _ b l =
  match b with
  | Hole (_, _) -> l
  | Single_child (hlvl, tlvl, hk, tk, y, o, n, b0) ->
    concat_map_node'_seq (Obj.magic stored_triple_seq0) hlvl (S O) hk (Mix
      (NoGreen, y, o, NoRed)) n
      (Obj.magic body_seq0 __ (S hlvl) tlvl Coq_only tk b0 l)
  | Pair_yellow (hlvl, tlvl, hk, tk, c, n, b0, c0) ->
    concat_map_node'_seq (Obj.magic stored_triple_seq0) hlvl (S (S O)) hk
      (Mix (NoGreen, SomeYellow, NoOrange, NoRed)) n
      (app (Obj.magic body_seq0 __ (S hlvl) tlvl Coq_left tk b0 l)
        (Obj.magic chain_seq0 __ (S hlvl) (S O) Coq_right c c c0))
  | Pair_orange (hlvl, tlvl, hk, tk, n, c, b0) ->
    concat_map_node'_seq (Obj.magic stored_triple_seq0) hlvl (S (S O)) hk
      (Mix (NoGreen, NoYellow, SomeOrange, NoRed)) n
      (app
        (Obj.magic chain_seq0 __ (S hlvl) (S O) Coq_left (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) c) (Obj.magic body_seq0 __ (S hlvl) tlvl Coq_right tk b0 l))

(** val packet_seq_functional :
    (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind
    -> nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
    nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat
    -> nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nat
    -> nkind -> color -> 'a1 packet -> 'a1 list -> 'a1 list **)

let packet_seq_functional stored_triple_seq0 body_seq0 _ _ _ _ _ _ _ p l =
  let Packet (hlvl, tlvl, nc, hk, tk, g, r, b, n) = p in
  Obj.magic body_seq0 __ hlvl tlvl hk tk b
    (concat_map_node'_seq (Obj.magic stored_triple_seq0) tlvl nc tk (Mix (g,
      NoYellow, NoOrange, r)) n l)

(** val chain_seq_functional :
    (__ -> nat -> __ stored_triple -> __ list) -> (__ -> nat -> nat -> nkind
    -> nkind -> __ body -> __ list -> __ list) -> (__ -> nat -> nat -> nat ->
    nkind -> color -> __ packet -> __ list -> __ list) -> (__ -> nat -> nat
    -> nkind -> color -> color -> __ chain -> __ list) -> nat -> nat -> nkind
    -> color -> color -> 'a1 chain -> 'a1 list **)

let chain_seq_functional _ _ packet_seq0 chain_seq0 _ _ _ _ _ = function
| Empty _ -> Coq_nil
| Single (hlvl, tlvl, ck, nk, c0, cl, cr, _, p, c1) ->
  Obj.magic packet_seq0 __ hlvl tlvl ck nk c0 p
    (Obj.magic chain_seq0 __ tlvl ck Coq_only cl cr c1)
| Pair (lvl, cl, cr, c0, c1) ->
  app (Obj.magic chain_seq0 __ lvl (S O) Coq_left cl cl c0)
    (Obj.magic chain_seq0 __ lvl (S O) Coq_right cr cr c1)

(** val stored_triple_seq : nat -> 'a1 stored_triple -> 'a1 list **)

let rec stored_triple_seq lvl st =
  stored_triple_seq_functional (Obj.magic (fun _ -> stored_triple_seq))
    (Obj.magic (fun _ -> body_seq)) (Obj.magic (fun _ -> packet_seq))
    (Obj.magic (fun _ -> chain_seq)) lvl st

(** val body_seq :
    nat -> nat -> nkind -> nkind -> 'a1 body -> 'a1 list -> 'a1 list **)

and body_seq hlvl tlvl hk tk b l =
  body_seq_functional (Obj.magic (fun _ -> stored_triple_seq))
    (Obj.magic (fun _ -> body_seq)) (Obj.magic (fun _ -> packet_seq))
    (Obj.magic (fun _ -> chain_seq)) hlvl tlvl hk tk b l

(** val packet_seq :
    nat -> nat -> nat -> nkind -> color -> 'a1 packet -> 'a1 list -> 'a1 list **)

and packet_seq hlvl tlvl nc nk c p l =
  packet_seq_functional (Obj.magic (fun _ -> stored_triple_seq))
    (Obj.magic (fun _ -> body_seq)) (Obj.magic (fun _ -> packet_seq))
    (Obj.magic (fun _ -> chain_seq)) hlvl tlvl nc nk c p l

(** val chain_seq :
    nat -> nat -> nkind -> color -> color -> 'a1 chain -> 'a1 list **)

and chain_seq lvl ck nk cl cr c =
  chain_seq_functional (Obj.magic (fun _ -> stored_triple_seq))
    (Obj.magic (fun _ -> body_seq)) (Obj.magic (fun _ -> packet_seq))
    (Obj.magic (fun _ -> chain_seq)) lvl ck nk cl cr c

type 'a stored_triple_seq_graph =
| Coq_stored_triple_seq_graph_equation_1 of 'a
| Coq_stored_triple_seq_graph_equation_2 of nat * nat
   * 'a stored_triple suffix'
| Coq_stored_triple_seq_graph_equation_3 of nat * nat * nat * nat * color
   * color * 'a stored_triple prefix' * 'a chain * 'a stored_triple suffix'
   * 'a chain_seq_graph
and 'a body_seq_graph =
| Coq_body_seq_graph_equation_1 of nat * nkind * 'a list
| Coq_body_seq_graph_equation_2 of nat * nat * nkind * nkind * yellow_hue
   * orange_hue * 'a stored_triple node' * 'a body * 'a list
   * 'a body_seq_graph
| Coq_body_seq_graph_equation_3 of nat * nat * nkind * nkind * color
   * 'a stored_triple node' * 'a body * 'a chain * 'a list
   * 'a body_seq_graph * 'a chain_seq_graph
| Coq_body_seq_graph_equation_4 of nat * nat * nkind * nkind
   * 'a stored_triple node' * 'a chain * 'a body * 'a list
   * 'a chain_seq_graph * 'a body_seq_graph
and 'a packet_seq_graph =
| Coq_packet_seq_graph_equation_1 of nat * nat * nat * nkind * green_hue
   * red_hue * nkind * 'a body * 'a stored_triple node' * 'a list
   * 'a body_seq_graph
and 'a chain_seq_graph =
| Coq_chain_seq_graph_equation_1 of nat
| Coq_chain_seq_graph_equation_2 of nat * nkind * color * nat * nat *
   color * color * regularity * 'a packet * 'a chain * 'a chain_seq_graph
   * 'a packet_seq_graph
| Coq_chain_seq_graph_equation_3 of nat * color * color * 'a chain *
   'a chain * 'a chain_seq_graph * 'a chain_seq_graph

(** val chain_seq_graph_mut :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> __
    chain_seq_graph -> 'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2)
    -> (__ -> nat -> nat -> nkind -> nkind -> yellow_hue -> orange_hue -> __
    stored_triple node' -> __ body -> __ list -> __ body_seq_graph -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __ stored_triple
    node' -> __ body -> __ chain -> __ list -> __ body_seq_graph -> 'a2 -> __
    chain_seq_graph -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
    __ stored_triple node' -> __ chain -> __ body -> __ list -> __
    chain_seq_graph -> 'a4 -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat
    -> nat -> nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__
    -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color ->
    color -> regularity -> __ packet -> __ chain -> __ chain_seq_graph -> 'a4
    -> __ packet_seq_graph -> 'a3 -> 'a4) -> (__ -> nat -> color -> color ->
    __ chain -> __ chain -> __ chain_seq_graph -> 'a4 -> __ chain_seq_graph
    -> 'a4 -> 'a4) -> nat -> nat -> nkind -> color -> color -> 'a5 chain ->
    'a5 list -> 'a5 chain_seq_graph -> 'a4 **)

let chain_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 lvl ck nk cl cr c l c0 =
  let rec _f10 _ _ _ _ = function (* # mod # -> added _ before f10 *)
  | Coq_stored_triple_seq_graph_equation_1 a -> f __ a
  | Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s0) -> f0 __ lvl0 q s0
  | Coq_stored_triple_seq_graph_equation_3 (lvl1, qp, qs, ck0, cl0, cr0, p,
                                            child, s0, hind) ->
    f1 __ lvl1 qp qs ck0 cl0 cr0 p child s0 hind
      (Obj.magic f13 (S lvl1) ck0 Coq_only cl0 cr0 child
        (chain_seq (S lvl1) ck0 Coq_only cl0 cr0 child) hind)
  and f11 _ _ _ _ _ _ _ _ = function
  | Coq_body_seq_graph_equation_1 (hlvl, hk, l0) -> f2 __ hlvl hk l0
  | Coq_body_seq_graph_equation_2 (hlvl, tlvl, hk, tk, y, o, hd, b, l0, hind) ->
    f3 __ hlvl tlvl hk tk y o hd b l0 hind
      (f11 __ (S hlvl) tlvl Coq_only tk b l0
        (body_seq (S hlvl) tlvl Coq_only tk b l0) hind)
  | Coq_body_seq_graph_equation_3 (hlvl, tlvl, hk, tk, c1, hd, b, cr0, l0,
                                   hind, hind0) ->
    f4 __ hlvl tlvl hk tk c1 hd b cr0 l0 hind
      (f11 __ (S hlvl) tlvl Coq_left tk b l0
        (body_seq (S hlvl) tlvl Coq_left tk b l0) hind) hind0
      (Obj.magic f13 (S hlvl) (S O) Coq_right c1 c1 cr0
        (chain_seq (S hlvl) (S O) Coq_right c1 c1 cr0) hind0)
  | Coq_body_seq_graph_equation_4 (hlvl, tlvl, hk, tk, hd, cl0, b, l0, hind,
                                   hind0) ->
    f5 __ hlvl tlvl hk tk hd cl0 b l0 hind
      (Obj.magic f13 (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl0
        (chain_seq (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl0)
        hind) hind0
      (f11 __ (S hlvl) tlvl Coq_right tk b l0
        (body_seq (S hlvl) tlvl Coq_right tk b l0) hind0)
  and f12 _ _ _ _ _ _ _ _ _ = function
  | Coq_packet_seq_graph_equation_1 (hlvl, tlvl0, nc, nk0, g, r, tk, b, tl,
                                     l0, hind) ->
    f6 __ hlvl tlvl0 nc nk0 g r tk b tl l0 hind
      (f11 __ hlvl tlvl0 nk0 tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk (Mix
          (g, NoYellow, NoOrange, r)) tl l0)
        (body_seq hlvl tlvl0 nk0 tk b
          (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk (Mix
            (g, NoYellow, NoOrange, r)) tl l0)) hind)
  and f13 _ _ _ _ _ _ _ = function
  | Coq_chain_seq_graph_equation_1 lvl0 -> f7 __ lvl0
  | Coq_chain_seq_graph_equation_2 (lvl0, nk0, cl0, tlvl, ck0, cl1, cr0, r,
                                    pkt, rest, hind, hind0) ->
    Obj.magic f8 __ lvl0 nk0 cl0 tlvl ck0 cl1 cr0 r pkt rest hind
      (f13 tlvl ck0 Coq_only cl1 cr0 rest
        (chain_seq tlvl ck0 Coq_only cl1 cr0 rest) hind) hind0
      (Obj.magic f12 __ lvl0 tlvl ck0 nk0 cl0 pkt
        (chain_seq tlvl ck0 Coq_only cl1 cr0 rest)
        (packet_seq lvl0 tlvl ck0 nk0 cl0 pkt
          (chain_seq tlvl ck0 Coq_only cl1 cr0 rest)) hind0)
  | Coq_chain_seq_graph_equation_3 (lvl0, cl0, cr0, cl1, cr1, hind, hind0) ->
    Obj.magic f9 __ lvl0 cl0 cr0 cl1 cr1 hind
      (f13 lvl0 (S O) Coq_left cl0 cl0 cl1
        (chain_seq lvl0 (S O) Coq_left cl0 cl0 cl1) hind) hind0
      (f13 lvl0 (S O) Coq_right cr0 cr0 cr1
        (chain_seq lvl0 (S O) Coq_right cr0 cr0 cr1) hind0)
  in f13 lvl ck nk cl cr c l c0

(** val packet_seq_graph_mut :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> __
    chain_seq_graph -> 'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2)
    -> (__ -> nat -> nat -> nkind -> nkind -> yellow_hue -> orange_hue -> __
    stored_triple node' -> __ body -> __ list -> __ body_seq_graph -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __ stored_triple
    node' -> __ body -> __ chain -> __ list -> __ body_seq_graph -> 'a2 -> __
    chain_seq_graph -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
    __ stored_triple node' -> __ chain -> __ body -> __ list -> __
    chain_seq_graph -> 'a4 -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat
    -> nat -> nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__
    -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color ->
    color -> regularity -> __ packet -> __ chain -> __ chain_seq_graph -> 'a4
    -> __ packet_seq_graph -> 'a3 -> 'a4) -> (__ -> nat -> color -> color ->
    __ chain -> __ chain -> __ chain_seq_graph -> 'a4 -> __ chain_seq_graph
    -> 'a4 -> 'a4) -> nat -> nat -> nat -> nkind -> color -> 'a5 packet ->
    'a5 list -> 'a5 list -> 'a5 packet_seq_graph -> 'a3 **)

let packet_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 hlvl tlvl nc nk c p l l0 p0 =
  let rec _f10 _ _ _ _ = function (* # mod # -> added _ before f10 *)
  | Coq_stored_triple_seq_graph_equation_1 a -> f __ a
  | Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s0) -> f0 __ lvl0 q s0
  | Coq_stored_triple_seq_graph_equation_3 (lvl1, qp, qs, ck, cl, cr, p1,
                                            child, s0, hind) ->
    f1 __ lvl1 qp qs ck cl cr p1 child s0 hind
      (f13 __ (S lvl1) ck Coq_only cl cr child
        (chain_seq (S lvl1) ck Coq_only cl cr child) hind)
  and f11 _ _ _ _ _ _ _ _ = function
  | Coq_body_seq_graph_equation_1 (hlvl0, hk, l1) -> f2 __ hlvl0 hk l1
  | Coq_body_seq_graph_equation_2 (hlvl0, tlvl0, hk, tk, y, o, hd, b, l1, hind) ->
    f3 __ hlvl0 tlvl0 hk tk y o hd b l1 hind
      (f11 __ (S hlvl0) tlvl0 Coq_only tk b l1
        (body_seq (S hlvl0) tlvl0 Coq_only tk b l1) hind)
  | Coq_body_seq_graph_equation_3 (hlvl0, tlvl0, hk, tk, c0, hd, b, cr, l1,
                                   hind, hind0) ->
    f4 __ hlvl0 tlvl0 hk tk c0 hd b cr l1 hind
      (f11 __ (S hlvl0) tlvl0 Coq_left tk b l1
        (body_seq (S hlvl0) tlvl0 Coq_left tk b l1) hind) hind0
      (f13 __ (S hlvl0) (S O) Coq_right c0 c0 cr
        (chain_seq (S hlvl0) (S O) Coq_right c0 c0 cr) hind0)
  | Coq_body_seq_graph_equation_4 (hlvl0, tlvl0, hk, tk, hd, cl, b, l1, hind,
                                   hind0) ->
    f5 __ hlvl0 tlvl0 hk tk hd cl b l1 hind
      (f13 __ (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl
        (chain_seq (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl)
        hind) hind0
      (f11 __ (S hlvl0) tlvl0 Coq_right tk b l1
        (body_seq (S hlvl0) tlvl0 Coq_right tk b l1) hind0)
  and f12 _ _ _ _ _ _ _ _ = function
  | Coq_packet_seq_graph_equation_1 (hlvl0, tlvl0, nc0, nk0, g, r, tk, b, tl,
                                     l1, hind) ->
    Obj.magic f6 __ hlvl0 tlvl0 nc0 nk0 g r tk b tl l1 hind
      (Obj.magic f11 __ hlvl0 tlvl0 nk0 tk b
        (concat_map_node'_seq (Obj.magic (fun _ -> stored_triple_seq)) tlvl0
          nc0 tk (Mix (g, NoYellow, NoOrange, r)) tl l1)
        (body_seq hlvl0 tlvl0 nk0 tk b
          (concat_map_node'_seq (Obj.magic (fun _ -> stored_triple_seq))
            tlvl0 nc0 tk (Mix (g, NoYellow, NoOrange, r)) tl l1)) hind)
  and f13 _ _ _ _ _ _ _ _ = function
  | Coq_chain_seq_graph_equation_1 lvl -> f7 __ lvl
  | Coq_chain_seq_graph_equation_2 (lvl, nk0, cl, tlvl0, ck0, cl0, cr0, r,
                                    pkt, rest, hind, hind0) ->
    f8 __ lvl nk0 cl tlvl0 ck0 cl0 cr0 r pkt rest hind
      (f13 __ tlvl0 ck0 Coq_only cl0 cr0 rest
        (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest) hind) hind0
      (Obj.magic f12 lvl tlvl0 ck0 nk0 cl pkt
        (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest)
        (packet_seq lvl tlvl0 ck0 nk0 cl pkt
          (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest)) hind0)
  | Coq_chain_seq_graph_equation_3 (lvl, cl, cr, cl0, cr0, hind, hind0) ->
    f9 __ lvl cl cr cl0 cr0 hind
      (f13 __ lvl (S O) Coq_left cl cl cl0
        (chain_seq lvl (S O) Coq_left cl cl cl0) hind) hind0
      (f13 __ lvl (S O) Coq_right cr cr cr0
        (chain_seq lvl (S O) Coq_right cr cr cr0) hind0)
  in f12 hlvl tlvl nc nk c p l l0 p0

(** val body_seq_graph_mut :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> __
    chain_seq_graph -> 'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2)
    -> (__ -> nat -> nat -> nkind -> nkind -> yellow_hue -> orange_hue -> __
    stored_triple node' -> __ body -> __ list -> __ body_seq_graph -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __ stored_triple
    node' -> __ body -> __ chain -> __ list -> __ body_seq_graph -> 'a2 -> __
    chain_seq_graph -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
    __ stored_triple node' -> __ chain -> __ body -> __ list -> __
    chain_seq_graph -> 'a4 -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat
    -> nat -> nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__
    -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color ->
    color -> regularity -> __ packet -> __ chain -> __ chain_seq_graph -> 'a4
    -> __ packet_seq_graph -> 'a3 -> 'a4) -> (__ -> nat -> color -> color ->
    __ chain -> __ chain -> __ chain_seq_graph -> 'a4 -> __ chain_seq_graph
    -> 'a4 -> 'a4) -> nat -> nat -> nkind -> nkind -> 'a5 body -> 'a5 list ->
    'a5 list -> 'a5 body_seq_graph -> 'a2 **)

let body_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 hlvl tlvl hk tk b l l0 b0 =
  let rec _f10 _ _ _ _ = function (* # mod # -> added _ before f10 *)
  | Coq_stored_triple_seq_graph_equation_1 a -> f __ a
  | Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s0) -> f0 __ lvl0 q s0
  | Coq_stored_triple_seq_graph_equation_3 (lvl1, qp, qs, ck, cl, cr, p,
                                            child, s0, hind) ->
    f1 __ lvl1 qp qs ck cl cr p child s0 hind
      (f13 __ (S lvl1) ck Coq_only cl cr child
        (chain_seq (S lvl1) ck Coq_only cl cr child) hind)
  and f11 _ _ _ _ _ _ _ = function
  | Coq_body_seq_graph_equation_1 (hlvl0, hk0, l1) ->
    Obj.magic f2 __ hlvl0 hk0 l1
  | Coq_body_seq_graph_equation_2 (hlvl0, tlvl0, hk0, tk0, y, o, hd, b2, l1,
                                   hind) ->
    Obj.magic f3 __ hlvl0 tlvl0 hk0 tk0 y o hd b2 l1 hind
      (f11 (S hlvl0) tlvl0 Coq_only tk0 b2 l1
        (body_seq (S hlvl0) tlvl0 Coq_only tk0 b2 l1) hind)
  | Coq_body_seq_graph_equation_3 (hlvl0, tlvl0, hk0, tk0, c, hd, b2, cr, l1,
                                   hind, hind0) ->
    Obj.magic f4 __ hlvl0 tlvl0 hk0 tk0 c hd b2 cr l1 hind
      (f11 (S hlvl0) tlvl0 Coq_left tk0 b2 l1
        (body_seq (S hlvl0) tlvl0 Coq_left tk0 b2 l1) hind) hind0
      (Obj.magic f13 __ (S hlvl0) (S O) Coq_right c c cr
        (chain_seq (S hlvl0) (S O) Coq_right c c cr) hind0)
  | Coq_body_seq_graph_equation_4 (hlvl0, tlvl0, hk0, tk0, hd, cl, b2, l1,
                                   hind, hind0) ->
    Obj.magic f5 __ hlvl0 tlvl0 hk0 tk0 hd cl b2 l1 hind
      (Obj.magic f13 __ (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl
        (chain_seq (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl)
        hind) hind0
      (f11 (S hlvl0) tlvl0 Coq_right tk0 b2 l1
        (body_seq (S hlvl0) tlvl0 Coq_right tk0 b2 l1) hind0)
  and f12 _ _ _ _ _ _ _ _ _ = function
  | Coq_packet_seq_graph_equation_1 (hlvl0, tlvl0, nc, nk, g, r, tk0, b1, tl,
                                     l1, hind) ->
    f6 __ hlvl0 tlvl0 nc nk g r tk0 b1 tl l1 hind
      (Obj.magic f11 hlvl0 tlvl0 nk tk0 b1
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk0 (Mix
          (g, NoYellow, NoOrange, r)) tl l1)
        (body_seq hlvl0 tlvl0 nk tk0 b1
          (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk0
            (Mix (g, NoYellow, NoOrange, r)) tl l1)) hind)
  and f13 _ _ _ _ _ _ _ _ = function
  | Coq_chain_seq_graph_equation_1 lvl -> f7 __ lvl
  | Coq_chain_seq_graph_equation_2 (lvl, nk, cl, tlvl0, ck0, cl0, cr0, r,
                                    pkt, rest, hind, hind0) ->
    f8 __ lvl nk cl tlvl0 ck0 cl0 cr0 r pkt rest hind
      (f13 __ tlvl0 ck0 Coq_only cl0 cr0 rest
        (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest) hind) hind0
      (f12 __ lvl tlvl0 ck0 nk cl pkt
        (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest)
        (packet_seq lvl tlvl0 ck0 nk cl pkt
          (chain_seq tlvl0 ck0 Coq_only cl0 cr0 rest)) hind0)
  | Coq_chain_seq_graph_equation_3 (lvl, cl, cr, cl0, cr0, hind, hind0) ->
    f9 __ lvl cl cr cl0 cr0 hind
      (f13 __ lvl (S O) Coq_left cl cl cl0
        (chain_seq lvl (S O) Coq_left cl cl cl0) hind) hind0
      (f13 __ lvl (S O) Coq_right cr cr cr0
        (chain_seq lvl (S O) Coq_right cr cr cr0) hind0)
  in f11 hlvl tlvl hk tk b l l0 b0

(** val stored_triple_seq_graph_mut :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> __
    chain_seq_graph -> 'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2)
    -> (__ -> nat -> nat -> nkind -> nkind -> yellow_hue -> orange_hue -> __
    stored_triple node' -> __ body -> __ list -> __ body_seq_graph -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __ stored_triple
    node' -> __ body -> __ chain -> __ list -> __ body_seq_graph -> 'a2 -> __
    chain_seq_graph -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
    __ stored_triple node' -> __ chain -> __ body -> __ list -> __
    chain_seq_graph -> 'a4 -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat
    -> nat -> nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__
    -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color ->
    color -> regularity -> __ packet -> __ chain -> __ chain_seq_graph -> 'a4
    -> __ packet_seq_graph -> 'a3 -> 'a4) -> (__ -> nat -> color -> color ->
    __ chain -> __ chain -> __ chain_seq_graph -> 'a4 -> __ chain_seq_graph
    -> 'a4 -> 'a4) -> nat -> 'a5 stored_triple -> 'a5 list -> 'a5
    stored_triple_seq_graph -> 'a1 **)

let stored_triple_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 lvl st l s =
  let rec f10 _ _ _ = function
  | Coq_stored_triple_seq_graph_equation_1 a -> Obj.magic f __ a
  | Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s1) ->
    Obj.magic f0 __ lvl0 q s1
  | Coq_stored_triple_seq_graph_equation_3 (lvl1, qp, qs, ck, cl, cr, p,
                                            child, s1, hind) ->
    Obj.magic f1 __ lvl1 qp qs ck cl cr p child s1 hind
      (f13 __ (S lvl1) ck Coq_only cl cr child
        (chain_seq (S lvl1) ck Coq_only cl cr child) hind)
  and f11 _ _ _ _ _ _ _ _ = function
  | Coq_body_seq_graph_equation_1 (hlvl, hk, l0) -> f2 __ hlvl hk l0
  | Coq_body_seq_graph_equation_2 (hlvl, tlvl, hk, tk, y, o, hd, b, l0, hind) ->
    f3 __ hlvl tlvl hk tk y o hd b l0 hind
      (f11 __ (S hlvl) tlvl Coq_only tk b l0
        (body_seq (S hlvl) tlvl Coq_only tk b l0) hind)
  | Coq_body_seq_graph_equation_3 (hlvl, tlvl, hk, tk, c, hd, b, cr, l0,
                                   hind, hind0) ->
    f4 __ hlvl tlvl hk tk c hd b cr l0 hind
      (f11 __ (S hlvl) tlvl Coq_left tk b l0
        (body_seq (S hlvl) tlvl Coq_left tk b l0) hind) hind0
      (Obj.magic f13 __ (S hlvl) (S O) Coq_right c c cr
        (chain_seq (S hlvl) (S O) Coq_right c c cr) hind0)
  | Coq_body_seq_graph_equation_4 (hlvl, tlvl, hk, tk, hd, cl, b, l0, hind,
                                   hind0) ->
    f5 __ hlvl tlvl hk tk hd cl b l0 hind
      (Obj.magic f13 __ (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl
        (chain_seq (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) cl)
        hind) hind0
      (f11 __ (S hlvl) tlvl Coq_right tk b l0
        (body_seq (S hlvl) tlvl Coq_right tk b l0) hind0)
  and f12 _ _ _ _ _ _ _ _ _ = function
  | Coq_packet_seq_graph_equation_1 (hlvl, tlvl0, nc, nk, g, r, tk, b, tl,
                                     l0, hind) ->
    f6 __ hlvl tlvl0 nc nk g r tk b tl l0 hind
      (f11 __ hlvl tlvl0 nk tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk (Mix
          (g, NoYellow, NoOrange, r)) tl l0)
        (body_seq hlvl tlvl0 nk tk b
          (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl0 nc tk (Mix
            (g, NoYellow, NoOrange, r)) tl l0)) hind)
  and f13 _ _ _ _ _ _ _ _ = function
  | Coq_chain_seq_graph_equation_1 lvl0 -> f7 __ lvl0
  | Coq_chain_seq_graph_equation_2 (lvl0, nk, cl, tlvl, ck0, cl0, cr0, r,
                                    pkt, rest, hind, hind0) ->
    Obj.magic f8 __ lvl0 nk cl tlvl ck0 cl0 cr0 r pkt rest hind
      (f13 __ tlvl ck0 Coq_only cl0 cr0 rest
        (chain_seq tlvl ck0 Coq_only cl0 cr0 rest) hind) hind0
      (Obj.magic f12 __ lvl0 tlvl ck0 nk cl pkt
        (chain_seq tlvl ck0 Coq_only cl0 cr0 rest)
        (packet_seq lvl0 tlvl ck0 nk cl pkt
          (chain_seq tlvl ck0 Coq_only cl0 cr0 rest)) hind0)
  | Coq_chain_seq_graph_equation_3 (lvl0, cl, cr, cl0, cr0, hind, hind0) ->
    Obj.magic f9 __ lvl0 cl cr cl0 cr0 hind
      (f13 __ lvl0 (S O) Coq_left cl cl cl0
        (chain_seq lvl0 (S O) Coq_left cl cl cl0) hind) hind0
      (f13 __ lvl0 (S O) Coq_right cr cr cr0
        (chain_seq lvl0 (S O) Coq_right cr cr cr0) hind0)
  in f10 lvl st l s

(** val stored_triple_seq_graph_rect :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> __
    chain_seq_graph -> 'a4 -> 'a1) -> (__ -> nat -> nkind -> __ list -> 'a2)
    -> (__ -> nat -> nat -> nkind -> nkind -> yellow_hue -> orange_hue -> __
    stored_triple node' -> __ body -> __ list -> __ body_seq_graph -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __ stored_triple
    node' -> __ body -> __ chain -> __ list -> __ body_seq_graph -> 'a2 -> __
    chain_seq_graph -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind ->
    __ stored_triple node' -> __ chain -> __ body -> __ list -> __
    chain_seq_graph -> 'a4 -> __ body_seq_graph -> 'a2 -> 'a2) -> (__ -> nat
    -> nat -> nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ body_seq_graph -> 'a2 -> 'a3) -> (__
    -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color ->
    color -> regularity -> __ packet -> __ chain -> __ chain_seq_graph -> 'a4
    -> __ packet_seq_graph -> 'a3 -> 'a4) -> (__ -> nat -> color -> color ->
    __ chain -> __ chain -> __ chain_seq_graph -> 'a4 -> __ chain_seq_graph
    -> 'a4 -> 'a4) -> (__ -> nat -> __ stored_triple -> __ list -> __
    stored_triple_seq_graph -> 'a1, (__ -> nat -> nat -> nkind -> nkind -> __
    body -> __ list -> __ list -> __ body_seq_graph -> 'a2, (__ -> nat -> nat
    -> nat -> nkind -> color -> __ packet -> __ list -> __ list -> __
    packet_seq_graph -> 'a3, __ -> nat -> nat -> nkind -> color -> color ->
    __ chain -> __ list -> __ chain_seq_graph -> 'a4) prod) prod) prod **)

let stored_triple_seq_graph_rect f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  Coq_pair ((fun _ ->
    stored_triple_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9), (Coq_pair
    ((fun _ -> body_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9), (Coq_pair
    ((fun _ -> packet_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9),
    (fun _ -> chain_seq_graph_mut f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9))))))

(** val stored_triple_seq_graph_correct :
    (__ -> nat -> __ stored_triple -> __ stored_triple_seq_graph, (__ -> nat
    -> nat -> nkind -> nkind -> __ body -> __ list -> __ body_seq_graph, (__
    -> nat -> nat -> nat -> nkind -> color -> __ packet -> __ list -> __
    packet_seq_graph, __ -> nat -> nat -> nkind -> color -> color -> __ chain
    -> __ chain_seq_graph) prod) prod) prod **)

let stored_triple_seq_graph_correct =
  Coq_pair ((fun _ lvl ->
    let rec fix_0 _ = function
    | Ground y -> Coq_stored_triple_seq_graph_equation_1 y
    | Small (lvl0, q, s) ->
      Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s)
    | Big (lvl0, qp, qs, ck, cl, cr, p, c, s) ->
      Coq_stored_triple_seq_graph_equation_3 (lvl0, qp, qs, ck, cl, cr, p, c,
        s, (fix_3 __ (S lvl0) ck Coq_only cl cr c))
    and fix_1 _ _ _ _ _ b l =
      match b with
      | Hole (lvl0, k) -> Coq_body_seq_graph_equation_1 (lvl0, k, l)
      | Single_child (hlvl, tlvl, hk, tk, y, o, n, b0) ->
        Coq_body_seq_graph_equation_2 (hlvl, tlvl, hk, tk, y, o, n, b0, l,
          (fix_1 __ (S hlvl) tlvl Coq_only tk b0 l))
      | Pair_yellow (hlvl, tlvl, hk, tk, c, n, b0, c0) ->
        Coq_body_seq_graph_equation_3 (hlvl, tlvl, hk, tk, c, n, b0, c0, l,
          (fix_1 __ (S hlvl) tlvl Coq_left tk b0 l),
          (fix_3 __ (S hlvl) (S O) Coq_right c c c0))
      | Pair_orange (hlvl, tlvl, hk, tk, n, c, b0) ->
        Coq_body_seq_graph_equation_4 (hlvl, tlvl, hk, tk, n, c, b0, l,
          (fix_3 __ (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow,
            NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c),
          (fix_1 __ (S hlvl) tlvl Coq_right tk b0 l))
    and fix_2 _ _ _ _ _ _ p l =
      let Packet (hlvl, tlvl, nc, hk, tk, g, r, b, n) = p in
      Coq_packet_seq_graph_equation_1 (hlvl, tlvl, nc, hk, g, r, tk, b, n, l,
      (fix_1 __ hlvl tlvl hk tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl nc tk (Mix
          (g, NoYellow, NoOrange, r)) n l)))
    and fix_3 _ _ _ _ _ _ = function
    | Empty lvl0 -> Coq_chain_seq_graph_equation_1 lvl0
    | Single (hlvl, tlvl, ck, nk, c0, cl, cr, r, p, c1) ->
      Coq_chain_seq_graph_equation_2 (hlvl, nk, c0, tlvl, ck, cl, cr, r, p,
        c1, (fix_3 __ tlvl ck Coq_only cl cr c1),
        (fix_2 __ hlvl tlvl ck nk c0 p (chain_seq tlvl ck Coq_only cl cr c1)))
    | Pair (lvl0, cl, cr, c0, c1) ->
      Coq_chain_seq_graph_equation_3 (lvl0, cl, cr, c0, c1,
        (fix_3 __ lvl0 (S O) Coq_left cl cl c0),
        (fix_3 __ lvl0 (S O) Coq_right cr cr c1))
    in fix_0 lvl), (Coq_pair ((fun _ hlvl ->
    let rec _fix_0 _ _ = function (* # mod # -> added _ before fix_0 *)
    | Ground y -> Coq_stored_triple_seq_graph_equation_1 y
    | Small (lvl, q, s) -> Coq_stored_triple_seq_graph_equation_2 (lvl, q, s)
    | Big (lvl, qp, qs, ck, cl, cr, p, c, s) ->
      Coq_stored_triple_seq_graph_equation_3 (lvl, qp, qs, ck, cl, cr, p, c,
        s, (fix_3 __ (S lvl) ck Coq_only cl cr c))
    and fix_1 _ _ _ _ b l =
      match b with
      | Hole (lvl, k) -> Coq_body_seq_graph_equation_1 (lvl, k, l)
      | Single_child (hlvl0, tlvl, hk, tk, y, o, n, b0) ->
        Coq_body_seq_graph_equation_2 (hlvl0, tlvl, hk, tk, y, o, n, b0, l,
          (fix_1 (S hlvl0) tlvl Coq_only tk b0 l))
      | Pair_yellow (hlvl0, tlvl, hk, tk, c, n, b0, c0) ->
        Coq_body_seq_graph_equation_3 (hlvl0, tlvl, hk, tk, c, n, b0, c0, l,
          (fix_1 (S hlvl0) tlvl Coq_left tk b0 l),
          (fix_3 __ (S hlvl0) (S O) Coq_right c c c0))
      | Pair_orange (hlvl0, tlvl, hk, tk, n, c, b0) ->
        Coq_body_seq_graph_equation_4 (hlvl0, tlvl, hk, tk, n, c, b0, l,
          (fix_3 __ (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow,
            NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c),
          (fix_1 (S hlvl0) tlvl Coq_right tk b0 l))
    and fix_2 _ _ _ _ _ _ p l =
      let Packet (hlvl0, tlvl, nc, hk, tk, g, r, b, n) = p in
      Coq_packet_seq_graph_equation_1 (hlvl0, tlvl, nc, hk, g, r, tk, b, n,
      l,
      (fix_1 hlvl0 tlvl hk tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl nc tk (Mix
          (g, NoYellow, NoOrange, r)) n l)))
    and fix_3 _ _ _ _ _ _ = function
    | Empty lvl -> Coq_chain_seq_graph_equation_1 lvl
    | Single (hlvl0, tlvl, ck, nk, c0, cl, cr, r, p, c1) ->
      Coq_chain_seq_graph_equation_2 (hlvl0, nk, c0, tlvl, ck, cl, cr, r, p,
        c1, (fix_3 __ tlvl ck Coq_only cl cr c1),
        (fix_2 __ hlvl0 tlvl ck nk c0 p (chain_seq tlvl ck Coq_only cl cr c1)))
    | Pair (lvl, cl, cr, c0, c1) ->
      Coq_chain_seq_graph_equation_3 (lvl, cl, cr, c0, c1,
        (fix_3 __ lvl (S O) Coq_left cl cl c0),
        (fix_3 __ lvl (S O) Coq_right cr cr c1))
    in fix_1 hlvl), (Coq_pair ((fun _ hlvl ->
    let rec _fix_0 _ _ = function (* # mod # -> added _ before fix_0 *)
    | Ground y -> Coq_stored_triple_seq_graph_equation_1 y
    | Small (lvl, q, s) -> Coq_stored_triple_seq_graph_equation_2 (lvl, q, s)
    | Big (lvl, qp, qs, ck, cl, cr, p, c, s) ->
      Coq_stored_triple_seq_graph_equation_3 (lvl, qp, qs, ck, cl, cr, p, c,
        s, (fix_3 __ (S lvl) ck Coq_only cl cr c))
    and fix_1 _ _ _ _ _ b l =
      match b with
      | Hole (lvl, k) -> Coq_body_seq_graph_equation_1 (lvl, k, l)
      | Single_child (hlvl0, tlvl, hk, tk, y, o, n, b0) ->
        Coq_body_seq_graph_equation_2 (hlvl0, tlvl, hk, tk, y, o, n, b0, l,
          (fix_1 __ (S hlvl0) tlvl Coq_only tk b0 l))
      | Pair_yellow (hlvl0, tlvl, hk, tk, c, n, b0, c0) ->
        Coq_body_seq_graph_equation_3 (hlvl0, tlvl, hk, tk, c, n, b0, c0, l,
          (fix_1 __ (S hlvl0) tlvl Coq_left tk b0 l),
          (fix_3 __ (S hlvl0) (S O) Coq_right c c c0))
      | Pair_orange (hlvl0, tlvl, hk, tk, n, c, b0) ->
        Coq_body_seq_graph_equation_4 (hlvl0, tlvl, hk, tk, n, c, b0, l,
          (fix_3 __ (S hlvl0) (S O) Coq_left (Mix (SomeGreen, NoYellow,
            NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c),
          (fix_1 __ (S hlvl0) tlvl Coq_right tk b0 l))
    and fix_2 _ _ _ _ _ p l =
      let Packet (hlvl0, tlvl, nc, hk, tk, g, r, b, n) = p in
      Coq_packet_seq_graph_equation_1 (hlvl0, tlvl, nc, hk, g, r, tk, b, n,
      l,
      (fix_1 __ hlvl0 tlvl hk tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl nc tk (Mix
          (g, NoYellow, NoOrange, r)) n l)))
    and fix_3 _ _ _ _ _ _ = function
    | Empty lvl -> Coq_chain_seq_graph_equation_1 lvl
    | Single (hlvl0, tlvl, ck, nk, c0, cl, cr, r, p, c1) ->
      Coq_chain_seq_graph_equation_2 (hlvl0, nk, c0, tlvl, ck, cl, cr, r, p,
        c1, (fix_3 __ tlvl ck Coq_only cl cr c1),
        (fix_2 hlvl0 tlvl ck nk c0 p (chain_seq tlvl ck Coq_only cl cr c1)))
    | Pair (lvl, cl, cr, c0, c1) ->
      Coq_chain_seq_graph_equation_3 (lvl, cl, cr, c0, c1,
        (fix_3 __ lvl (S O) Coq_left cl cl c0),
        (fix_3 __ lvl (S O) Coq_right cr cr c1))
    in fix_2 hlvl), (fun _ lvl ->
    let rec _fix_0 _ _ = function (* # mod # -> added _ before fix_0 *)
    | Ground y -> Coq_stored_triple_seq_graph_equation_1 y
    | Small (lvl0, q, s) ->
      Coq_stored_triple_seq_graph_equation_2 (lvl0, q, s)
    | Big (lvl0, qp, qs, ck, cl, cr, p, c, s) ->
      Coq_stored_triple_seq_graph_equation_3 (lvl0, qp, qs, ck, cl, cr, p, c,
        s, (fix_3 (S lvl0) ck Coq_only cl cr c))
    and fix_1 _ _ _ _ _ b l =
      match b with
      | Hole (lvl0, k) -> Coq_body_seq_graph_equation_1 (lvl0, k, l)
      | Single_child (hlvl, tlvl, hk, tk, y, o, n, b0) ->
        Coq_body_seq_graph_equation_2 (hlvl, tlvl, hk, tk, y, o, n, b0, l,
          (fix_1 __ (S hlvl) tlvl Coq_only tk b0 l))
      | Pair_yellow (hlvl, tlvl, hk, tk, c, n, b0, c0) ->
        Coq_body_seq_graph_equation_3 (hlvl, tlvl, hk, tk, c, n, b0, c0, l,
          (fix_1 __ (S hlvl) tlvl Coq_left tk b0 l),
          (fix_3 (S hlvl) (S O) Coq_right c c c0))
      | Pair_orange (hlvl, tlvl, hk, tk, n, c, b0) ->
        Coq_body_seq_graph_equation_4 (hlvl, tlvl, hk, tk, n, c, b0, l,
          (fix_3 (S hlvl) (S O) Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c),
          (fix_1 __ (S hlvl) tlvl Coq_right tk b0 l))
    and fix_2 _ _ _ _ _ _ p l =
      let Packet (hlvl, tlvl, nc, hk, tk, g, r, b, n) = p in
      Coq_packet_seq_graph_equation_1 (hlvl, tlvl, nc, hk, g, r, tk, b, n, l,
      (fix_1 __ hlvl tlvl hk tk b
        (concat_map_node'_seq (fun _ -> stored_triple_seq) tlvl nc tk (Mix
          (g, NoYellow, NoOrange, r)) n l)))
    and fix_3 _ _ _ _ _ = function
    | Empty lvl0 -> Coq_chain_seq_graph_equation_1 lvl0
    | Single (hlvl, tlvl, ck, nk, c0, cl, cr, r, p, c1) ->
      Coq_chain_seq_graph_equation_2 (hlvl, nk, c0, tlvl, ck, cl, cr, r, p,
        c1, (fix_3 tlvl ck Coq_only cl cr c1),
        (fix_2 __ hlvl tlvl ck nk c0 p (chain_seq tlvl ck Coq_only cl cr c1)))
    | Pair (lvl0, cl, cr, c0, c1) ->
      Coq_chain_seq_graph_equation_3 (lvl0, cl, cr, c0, c1,
        (fix_3 lvl0 (S O) Coq_left cl cl c0),
        (fix_3 lvl0 (S O) Coq_right cr cr c1))
    in fix_3 lvl))))))

(** val stored_triple_seq_elim :
    (__ -> __ -> 'a1) -> (__ -> nat -> nat -> __ stored_triple suffix' ->
    'a1) -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __
    stored_triple prefix' -> __ chain -> __ stored_triple suffix' -> 'a4 ->
    'a1) -> (__ -> nat -> nkind -> __ list -> 'a2) -> (__ -> nat -> nat ->
    nkind -> nkind -> yellow_hue -> orange_hue -> __ stored_triple node' ->
    __ body -> __ list -> 'a2 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind
    -> color -> __ stored_triple node' -> __ body -> __ chain -> __ list ->
    'a2 -> 'a4 -> 'a2) -> (__ -> nat -> nat -> nkind -> nkind -> __
    stored_triple node' -> __ chain -> __ body -> __ list -> 'a4 -> 'a2 ->
    'a2) -> (__ -> nat -> nat -> nat -> nkind -> green_hue -> red_hue ->
    nkind -> __ body -> __ stored_triple node' -> __ list -> 'a2 -> 'a3) ->
    (__ -> nat -> 'a4) -> (__ -> nat -> nkind -> color -> nat -> nat -> color
    -> color -> regularity -> __ packet -> __ chain -> 'a4 -> 'a3 -> 'a4) ->
    (__ -> nat -> color -> color -> __ chain -> __ chain -> 'a4 -> 'a4 ->
    'a4) -> (__ -> nat -> __ stored_triple -> 'a1, (__ -> nat -> nat -> nkind
    -> nkind -> __ body -> __ list -> 'a2, (__ -> nat -> nat -> nat -> nkind
    -> color -> __ packet -> __ list -> 'a3, __ -> nat -> nat -> nkind ->
    color -> color -> __ chain -> 'a4) prod) prod) prod **)

let stored_triple_seq_elim f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 =
  let h =
    let f10 = fun lvl1 qp qs ck cl cr p child s _ ->
      f1 __ lvl1 qp qs ck cl cr p child s
    in
    let f11 = fun hlvl tlvl hk tk y o hd b l _ ->
      f3 __ hlvl tlvl hk tk y o hd b l
    in
    let f12 = fun hlvl tlvl hk tk c hd b cr l _ x _ ->
      f4 __ hlvl tlvl hk tk c hd b cr l x
    in
    let f13 = fun hlvl tlvl hk tk hd cl b l _ x _ ->
      f5 __ hlvl tlvl hk tk hd cl b l x
    in
    let f14 = fun hlvl tlvl0 nc nk g r tk b tl l _ ->
      f6 __ hlvl tlvl0 nc nk g r tk b tl l
    in
    let f15 = fun lvl nk cl tlvl ck0 cl0 cr0 r pkt rest _ x _ ->
      f8 __ lvl nk cl tlvl ck0 cl0 cr0 r pkt rest x
    in
    let f16 = fun lvl cl cr cl0 cr0 _ x _ -> f9 __ lvl cl cr cl0 cr0 x in
    Coq_pair ((fun _ ->
    stored_triple_seq_graph_mut f f0 (fun _ -> f10) f2 (fun _ -> f11)
      (fun _ -> f12) (fun _ -> f13) (fun _ -> f14) f7 (fun _ -> f15)
      (fun _ -> f16)), (Coq_pair ((fun _ ->
    body_seq_graph_mut f f0 (fun _ -> f10) f2 (fun _ -> f11) (fun _ -> f12)
      (fun _ -> f13) (fun _ -> f14) f7 (fun _ -> f15) (fun _ -> f16)),
    (Coq_pair ((fun _ ->
    packet_seq_graph_mut f f0 (fun _ -> f10) f2 (fun _ -> f11) (fun _ -> f12)
      (fun _ -> f13) (fun _ -> f14) f7 (fun _ -> f15) (fun _ -> f16)),
    (fun _ ->
    chain_seq_graph_mut f f0 (fun _ -> f10) f2 (fun _ -> f11) (fun _ -> f12)
      (fun _ -> f13) (fun _ -> f14) f7 (fun _ -> f15) (fun _ -> f16)))))))
  in
  let Coq_pair (s, p) = stored_triple_seq_graph_correct in
  let Coq_pair (b, p0) = p in
  let Coq_pair (p1, c) = p0 in
  let Coq_pair (p2, p3) = h in
  let Coq_pair (p4, p5) = p3 in
  let Coq_pair (p6, p7) = p5 in
  Coq_pair ((fun _ lvl st ->
  p2 __ lvl st (stored_triple_seq lvl st) (s __ lvl st)), (Coq_pair
  ((fun _ hlvl tlvl hk tk b0 l ->
  p4 __ hlvl tlvl hk tk b0 l (body_seq hlvl tlvl hk tk b0 l)
    (b __ hlvl tlvl hk tk b0 l)), (Coq_pair
  ((fun _ hlvl tlvl nc nk c0 p8 l ->
  p6 __ hlvl tlvl nc nk c0 p8 l (packet_seq hlvl tlvl nc nk c0 p8 l)
    (p1 __ hlvl tlvl nc nk c0 p8 l)), (fun _ lvl ck nk cl cr c0 ->
  p7 __ lvl ck nk cl cr c0 (chain_seq lvl ck nk cl cr c0)
    (c __ lvl ck nk cl cr c0)))))))

(** val coq_FunctionalElimination_stored_triple_seq :
    (__ -> __ -> __) -> (__ -> nat -> nat -> __ stored_triple suffix' -> __)
    -> (__ -> nat -> nat -> nat -> nat -> color -> color -> __ stored_triple
    prefix' -> __ chain -> __ stored_triple suffix' -> __ -> __) -> (__ ->
    nat -> nkind -> __ list -> __) -> (__ -> nat -> nat -> nkind -> nkind ->
    yellow_hue -> orange_hue -> __ stored_triple node' -> __ body -> __ list
    -> __ -> __) -> (__ -> nat -> nat -> nkind -> nkind -> color -> __
    stored_triple node' -> __ body -> __ chain -> __ list -> __ -> __ -> __)
    -> (__ -> nat -> nat -> nkind -> nkind -> __ stored_triple node' -> __
    chain -> __ body -> __ list -> __ -> __ -> __) -> (__ -> nat -> nat ->
    nat -> nkind -> green_hue -> red_hue -> nkind -> __ body -> __
    stored_triple node' -> __ list -> __ -> __) -> (__ -> nat -> __) -> (__
    -> nat -> nkind -> color -> nat -> nat -> color -> color -> regularity ->
    __ packet -> __ chain -> __ -> __ -> __) -> (__ -> nat -> color -> color
    -> __ chain -> __ chain -> __ -> __ -> __) -> (__ -> nat -> __
    stored_triple -> __, (__ -> nat -> nat -> nkind -> nkind -> __ body -> __
    list -> __, (__ -> nat -> nat -> nat -> nkind -> color -> __ packet -> __
    list -> __, __ -> nat -> nat -> nkind -> color -> color -> __ chain ->
    __) prod) prod) prod **)

let coq_FunctionalElimination_stored_triple_seq =
  stored_triple_seq_elim

(** val coq_FunctionalInduction_stored_triple_seq :
    (__ -> nat -> __ stored_triple -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_stored_triple_seq =
  Obj.magic stored_triple_seq_graph_correct

(** val green_buffer_seq : nat -> 'a1 green_buffer -> 'a1 list **)

let green_buffer_seq lvl = function
| Gbuf (q, t0) ->
  concat_map_seq (Obj.magic (fun _ -> stored_triple_seq)) lvl
    (add (S (S (S (S (S (S (S (S O)))))))) q) t0

type 'a green_buffer_seq_graph =
| Coq_green_buffer_seq_graph_equation_1 of nat * nat * 'a stored_triple t

(** val green_buffer_seq_graph_rect :
    (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2
    green_buffer -> 'a2 list -> 'a2 green_buffer_seq_graph -> 'a1 **)

let green_buffer_seq_graph_rect f _ _ _ = function
| Coq_green_buffer_seq_graph_equation_1 (lvl, q, b) -> Obj.magic f __ lvl q b

(** val green_buffer_seq_graph_correct :
    nat -> 'a1 green_buffer -> 'a1 green_buffer_seq_graph **)

let green_buffer_seq_graph_correct lvl = function
| Gbuf (q, t0) -> Coq_green_buffer_seq_graph_equation_1 (lvl, q, t0)

(** val green_buffer_seq_elim :
    (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2
    green_buffer -> 'a1 **)

let green_buffer_seq_elim f lvl g =
  let Coq_green_buffer_seq_graph_equation_1 (lvl0, q, b) =
    green_buffer_seq_graph_correct lvl g
  in
  Obj.magic f __ lvl0 q b

(** val coq_FunctionalElimination_green_buffer_seq :
    (__ -> nat -> nat -> __ stored_triple t -> __) -> nat -> __ green_buffer
    -> __ **)

let coq_FunctionalElimination_green_buffer_seq =
  green_buffer_seq_elim

(** val coq_FunctionalInduction_green_buffer_seq :
    (__ -> nat -> __ green_buffer -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_green_buffer_seq =
  Obj.magic (fun _ -> green_buffer_seq_graph_correct)

(** val stored_buffer_seq : nat -> 'a1 stored_buffer -> 'a1 list **)

let stored_buffer_seq lvl = function
| Sbuf (q, t0) ->
  concat_map_seq (Obj.magic (fun _ -> stored_triple_seq)) lvl
    (add (S (S (S O))) q) t0

type 'a stored_buffer_seq_graph =
| Coq_stored_buffer_seq_graph_equation_1 of nat * nat * 'a stored_triple t

(** val stored_buffer_seq_graph_rect :
    (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2
    stored_buffer -> 'a2 list -> 'a2 stored_buffer_seq_graph -> 'a1 **)

let stored_buffer_seq_graph_rect f _ _ _ = function
| Coq_stored_buffer_seq_graph_equation_1 (lvl, q, b) -> Obj.magic f __ lvl q b

(** val stored_buffer_seq_graph_correct :
    nat -> 'a1 stored_buffer -> 'a1 stored_buffer_seq_graph **)

let stored_buffer_seq_graph_correct lvl = function
| Sbuf (q, t0) -> Coq_stored_buffer_seq_graph_equation_1 (lvl, q, t0)

(** val stored_buffer_seq_elim :
    (__ -> nat -> nat -> __ stored_triple t -> 'a1) -> nat -> 'a2
    stored_buffer -> 'a1 **)

let stored_buffer_seq_elim f lvl s =
  let Coq_stored_buffer_seq_graph_equation_1 (lvl0, q, b) =
    stored_buffer_seq_graph_correct lvl s
  in
  Obj.magic f __ lvl0 q b

(** val coq_FunctionalElimination_stored_buffer_seq :
    (__ -> nat -> nat -> __ stored_triple t -> __) -> nat -> __ stored_buffer
    -> __ **)

let coq_FunctionalElimination_stored_buffer_seq =
  stored_buffer_seq_elim

(** val coq_FunctionalInduction_stored_buffer_seq :
    (__ -> nat -> __ stored_buffer -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_stored_buffer_seq =
  Obj.magic (fun _ -> stored_buffer_seq_graph_correct)

(** val triple_seq : nat -> nkind -> color -> 'a1 triple -> 'a1 list **)

let triple_seq _ _ _ = function
| Triple (lvl, ck, nk, c, cl, cr, _, _, n, c0) ->
  concat_map_node'_seq (Obj.magic (fun _ -> stored_triple_seq)) lvl ck nk c n
    (chain_seq (S lvl) ck Coq_only cl cr c0)

type 'a triple_seq_graph =
| Coq_triple_seq_graph_equation_1 of nat * nkind * color * nat * color
   * color * color * regularity * 'a node * 'a chain

(** val triple_seq_graph_rect :
    (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
    regularity -> __ node -> __ chain -> 'a1) -> nat -> nkind -> color -> 'a2
    triple -> 'a2 list -> 'a2 triple_seq_graph -> 'a1 **)

let triple_seq_graph_rect f _ _ _ _ _ = function
| Coq_triple_seq_graph_equation_1 (lvl, k, c, ck, c0, cl, cr, r, hd, child) ->
  Obj.magic f __ lvl k c ck c0 cl cr r hd child

(** val triple_seq_graph_correct :
    nat -> nkind -> color -> 'a1 triple -> 'a1 triple_seq_graph **)

let triple_seq_graph_correct _ _ _ = function
| Triple (lvl, ck, nk, c, cl, cr, cpkt, r, n, c0) ->
  Coq_triple_seq_graph_equation_1 (lvl, nk, cpkt, ck, c, cl, cr, r, n, c0)

(** val triple_seq_elim :
    (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
    regularity -> __ node -> __ chain -> 'a1) -> nat -> nkind -> color -> 'a2
    triple -> 'a1 **)

let triple_seq_elim f lvl k c t0 =
  let Coq_triple_seq_graph_equation_1 (lvl0, k0, c0, ck, c1, cl, cr, r, hd,
                                       child) =
    triple_seq_graph_correct lvl k c t0
  in
  Obj.magic f __ lvl0 k0 c0 ck c1 cl cr r hd child

(** val coq_FunctionalElimination_triple_seq :
    (__ -> nat -> nkind -> color -> nat -> color -> color -> color ->
    regularity -> __ node -> __ chain -> __) -> nat -> nkind -> color -> __
    triple -> __ **)

let coq_FunctionalElimination_triple_seq =
  triple_seq_elim

(** val coq_FunctionalInduction_triple_seq :
    (__ -> nat -> nkind -> color -> __ triple -> __ list)
    coq_FunctionalInduction **)

let coq_FunctionalInduction_triple_seq =
  Obj.magic (fun _ -> triple_seq_graph_correct)

(** val lr_triple_seq :
    nat -> nkind -> color -> 'a1 left_right_triple -> 'a1 list **)

let lr_triple_seq _ _ _ = function
| Not_enough (lvl, _, v) ->
  concat
    (map (stored_triple_seq lvl) (vector_seq (S (S (S (S (S (S O)))))) v))
| Ok_lrt (lvl, k, cpkt, t0) -> triple_seq lvl k cpkt t0

type 'a lr_triple_seq_graph =
| Coq_lr_triple_seq_graph_equation_1 of nat * nkind * 'a stored_triple vector
| Coq_lr_triple_seq_graph_equation_2 of nat * nkind * color * 'a triple

(** val lr_triple_seq_graph_rect :
    (__ -> nat -> nkind -> __ stored_triple vector -> 'a1) -> (__ -> nat ->
    nkind -> color -> __ triple -> 'a1) -> nat -> nkind -> color -> 'a2
    left_right_triple -> 'a2 list -> 'a2 lr_triple_seq_graph -> 'a1 **)

let lr_triple_seq_graph_rect f f0 _ _ _ _ _ = function
| Coq_lr_triple_seq_graph_equation_1 (lvl, k, v) -> Obj.magic f __ lvl k v
| Coq_lr_triple_seq_graph_equation_2 (lvl, k, c, t0) ->
  Obj.magic f0 __ lvl k c t0

(** val lr_triple_seq_graph_correct :
    nat -> nkind -> color -> 'a1 left_right_triple -> 'a1 lr_triple_seq_graph **)

let lr_triple_seq_graph_correct _ _ _ = function
| Not_enough (lvl, k, v) -> Coq_lr_triple_seq_graph_equation_1 (lvl, k, v)
| Ok_lrt (lvl, k, cpkt, t0) ->
  Coq_lr_triple_seq_graph_equation_2 (lvl, k, cpkt, t0)

(** val lr_triple_seq_elim :
    (__ -> nat -> nkind -> __ stored_triple vector -> 'a1) -> (__ -> nat ->
    nkind -> color -> __ triple -> 'a1) -> nat -> nkind -> color -> 'a2
    left_right_triple -> 'a1 **)

let lr_triple_seq_elim f f0 lvl k c l =
  match lr_triple_seq_graph_correct lvl k c l with
  | Coq_lr_triple_seq_graph_equation_1 (lvl0, k0, v) ->
    Obj.magic f __ lvl0 k0 v
  | Coq_lr_triple_seq_graph_equation_2 (lvl0, k0, c0, t0) ->
    Obj.magic f0 __ lvl0 k0 c0 t0

(** val coq_FunctionalElimination_lr_triple_seq :
    (__ -> nat -> nkind -> __ stored_triple vector -> __) -> (__ -> nat ->
    nkind -> color -> __ triple -> __) -> nat -> nkind -> color -> __
    left_right_triple -> __ **)

let coq_FunctionalElimination_lr_triple_seq =
  lr_triple_seq_elim

(** val coq_FunctionalInduction_lr_triple_seq :
    (__ -> nat -> nkind -> color -> __ left_right_triple -> __ list)
    coq_FunctionalInduction **)

let coq_FunctionalInduction_lr_triple_seq =
  Obj.magic (fun _ -> lr_triple_seq_graph_correct)

(** val six_stored_triple_seq : nat -> 'a1 six_stored_triple -> 'a1 list **)

let six_stored_triple_seq lvl = function
| Coq_pair (p, s0) ->
  let Coq_pair (p0, s1) = p in
  let Coq_pair (p1, s2) = p0 in
  let Coq_pair (p2, s3) = p1 in
  let Coq_pair (s4, s5) = p2 in
  app (stored_triple_seq lvl s4)
    (app (stored_triple_seq lvl s5)
      (app (stored_triple_seq lvl s3)
        (app (stored_triple_seq lvl s2)
          (app (stored_triple_seq lvl s1) (stored_triple_seq lvl s0)))))

type 'a six_stored_triple_seq_graph =
| Coq_six_stored_triple_seq_graph_equation_1 of nat * 'a stored_triple
   * 'a stored_triple * 'a stored_triple * 'a stored_triple
   * 'a stored_triple * 'a stored_triple

(** val six_stored_triple_seq_graph_rect :
    (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
    __ stored_triple -> __ stored_triple -> __ stored_triple -> 'a1) -> nat
    -> 'a2 six_stored_triple -> 'a2 list -> 'a2 six_stored_triple_seq_graph
    -> 'a1 **)

let six_stored_triple_seq_graph_rect f _ _ _ = function
| Coq_six_stored_triple_seq_graph_equation_1 (lvl, a1, a2, a3, a4, a5, a6) ->
  Obj.magic f __ lvl a1 a2 a3 a4 a5 a6

(** val six_stored_triple_seq_graph_correct :
    nat -> 'a1 six_stored_triple -> 'a1 six_stored_triple_seq_graph **)

let six_stored_triple_seq_graph_correct lvl = function
| Coq_pair (p, s0) ->
  let Coq_pair (p0, s1) = p in
  let Coq_pair (p1, s2) = p0 in
  let Coq_pair (p2, s3) = p1 in
  let Coq_pair (s4, s5) = p2 in
  Coq_six_stored_triple_seq_graph_equation_1 (lvl, s4, s5, s3, s2, s1, s0)

(** val six_stored_triple_seq_elim :
    (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
    __ stored_triple -> __ stored_triple -> __ stored_triple -> 'a1) -> nat
    -> 'a2 six_stored_triple -> 'a1 **)

let six_stored_triple_seq_elim f lvl s =
  let Coq_six_stored_triple_seq_graph_equation_1 (lvl0, a1, a2, a3, a4, a5, a6) =
    six_stored_triple_seq_graph_correct lvl s
  in
  Obj.magic f __ lvl0 a1 a2 a3 a4 a5 a6

(** val coq_FunctionalElimination_six_stored_triple_seq :
    (__ -> nat -> __ stored_triple -> __ stored_triple -> __ stored_triple ->
    __ stored_triple -> __ stored_triple -> __ stored_triple -> __) -> nat ->
    __ six_stored_triple -> __ **)

let coq_FunctionalElimination_six_stored_triple_seq =
  six_stored_triple_seq_elim

(** val coq_FunctionalInduction_six_stored_triple_seq :
    (__ -> nat -> __ six_stored_triple -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_six_stored_triple_seq =
  Obj.magic (fun _ -> six_stored_triple_seq_graph_correct)

(** val pt_triple_seq :
    nat -> nat -> nkind -> 'a1 partial_triple -> 'a1 list **)

let pt_triple_seq _ _ _ = function
| Zero_element (_, _) -> Coq_nil
| Six_elements (lvl, _, s) -> six_stored_triple_seq lvl s
| Ok_pt (lvl, _, nk, c, t0) -> triple_seq lvl nk c t0

type 'a pt_triple_seq_graph =
| Coq_pt_triple_seq_graph_equation_1 of nat * nkind
| Coq_pt_triple_seq_graph_equation_2 of nat * nkind * 'a six_stored_triple
| Coq_pt_triple_seq_graph_equation_3 of nat * nat * nkind * color * 'a triple

(** val pt_triple_seq_graph_rect :
    (__ -> nat -> nkind -> 'a1) -> (__ -> nat -> nkind -> __
    six_stored_triple -> 'a1) -> (__ -> nat -> nat -> nkind -> color -> __
    triple -> 'a1) -> nat -> nat -> nkind -> 'a2 partial_triple -> 'a2 list
    -> 'a2 pt_triple_seq_graph -> 'a1 **)

let pt_triple_seq_graph_rect f f0 f1 _ _ _ _ _ = function
| Coq_pt_triple_seq_graph_equation_1 (lvl, k) -> f __ lvl k
| Coq_pt_triple_seq_graph_equation_2 (lvl, k, six) ->
  Obj.magic f0 __ lvl k six
| Coq_pt_triple_seq_graph_equation_3 (lvl, pk, k, c, t0) ->
  Obj.magic f1 __ lvl pk k c t0

(** val pt_triple_seq_graph_correct :
    nat -> nat -> nkind -> 'a1 partial_triple -> 'a1 pt_triple_seq_graph **)

let pt_triple_seq_graph_correct _ _ _ = function
| Zero_element (lvl, nk) -> Coq_pt_triple_seq_graph_equation_1 (lvl, nk)
| Six_elements (lvl, nk, s) -> Coq_pt_triple_seq_graph_equation_2 (lvl, nk, s)
| Ok_pt (lvl, ck, nk, c, t0) ->
  Coq_pt_triple_seq_graph_equation_3 (lvl, ck, nk, c, t0)

(** val pt_triple_seq_elim :
    (__ -> nat -> nkind -> 'a1) -> (__ -> nat -> nkind -> __
    six_stored_triple -> 'a1) -> (__ -> nat -> nat -> nkind -> color -> __
    triple -> 'a1) -> nat -> nat -> nkind -> 'a2 partial_triple -> 'a1 **)

let pt_triple_seq_elim f f0 f1 lvl pk k p =
  match pt_triple_seq_graph_correct lvl pk k p with
  | Coq_pt_triple_seq_graph_equation_1 (lvl0, k0) -> f __ lvl0 k0
  | Coq_pt_triple_seq_graph_equation_2 (lvl0, k0, six) ->
    Obj.magic f0 __ lvl0 k0 six
  | Coq_pt_triple_seq_graph_equation_3 (lvl0, pk0, k0, c, t0) ->
    Obj.magic f1 __ lvl0 pk0 k0 c t0

(** val coq_FunctionalElimination_pt_triple_seq :
    (__ -> nat -> nkind -> __) -> (__ -> nat -> nkind -> __ six_stored_triple
    -> __) -> (__ -> nat -> nat -> nkind -> color -> __ triple -> __) -> nat
    -> nat -> nkind -> __ partial_triple -> __ **)

let coq_FunctionalElimination_pt_triple_seq =
  pt_triple_seq_elim

(** val coq_FunctionalInduction_pt_triple_seq :
    (__ -> nat -> nat -> nkind -> __ partial_triple -> __ list)
    coq_FunctionalInduction **)

let coq_FunctionalInduction_pt_triple_seq =
  Obj.magic (fun _ -> pt_triple_seq_graph_correct)

(** val sandwich_seq :
    ('a1 -> 'a3 list) -> ('a2 -> 'a3 list) -> ('a1, 'a2) sandwich -> 'a3 list **)

let sandwich_seq l l0 = function
| Alone a -> l a
| Sandwich (a, b, a0) -> app (l a) (app (l0 b) (l a0))

type ('a, 'b, 'c) sandwich_seq_graph =
| Coq_sandwich_seq_graph_equation_1 of ('a -> 'c list) * ('b -> 'c list) * 'a
| Coq_sandwich_seq_graph_equation_2 of ('a -> 'c list) * ('b -> 'c list) *
   'a * 'b * 'a

(** val sandwich_seq_graph_rect :
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> 'a1) ->
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __
    -> 'a1) -> ('a2 -> 'a4 list) -> ('a3 -> 'a4 list) -> ('a2, 'a3) sandwich
    -> 'a4 list -> ('a2, 'a3, 'a4) sandwich_seq_graph -> 'a1 **)

let sandwich_seq_graph_rect f f0 _ _ _ _ = function
| Coq_sandwich_seq_graph_equation_1 (a_seq, l0, a) ->
  Obj.magic f __ __ __ a_seq l0 a
| Coq_sandwich_seq_graph_equation_2 (a_seq, b_seq, a, m, z) ->
  Obj.magic f0 __ __ __ a_seq b_seq a m z

(** val sandwich_seq_graph_correct :
    ('a1 -> 'a3 list) -> ('a2 -> 'a3 list) -> ('a1, 'a2) sandwich -> ('a1,
    'a2, 'a3) sandwich_seq_graph **)

let sandwich_seq_graph_correct l l0 = function
| Alone a -> Coq_sandwich_seq_graph_equation_1 (l, l0, a)
| Sandwich (a, b, a0) -> Coq_sandwich_seq_graph_equation_2 (l, l0, a, b, a0)

(** val sandwich_seq_elim :
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> 'a1) ->
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __
    -> 'a1) -> ('a2 -> 'a4 list) -> ('a3 -> 'a4 list) -> ('a2, 'a3) sandwich
    -> 'a1 **)

let sandwich_seq_elim f f0 l l0 s =
  match sandwich_seq_graph_correct l l0 s with
  | Coq_sandwich_seq_graph_equation_1 (a_seq, l1, a) ->
    Obj.magic f __ __ __ a_seq l1 a
  | Coq_sandwich_seq_graph_equation_2 (a_seq, b_seq, a, m, z) ->
    Obj.magic f0 __ __ __ a_seq b_seq a m z

(** val coq_FunctionalElimination_sandwich_seq :
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __) -> (__
    -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> __ -> __ -> __ ->
    __) -> (__ -> __ list) -> (__ -> __ list) -> (__, __) sandwich -> __ **)

let coq_FunctionalElimination_sandwich_seq =
  sandwich_seq_elim

(** val coq_FunctionalInduction_sandwich_seq :
    (__ -> __ -> __ -> (__ -> __ list) -> (__ -> __ list) -> (__, __)
    sandwich -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_sandwich_seq =
  Obj.magic (fun _ _ _ -> sandwich_seq_graph_correct)

(** val semi_cadeque_seq : nat -> 'a1 semi_cadeque -> 'a1 list **)

let semi_cadeque_seq _ = function
| Semi (lvl, ck, cl, cr, c) -> chain_seq lvl ck Coq_only cl cr c

type 'a semi_cadeque_seq_graph =
| Coq_semi_cadeque_seq_graph_equation_1 of nat * nat * color * color
   * 'a chain

(** val semi_cadeque_seq_graph_rect :
    (__ -> nat -> nat -> color -> color -> __ chain -> 'a1) -> nat -> 'a2
    semi_cadeque -> 'a2 list -> 'a2 semi_cadeque_seq_graph -> 'a1 **)

let semi_cadeque_seq_graph_rect f _ _ _ = function
| Coq_semi_cadeque_seq_graph_equation_1 (lvl, ck, cl, cr, c) ->
  Obj.magic f __ lvl ck cl cr c

(** val semi_cadeque_seq_graph_correct :
    nat -> 'a1 semi_cadeque -> 'a1 semi_cadeque_seq_graph **)

let semi_cadeque_seq_graph_correct _ = function
| Semi (lvl, ck, cl, cr, c) ->
  Coq_semi_cadeque_seq_graph_equation_1 (lvl, ck, cl, cr, c)

(** val semi_cadeque_seq_elim :
    (__ -> nat -> nat -> color -> color -> __ chain -> 'a1) -> nat -> 'a2
    semi_cadeque -> 'a1 **)

let semi_cadeque_seq_elim f lvl s =
  let Coq_semi_cadeque_seq_graph_equation_1 (lvl0, ck, cl, cr, c) =
    semi_cadeque_seq_graph_correct lvl s
  in
  Obj.magic f __ lvl0 ck cl cr c

(** val coq_FunctionalElimination_semi_cadeque_seq :
    (__ -> nat -> nat -> color -> color -> __ chain -> __) -> nat -> __
    semi_cadeque -> __ **)

let coq_FunctionalElimination_semi_cadeque_seq =
  semi_cadeque_seq_elim

(** val coq_FunctionalInduction_semi_cadeque_seq :
    (__ -> nat -> __ semi_cadeque -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_semi_cadeque_seq =
  Obj.magic (fun _ -> semi_cadeque_seq_graph_correct)

(** val cadeque_seq : 'a1 cadeque -> 'a1 list **)

let cadeque_seq = function
| T (ck, c0) ->
  chain_seq O ck Coq_only (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Mix
    (SomeGreen, NoYellow, NoOrange, NoRed)) c0

type 'a cadeque_seq_graph =
| Coq_cadeque_seq_graph_equation_1 of nat * 'a chain

(** val cadeque_seq_graph_rect :
    (__ -> nat -> __ chain -> 'a1) -> 'a2 cadeque -> 'a2 list -> 'a2
    cadeque_seq_graph -> 'a1 **)

let cadeque_seq_graph_rect f _ _ = function
| Coq_cadeque_seq_graph_equation_1 (ck, c) -> Obj.magic f __ ck c

(** val cadeque_seq_graph_correct : 'a1 cadeque -> 'a1 cadeque_seq_graph **)

let cadeque_seq_graph_correct = function
| T (ck, c0) -> Coq_cadeque_seq_graph_equation_1 (ck, c0)

(** val cadeque_seq_elim :
    (__ -> nat -> __ chain -> 'a1) -> 'a2 cadeque -> 'a1 **)

let cadeque_seq_elim f c =
  let Coq_cadeque_seq_graph_equation_1 (ck, c0) = cadeque_seq_graph_correct c
  in
  Obj.magic f __ ck c0

(** val coq_FunctionalElimination_cadeque_seq :
    (__ -> nat -> __ chain -> __) -> __ cadeque -> __ **)

let coq_FunctionalElimination_cadeque_seq =
  cadeque_seq_elim

(** val coq_FunctionalInduction_cadeque_seq :
    (__ -> __ cadeque -> __ list) coq_FunctionalInduction **)

let coq_FunctionalInduction_cadeque_seq =
  Obj.magic (fun _ -> cadeque_seq_graph_correct)
