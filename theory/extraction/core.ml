open Datatypes
open EqDec
open GYOR
open Nat
open Buffer
open Models
open Types

let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val push_left_node_obligations_obligation_1 : nat -> nat **)

let push_left_node_obligations_obligation_1 qs =
  qs

(** val push_left_node_obligations_obligation_3 : nat -> nat **)

let push_left_node_obligations_obligation_3 qs1 =
  qs1

(** val push_left_node_obligations_obligation_5 : nat -> nat **)

let push_left_node_obligations_obligation_5 qs2 =
  qs2

(** val push_left_node_obligations_obligation_7 : nat -> nat **)

let push_left_node_obligations_obligation_7 qs3 =
  qs3

(** val push_left_node_obligations_obligation_9 : nat -> nat **)

let push_left_node_obligations_obligation_9 qs4 =
  qs4

(** val push_left_node :
    nat -> nat -> color -> 'a1 stored_triple -> 'a1 node -> 'a1 node **)

let push_left_node _ _ _ a1 = function
| Left (_, _, _, _, c, p, s) ->
  (match c with
   | Gc (qp, qs, nc) ->
     Left
       ((add (S (S (S O))) (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O qp))),
       (add (S (S (S O))) (push_left_node_obligations_obligation_1 qs)), (S
       nc), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Gc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O qp)), (push_left_node_obligations_obligation_1 qs), nc)),
       (push (add (S (S (S (S (S O))))) (add (S (S (S O))) qp)) a1 p), s)
   | Yc (qp, qs, nc) ->
     Left
       ((add (S (S O)) (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O qp))),
       (add (S (S O)) (push_left_node_obligations_obligation_3 qs)), (S nc),
       (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), (Yc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O qp)), (push_left_node_obligations_obligation_3 qs), nc)),
       (push (add (S (S (S (S (S O))))) (add (S (S O)) qp)) a1 p), s)
   | Oc (qp, qs, nc) ->
     Left ((add (S O) (add (S O) qp)),
       (add (S O) (push_left_node_obligations_obligation_5 qs)), (S nc), (Mix
       (NoGreen, NoYellow, SomeOrange, NoRed)), (Oc ((add (S O) qp),
       (push_left_node_obligations_obligation_5 qs), nc)),
       (push (add (S (S (S (S (S O))))) (add (S O) qp)) a1 p), s)
   | Rc (qp, qs, nc) ->
     Left
       ((add O (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O (add O qp)))),
       (add O (push_left_node_obligations_obligation_7 qs)), (S nc), (Mix
       (NoGreen, NoYellow, NoOrange, SomeRed)), (Rc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O (add O qp))), (push_left_node_obligations_obligation_7 qs),
       nc)), (push (add (S (S (S (S (S O))))) (add O qp)) a1 p), s)
   | Ec (qp, qs) ->
     Left
       ((add O (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O (add O qp)))),
       (add O (push_left_node_obligations_obligation_9 qs)), O, (Mix
       (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O (add O qp))),
       (push_left_node_obligations_obligation_9 qs))),
       (push (add (S (S (S (S (S O))))) (add O qp)) a1 p), s))
| _ -> assert false (* absurd case *)

(** val inject_right_node_obligations_obligation_1 : nat -> nat **)

let inject_right_node_obligations_obligation_1 qs =
  qs

(** val inject_right_node_obligations_obligation_3 : nat -> nat **)

let inject_right_node_obligations_obligation_3 qs0 =
  qs0

(** val inject_right_node_obligations_obligation_5 : nat -> nat **)

let inject_right_node_obligations_obligation_5 qs2 =
  qs2

(** val inject_right_node_obligations_obligation_7 : nat -> nat **)

let inject_right_node_obligations_obligation_7 qs3 =
  qs3

(** val inject_right_node_obligations_obligation_9 : nat -> nat **)

let inject_right_node_obligations_obligation_9 qs4 =
  qs4

(** val inject_right_node :
    nat -> nat -> color -> 'a1 node -> 'a1 stored_triple -> 'a1 node **)

let inject_right_node _ _ _ st a1 =
  match st with
  | Right (_, _, _, _, c, p, s) ->
    (match c with
     | Gc (_, qs, nc) ->
       Right
         ((add (S (S (S O))) (inject_right_node_obligations_obligation_1 qs)),
         (add (S (S (S O))) (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs))), (S nc), (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed)), (Gc ((inject_right_node_obligations_obligation_1 qs), (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O qs)), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add (S (S (S O))) qs)) s a1))
     | Yc (_, qs, nc) ->
       Right
         ((add (S (S O)) (inject_right_node_obligations_obligation_3 qs)),
         (add (S (S O)) (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs))), (S nc), (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)), (Yc ((inject_right_node_obligations_obligation_3 qs), (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O qs)), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add (S (S O)) qs)) s a1))
     | Oc (_, qs, nc) ->
       Right ((add (S O) (inject_right_node_obligations_obligation_5 qs)),
         (add (S O) (add (S O) qs)), (S nc), (Mix (NoGreen, NoYellow,
         SomeOrange, NoRed)), (Oc
         ((inject_right_node_obligations_obligation_5 qs), (add (S O) qs),
         nc)), p, (inject (add (S (S (S (S (S O))))) (add (S O) qs)) s a1))
     | Rc (_, qs, nc) ->
       Right ((add O (inject_right_node_obligations_obligation_7 qs)),
         (add O (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O (add O qs)))), (S nc), (Mix (NoGreen, NoYellow,
         NoOrange, SomeRed)), (Rc
         ((inject_right_node_obligations_obligation_7 qs), (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O (add O qs))), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add O qs)) s a1))
     | Ec (_, qs) ->
       Right ((add O (inject_right_node_obligations_obligation_9 qs)),
         (add O (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O (add O qs)))), O, (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed)), (Ec ((inject_right_node_obligations_obligation_9 qs), (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O (add O qs))))), p,
         (inject (add (S (S (S (S (S O))))) (add O qs)) s a1)))
  | _ -> assert false (* absurd case *)

(** val push_only_node :
    nat -> nat -> color -> 'a1 stored_triple -> 'a1 node -> 'a1 node **)

let push_only_node _ _ _ a1 = function
| Only_end (q, p) -> Only_end ((S q), (push (S q) a1 p))
| Only (_, _, nc, _, c, p, s) ->
  (match c with
   | Gc (qp, qs, _) ->
     Only
       ((add (S (S (S O))) (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O qp))), (add (S (S (S O))) qs), nc, (Mix (SomeGreen,
       NoYellow, NoOrange, NoRed)), (Gc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O qp)), qs, nc)),
       (push (add (S (S (S (S (S O))))) (add (S (S (S O))) qp)) a1 p), s)
   | Yc (qp, qs, _) ->
     Only
       ((add (S (S O)) (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O qp))), (add (S (S O)) qs), nc, (Mix (NoGreen,
       SomeYellow, NoOrange, NoRed)), (Yc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O qp)), qs, nc)),
       (push (add (S (S (S (S (S O))))) (add (S (S O)) qp)) a1 p), s)
   | Oc (qp, qs, _) ->
     Only ((add (S O) (add (S O) qp)), (add (S O) qs), nc, (Mix (NoGreen,
       NoYellow, SomeOrange, NoRed)), (Oc ((add (S O) qp), qs, nc)),
       (push (add (S (S (S (S (S O))))) (add (S O) qp)) a1 p), s)
   | Rc (qp, qs, _) ->
     Only
       ((add O (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 O (add O qp)))), (add O qs), nc, (Mix (NoGreen, NoYellow,
       NoOrange, SomeRed)), (Rc ((S
       (let rec add0 n m =
          match n with
          | O -> m
          | S p0 -> S (add0 p0 m)
        in add0 O (add O qp))), qs, nc)),
       (push (add (S (S (S (S (S O))))) (add O qp)) a1 p), s)
   | Ec (_, _) -> assert false (* absurd case *))
| _ -> assert false (* absurd case *)

(** val inject_only_node :
    nat -> nat -> color -> 'a1 node -> 'a1 stored_triple -> 'a1 node **)

let inject_only_node _ _ _ st a1 =
  match st with
  | Only_end (q, p) -> Only_end ((S q), (inject (S q) p a1))
  | Only (_, _, nc, _, c, p, s) ->
    (match c with
     | Gc (qp, qs, _) ->
       Only ((add (S (S (S O))) qp),
         (add (S (S (S O))) (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs))), nc, (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed)), (Gc (qp, (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O qs)), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add (S (S (S O))) qs)) s a1))
     | Yc (qp, qs, _) ->
       Only ((add (S (S O)) qp),
         (add (S (S O)) (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs))), nc, (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)), (Yc (qp, (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O qs)), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add (S (S O)) qs)) s a1))
     | Oc (qp, qs, _) ->
       Only ((add (S O) qp), (add (S O) (add (S O) qs)), nc, (Mix (NoGreen,
         NoYellow, SomeOrange, NoRed)), (Oc (qp, (add (S O) qs), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add (S O) qs)) s a1))
     | Rc (qp, qs, _) ->
       Only ((add O qp),
         (add O (S
           (let rec add0 n m =
              match n with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O (add O qs)))), nc, (Mix (NoGreen, NoYellow, NoOrange,
         SomeRed)), (Rc (qp, (S
         (let rec add0 n m =
            match n with
            | O -> m
            | S p0 -> S (add0 p0 m)
          in add0 O (add O qs))), nc)), p,
         (inject (add (S (S (S (S (S O))))) (add O qs)) s a1))
     | Ec (_, _) -> assert false (* absurd case *))
  | _ -> assert false (* absurd case *)

(** val push_left_packet :
    nat -> nat -> nat -> color -> 'a1 stored_triple -> 'a1 packet -> 'a1
    packet **)

let push_left_packet _ _ _ _ a1 = function
| Packet (_, _, nc, _, _, g, r, b, n) ->
  (match b with
   | Hole (lvl, _) ->
     Packet (lvl, lvl, nc, Coq_left, Coq_left, g, r, (Hole (lvl, Coq_left)),
       (push_left_node lvl nc (Mix (g, NoYellow, NoOrange, r)) a1 n))
   | Single_child (hlvl, tlvl, _, tk, y, o, n0, b0) ->
     Packet (hlvl, tlvl, nc, Coq_left, tk, g, r, (Single_child (hlvl, tlvl,
       Coq_left, tk, y, o,
       (push_left_node hlvl (S O) (Mix (NoGreen, y, o, NoRed)) a1 n0), b0)),
       n)
   | Pair_yellow (hlvl, tlvl, _, tk, c, n0, b0, c0) ->
     Packet (hlvl, tlvl, nc, Coq_left, tk, g, r, (Pair_yellow (hlvl, tlvl,
       Coq_left, tk, c,
       (push_left_node hlvl (S (S O)) (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)) a1 n0), b0, c0)), n)
   | Pair_orange (hlvl, tlvl, _, tk, n0, c, b0) ->
     Packet (hlvl, tlvl, nc, Coq_left, tk, g, r, (Pair_orange (hlvl, tlvl,
       Coq_left, tk,
       (push_left_node hlvl (S (S O)) (Mix (NoGreen, NoYellow, SomeOrange,
         NoRed)) a1 n0), c, b0)), n))

(** val inject_right_packet :
    nat -> nat -> nat -> color -> 'a1 packet -> 'a1 stored_triple -> 'a1
    packet **)

let inject_right_packet _ _ _ _ pkt a1 =
  let Packet (_, _, nc, _, _, g, r, b, n) = pkt in
  (match b with
   | Hole (lvl, _) ->
     Packet (lvl, lvl, nc, Coq_right, Coq_right, g, r, (Hole (lvl,
       Coq_right)),
       (inject_right_node lvl nc (Mix (g, NoYellow, NoOrange, r)) n a1))
   | Single_child (hlvl, tlvl, _, tk, y, o, n0, b0) ->
     Packet (hlvl, tlvl, nc, Coq_right, tk, g, r, (Single_child (hlvl, tlvl,
       Coq_right, tk, y, o,
       (inject_right_node hlvl (S O) (Mix (NoGreen, y, o, NoRed)) n0 a1),
       b0)), n)
   | Pair_yellow (hlvl, tlvl, _, tk, c, n0, b0, c0) ->
     Packet (hlvl, tlvl, nc, Coq_right, tk, g, r, (Pair_yellow (hlvl, tlvl,
       Coq_right, tk, c,
       (inject_right_node hlvl (S (S O)) (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)) n0 a1), b0, c0)), n)
   | Pair_orange (hlvl, tlvl, _, tk, n0, c, b0) ->
     Packet (hlvl, tlvl, nc, Coq_right, tk, g, r, (Pair_orange (hlvl, tlvl,
       Coq_right, tk,
       (inject_right_node hlvl (S (S O)) (Mix (NoGreen, NoYellow, SomeOrange,
         NoRed)) n0 a1), c, b0)), n))

(** val push_only_packet :
    nat -> nat -> nat -> color -> 'a1 stored_triple -> 'a1 packet -> 'a1
    packet **)

let push_only_packet _ _ _ _ a1 = function
| Packet (_, _, nc, _, _, g, r, b, n) ->
  (match b with
   | Hole (lvl, _) ->
     Packet (lvl, lvl, nc, Coq_only, Coq_only, g, r, (Hole (lvl, Coq_only)),
       (push_only_node lvl nc (Mix (g, NoYellow, NoOrange, r)) a1 n))
   | Single_child (hlvl, tlvl, _, tk, y, o, n0, b0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Single_child (hlvl, tlvl,
       Coq_only, tk, y, o,
       (push_only_node hlvl (S O) (Mix (NoGreen, y, o, NoRed)) a1 n0), b0)),
       n)
   | Pair_yellow (hlvl, tlvl, _, tk, c, n0, b0, c0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Pair_yellow (hlvl, tlvl,
       Coq_only, tk, c,
       (push_only_node hlvl (S (S O)) (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)) a1 n0), b0, c0)), n)
   | Pair_orange (hlvl, tlvl, _, tk, n0, c, b0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Pair_orange (hlvl, tlvl,
       Coq_only, tk,
       (push_only_node hlvl (S (S O)) (Mix (NoGreen, NoYellow, SomeOrange,
         NoRed)) a1 n0), c, b0)), n))

(** val inject_only_packet :
    nat -> nat -> nat -> color -> 'a1 packet -> 'a1 stored_triple -> 'a1
    packet **)

let inject_only_packet _ _ _ _ pkt a1 =
  let Packet (_, _, nc, _, _, g, r, b, n) = pkt in
  (match b with
   | Hole (lvl, _) ->
     Packet (lvl, lvl, nc, Coq_only, Coq_only, g, r, (Hole (lvl, Coq_only)),
       (inject_only_node lvl nc (Mix (g, NoYellow, NoOrange, r)) n a1))
   | Single_child (hlvl, tlvl, _, tk, y, o, n0, b0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Single_child (hlvl, tlvl,
       Coq_only, tk, y, o,
       (inject_only_node hlvl (S O) (Mix (NoGreen, y, o, NoRed)) n0 a1),
       b0)), n)
   | Pair_yellow (hlvl, tlvl, _, tk, c, n0, b0, c0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Pair_yellow (hlvl, tlvl,
       Coq_only, tk, c,
       (inject_only_node hlvl (S (S O)) (Mix (NoGreen, SomeYellow, NoOrange,
         NoRed)) n0 a1), b0, c0)), n)
   | Pair_orange (hlvl, tlvl, _, tk, n0, c, b0) ->
     Packet (hlvl, tlvl, nc, Coq_only, tk, g, r, (Pair_orange (hlvl, tlvl,
       Coq_only, tk,
       (inject_only_node hlvl (S (S O)) (Mix (NoGreen, NoYellow, SomeOrange,
         NoRed)) n0 a1), c, b0)), n))

(** val single_node : nat -> 'a1 stored_triple -> 'a1 node **)

let single_node _ a1 =
  Only_end (O, (single a1))

(** val single_packet : nat -> 'a1 stored_triple -> 'a1 packet **)

let single_packet lvl a1 =
  Packet (lvl, lvl, O, Coq_only, Coq_only, SomeGreen, NoRed, (Hole (lvl,
    Coq_only)), (single_node lvl a1))

(** val single_chain : nat -> 'a1 stored_triple -> 'a1 chain **)

let single_chain lvl a1 =
  Single (lvl, (S lvl), O, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange,
    NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
    NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen, NoYellow, NoOrange,
    NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)))),
    (single_packet lvl a1), (Empty (S lvl)))

(** val push_left_chain :
    nat -> color -> 'a1 stored_triple -> 'a1 chain -> 'a1 chain **)

let push_left_chain _ _ a1 = function
| Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
  coq_UIP_K c0 (fun _ -> Single (hlvl, tlvl, ck, Coq_left, c0, cl, cr, r,
    (push_left_packet hlvl tlvl ck c0 a1 p), c1)) __
| _ -> assert false (* absurd case *)

(** val inject_right_chain :
    nat -> color -> 'a1 chain -> 'a1 stored_triple -> 'a1 chain **)

let inject_right_chain _ _ c a1 =
  match c with
  | Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
    coq_UIP_K c0 (fun _ -> Single (hlvl, tlvl, ck, Coq_right, c0, cl, cr, r,
      (inject_right_packet hlvl tlvl ck c0 p a1), c1)) __
  | _ -> assert false (* absurd case *)

(** val push_ne_chain :
    nat -> nat -> color -> color -> 'a1 stored_triple -> 'a1 chain -> 'a1
    chain **)

let push_ne_chain _ _ _ _ a1 = function
| Empty _ -> assert false (* absurd case *)
| Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
  Single (hlvl, tlvl, ck, Coq_only, c0, cl, cr, r,
    (push_only_packet hlvl tlvl ck c0 a1 p), c1)
| Pair (lvl, cl, cr, c0, c1) ->
  Pair (lvl, cl, cr, (push_left_chain lvl cl a1 c0), c1)

(** val inject_ne_chain :
    nat -> nat -> color -> color -> 'a1 chain -> 'a1 stored_triple -> 'a1
    chain **)

let inject_ne_chain _ _ _ _ c a1 =
  match c with
  | Empty _ -> assert false (* absurd case *)
  | Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
    Single (hlvl, tlvl, ck, Coq_only, c0, cl, cr, r,
      (inject_only_packet hlvl tlvl ck c0 p a1), c1)
  | Pair (lvl, cl, cr, c0, c1) ->
    Pair (lvl, cl, cr, c0, (inject_right_chain lvl cr c1 a1))

(** val semi_push :
    nat -> 'a1 stored_triple -> 'a1 semi_cadeque -> 'a1 semi_cadeque **)

let semi_push _ a1 = function
| Semi (_, _, _, _, c) ->
  (match c with
   | Empty lvl ->
     Semi (lvl, (S O), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
       (SomeGreen, NoYellow, NoOrange, NoRed)), (single_chain lvl a1))
   | Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
     Semi (hlvl, (S O), c0, c0,
       (push_ne_chain hlvl O c0 c0 a1 (Single (hlvl, tlvl, ck, Coq_only, c0,
         cl, cr, r, p, c1))))
   | Pair (lvl, cl, cr, c0, c1) ->
     Semi (lvl, (S (S O)), cl, cr,
       (push_ne_chain lvl (S O) cl cr a1 (Pair (lvl, cl, cr, c0, c1)))))

(** val semi_inject :
    nat -> 'a1 semi_cadeque -> 'a1 stored_triple -> 'a1 semi_cadeque **)

let semi_inject _ sd a1 =
  let Semi (_, _, _, _, c) = sd in
  (match c with
   | Empty lvl ->
     Semi (lvl, (S O), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
       (SomeGreen, NoYellow, NoOrange, NoRed)), (single_chain lvl a1))
   | Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
     Semi (hlvl, (S O), c0, c0,
       (inject_ne_chain hlvl O c0 c0 (Single (hlvl, tlvl, ck, Coq_only, c0,
         cl, cr, r, p, c1)) a1))
   | Pair (lvl, cl, cr, c0, c1) ->
     Semi (lvl, (S (S O)), cl, cr,
       (inject_ne_chain lvl (S O) cl cr (Pair (lvl, cl, cr, c0, c1)) a1)))

(** val to_reg :
    nat -> nkind -> yellow_hue -> orange_hue -> color -> 'a1 node ->
    regularity **)

let to_reg _ _ _ _ c = function
| Only_end (_, _) -> assert false (* absurd case *)
| Only (_, _, _, _, c0, _, _) ->
  (match c0 with
   | Yc (_, _, _) -> Y (O, c, c)
   | Oc (_, _, _) -> OS c
   | _ -> assert false (* absurd case *))
| Left (_, _, _, _, c0, _, _) ->
  (match c0 with
   | Yc (_, _, _) -> Y (O, c, c)
   | Oc (_, _, _) -> OS c
   | _ -> assert false (* absurd case *))
| Right (_, _, _, _, c0, _, _) ->
  (match c0 with
   | Yc (_, _, _) -> Y (O, c, c)
   | Oc (_, _, _) -> OS c
   | _ -> assert false (* absurd case *))

(** val triple_of_chain : nat -> nkind -> color -> 'a1 chain -> 'a1 triple **)

let triple_of_chain _ _ _ = function
| Single (_, _, _, _, c0, _, _, r, p, c1) ->
  coq_UIP_K c0 (fun _ ->
    match r with
    | G (_, cl, cr) ->
      let Packet (_, _, nc, _, _, _, _, b, n) = p in
      (match b with
       | Hole (lvl, k) ->
         Triple (lvl, nc, k, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
           cl, cr, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (G (nc, cl,
           cr)), n, c1)
       | Single_child (hlvl, tlvl, hk, tk, y, o, n0, b0) ->
         Triple (hlvl, (S O), hk, (Mix (NoGreen, y, o, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)),
           (to_reg hlvl hk y o (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
             n0), n0, (Single ((S hlvl), (S tlvl), nc, Coq_only, (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, (G (nc, cl, cr)),
           (Packet ((S hlvl), tlvl, nc, Coq_only, tk, SomeGreen, NoRed, b0,
           n)), c1)))
       | Pair_yellow (hlvl, tlvl, hk, tk, c2, n0, b0, c3) ->
         Triple (hlvl, (S (S O)), hk, (Mix (NoGreen, SomeYellow, NoOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c2, (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (Y ((S O), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), c2)), n0, (Pair ((S
           hlvl), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), c2, (Single
           ((S hlvl), (S tlvl), nc, Coq_left, (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), cl, cr, (G (nc, cl, cr)), (Packet ((S hlvl),
           tlvl, nc, Coq_left, tk, SomeGreen, NoRed, b0, n)), c1)), c3)))
       | Pair_orange (hlvl, tlvl, hk, tk, n0, c2, b0) ->
         Triple (hlvl, (S (S O)), hk, (Mix (NoGreen, NoYellow, SomeOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (OP (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed))), n0, (Pair ((S hlvl), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), c2, (Single ((S hlvl), (S tlvl), nc, Coq_right, (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cl, cr, (G (nc, cl, cr)),
           (Packet ((S hlvl), tlvl, nc, Coq_right, tk, SomeGreen, NoRed, b0,
           n)), c1))))))
    | R ck ->
      let Packet (_, _, _, _, _, _, _, b, n) = p in
      (match b with
       | Hole (lvl, k) ->
         Triple (lvl, (S ck), k, (Mix (NoGreen, NoYellow, NoOrange,
           SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (R ck), n, c1)
       | Single_child (hlvl, tlvl, hk, tk, y, o, n0, b0) ->
         Triple (hlvl, (S O), hk, (Mix (NoGreen, y, o, NoRed)), (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
           (to_reg hlvl hk y o (Mix (NoGreen, NoYellow, NoOrange, SomeRed))
             n0), n0, (Single ((S hlvl), (S tlvl), (S ck), Coq_only, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (R ck), (Packet ((S hlvl), tlvl, (S ck), Coq_only, tk,
           NoGreen, SomeRed, b0, n)), c1)))
       | Pair_yellow (hlvl, tlvl, hk, tk, c2, n0, b0, c3) ->
         Triple (hlvl, (S (S O)), hk, (Mix (NoGreen, SomeYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), c2, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Y ((S O), (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), c2)), n0, (Pair ((S hlvl), (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), c2, (Single ((S hlvl), (S
           tlvl), (S ck), Coq_left, (Mix (NoGreen, NoYellow, NoOrange,
           SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), (R ck), (Packet ((S
           hlvl), tlvl, (S ck), Coq_left, tk, NoGreen, SomeRed, b0, n)),
           c1)), c3)))
       | Pair_orange (hlvl, tlvl, hk, tk, n0, c2, b0) ->
         Triple (hlvl, (S (S O)), hk, (Mix (NoGreen, NoYellow, SomeOrange,
           NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (OP (Mix (NoGreen, NoYellow, NoOrange,
           SomeRed))), n0, (Pair ((S hlvl), (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
           c2, (Single ((S hlvl), (S tlvl), (S ck), Coq_right, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
           (R ck), (Packet ((S hlvl), tlvl, (S ck), Coq_right, tk, NoGreen,
           SomeRed, b0, n)), c1))))))
    | _ -> assert false (* absurd case *)) __
| _ -> assert false (* absurd case *)

(** val chain_of_triple : nat -> nkind -> color -> 'a1 triple -> 'a1 chain **)

let chain_of_triple _ _ _ = function
| Triple (lvl, _, nk, _, _, _, _, r, n, c) ->
  (match r with
   | G (ck, cl, cr) ->
     Single (lvl, (S lvl), ck, nk, (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), cl, cr, (G (ck, cl, cr)), (Packet (lvl, lvl, ck, nk, nk,
       SomeGreen, NoRed, (Hole (lvl, nk)), n)), c)
   | Y (_, _, _) ->
     (match c with
      | Empty _ -> assert false (* absurd case *)
      | Single (_, _, _, _, _, cl, cr, r0, p, c0) ->
        let Packet (_, tlvl, nc, _, tk, g, r1, b, n0) = p in
        Single (lvl, (S tlvl), nc, nk, (Mix (g, NoYellow, NoOrange, r1)), cl,
        cr, r0, (Packet (lvl, tlvl, nc, nk, tk, g, r1, (Single_child (lvl,
        tlvl, nk, tk, SomeYellow, NoOrange, n, b)), n0)), c0)
      | Pair (_, _, cr, c0, c1) ->
        (match c0 with
         | Single (_, _, _, _, c2, cl, cr0, r0, p, c3) ->
           coq_UIP_K c2 (fun _ ->
             let Packet (_, tlvl, nc, _, tk, g, r1, b, n0) = p in
             Single (lvl, (S tlvl), nc, nk, (Mix (g, NoYellow, NoOrange,
             r1)), cl, cr0, r0, (Packet (lvl, tlvl, nc, nk, tk, g, r1,
             (Pair_yellow (lvl, tlvl, nk, tk, cr, n, b, c1)), n0)), c3)) __
         | _ -> assert false (* absurd case *)))
   | OS _ ->
     (match c with
      | Single (_, _, _, _, c0, cl, cr, r0, p, c1) ->
        coq_UIP_K c0 (fun _ ->
          let Packet (_, tlvl, nc, _, tk, g, r1, b, n0) = p in
          Single (lvl, (S tlvl), nc, nk, (Mix (g, NoYellow, NoOrange, r1)),
          cl, cr, r0, (Packet (lvl, tlvl, nc, nk, tk, g, r1, (Single_child
          (lvl, tlvl, nk, tk, NoYellow, SomeOrange, n, b)), n0)), c1)) __
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match c with
      | Pair (_, _, _, c0, c1) ->
        (match c1 with
         | Single (_, _, _, _, c2, cl, cr, r0, p, c3) ->
           coq_UIP_K c2 (fun _ ->
             let Packet (_, tlvl, nc, _, tk, g, r1, b, n0) = p in
             Single (lvl, (S tlvl), nc, nk, (Mix (g, NoYellow, NoOrange,
             r1)), cl, cr, r0, (Packet (lvl, tlvl, nc, nk, tk, g, r1,
             (Pair_orange (lvl, tlvl, nk, tk, n, c0, b)), n0)), c3)) __
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R ck ->
     Single (lvl, (S lvl), (S ck), nk, (Mix (NoGreen, NoYellow, NoOrange,
       SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
       (SomeGreen, NoYellow, NoOrange, NoRed)), (R ck), (Packet (lvl, lvl, (S
       ck), nk, nk, NoGreen, SomeRed, (Hole (lvl, nk)), n)), c))

(** val left_of_only_obligations_obligation_2 : nat -> nat **)

let left_of_only_obligations_obligation_2 q =
  q

(** val left_of_only : nat -> color -> 'a1 triple -> 'a1 left_right_triple **)

let left_of_only _ _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Only_end (q, p) ->
        (match c with
         | Empty _ ->
           (match has7 q p with
            | Coq_inl v -> Not_enough (lvl, Coq_left, v)
            | Coq_inr p0 ->
              let Coq_pair (p1, s) =
                eject2 (S
                  (let rec add0 n0 m =
                     match n0 with
                     | O -> m
                     | S p1 -> S (add0 p1 m)
                   in add0 (S (S (S (S O))))
                        (iter (S (S (S (S (S (S (S O))))))) pred (S q)))) p0
              in
              let Coq_pair (t0, s0) = p1 in
              Ok_lrt (lvl, Coq_left, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Triple (lvl, O, Coq_left, (Mix (SomeGreen, NoYellow,
              NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (G (O, (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)))), (Left
              ((add O (iter (S (S (S (S (S (S (S O))))))) pred (S q))),
              (add O (left_of_only_obligations_obligation_2 q)), O, (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec
              ((iter (S (S (S (S (S (S (S O))))))) pred (S q)),
              (left_of_only_obligations_obligation_2 q))), t0, (pair s0 s))),
              (Empty (S lvl))))))
         | _ -> assert false (* absurd case *))
      | Only (qp, qs, nc, _, c0, p, s) ->
        let Coq_pair (p0, s0) =
          eject2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qs)) s
        in
        let Coq_pair (t0, s1) = p0 in
        Ok_lrt (lvl, Coq_left, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        (Triple (lvl, (S nc), Coq_left, (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)), cl, cr, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (G ((S
        nc), cl, cr)), (Left (qp, qs, (S nc), (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), c0, p, (pair s1 s0))),
        (inject_ne_chain (S lvl) nc cl cr c (Small (lvl, qs, t0))))))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) =
          eject2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qs)) s
        in
        let Coq_pair (t0, s1) = p0 in
        Ok_lrt (lvl, Coq_left, cl, (Triple (lvl, (S ck), Coq_left, (Mix
        (NoGreen, SomeYellow, NoOrange, NoRed)), cl, cr, cl, (Y (ck, cl,
        cr)), (Left (qp, qs, (S ck), (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)), c0, p, (pair s1 s0))),
        (inject_ne_chain (S lvl) ck cl cr c (Small (lvl, qs, t0))))))
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Only (qp, qs, _, _, c1, p, s) ->
        let Coq_pair (p0, s0) =
          eject2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qs)) s
        in
        let Coq_pair (t0, s1) = p0 in
        Ok_lrt (lvl, Coq_left, c0, (Triple (lvl, (S O), Coq_left, (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), c0, c0, c0, (OS c0), (Left
        (qp, qs, (S O), (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), c1, p,
        (pair s1 s0))),
        (inject_ne_chain (S lvl) O c0 c0 c (Small (lvl, qs, t0))))))
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) =
          eject2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qs)) s
        in
        let Coq_pair (t0, s1) = p0 in
        Ok_lrt (lvl, Coq_left, cr, (Triple (lvl, (S (S O)), Coq_left, (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), cr, cr, (OP cr), (Left (qp, qs, (S (S O)), (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), c0, p, (pair s1 s0))),
        (inject_ne_chain (S lvl) (S O) (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) cr c (Small (lvl, qs, t0))))))
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) =
          eject2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qs)) s
        in
        let Coq_pair (t0, s1) = p0 in
        Ok_lrt (lvl, Coq_left, (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
        (Triple (lvl, (S ck), Coq_left, (Mix (NoGreen, NoYellow, NoOrange,
        SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
        NoOrange, SomeRed)), (R ck), (Left (qp, qs, (S ck), (Mix (NoGreen,
        NoYellow, NoOrange, SomeRed)), c0, p, (pair s1 s0))),
        (inject_ne_chain (S lvl) ck (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c (Small (lvl,
          qs, t0))))))
      | _ -> assert false (* absurd case *)))

(** val right_of_only_obligations_obligation_2 : nat -> nat **)

let right_of_only_obligations_obligation_2 q =
  q

(** val right_of_only :
    nat -> color -> 'a1 triple -> 'a1 left_right_triple **)

let right_of_only _ _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Only_end (q, p) ->
        (match c with
         | Empty _ ->
           (match has7 q p with
            | Coq_inl v -> Not_enough (lvl, Coq_right, v)
            | Coq_inr p0 ->
              let Coq_pair (p1, t0) =
                pop2 (S
                  (let rec add0 n0 m =
                     match n0 with
                     | O -> m
                     | S p1 -> S (add0 p1 m)
                   in add0 (S (S (S (S O))))
                        (iter (S (S (S (S (S (S (S O))))))) pred (S q)))) p0
              in
              let Coq_pair (s, s0) = p1 in
              Ok_lrt (lvl, Coq_right, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Triple (lvl, O, Coq_right, (Mix (SomeGreen, NoYellow,
              NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (G (O, (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)))), (Right
              ((add O (right_of_only_obligations_obligation_2 q)),
              (add O (iter (S (S (S (S (S (S (S O))))))) pred (S q))), O,
              (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec
              ((right_of_only_obligations_obligation_2 q),
              (iter (S (S (S (S (S (S (S O))))))) pred (S q)))), (pair s s0),
              t0)), (Empty (S lvl))))))
         | _ -> assert false (* absurd case *))
      | Only (qp, qs, nc, _, c0, p, s) ->
        let Coq_pair (p0, t0) =
          pop2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qp)) p
        in
        let Coq_pair (s0, s1) = p0 in
        Ok_lrt (lvl, Coq_right, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        (Triple (lvl, (S nc), Coq_right, (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)), cl, cr, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (G ((S
        nc), cl, cr)), (Right (qp, qs, (S nc), (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), c0, (pair s0 s1), s)),
        (push_ne_chain (S lvl) nc cl cr (Small (lvl, qp, t0)) c))))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, t0) =
          pop2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qp)) p
        in
        let Coq_pair (s0, s1) = p0 in
        Ok_lrt (lvl, Coq_right, cl, (Triple (lvl, (S ck), Coq_right, (Mix
        (NoGreen, SomeYellow, NoOrange, NoRed)), cl, cr, cl, (Y (ck, cl,
        cr)), (Right (qp, qs, (S ck), (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)), c0, (pair s0 s1), s)),
        (push_ne_chain (S lvl) ck cl cr (Small (lvl, qp, t0)) c))))
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Only (qp, qs, _, _, c1, p, s) ->
        let Coq_pair (p0, t0) =
          pop2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qp)) p
        in
        let Coq_pair (s0, s1) = p0 in
        Ok_lrt (lvl, Coq_right, c0, (Triple (lvl, (S O), Coq_right, (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), c0, c0, c0, (OS c0), (Right
        (qp, qs, (S O), (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), c1,
        (pair s0 s1), s)),
        (push_ne_chain (S lvl) O c0 c0 (Small (lvl, qp, t0)) c))))
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, t0) =
          pop2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qp)) p
        in
        let Coq_pair (s0, s1) = p0 in
        Ok_lrt (lvl, Coq_right, cr, (Triple (lvl, (S (S O)), Coq_right, (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), cr, cr, (OP cr), (Right (qp, qs, (S (S O)), (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)), c0, (pair s0 s1), s)),
        (push_ne_chain (S lvl) (S O) (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) cr (Small (lvl, qp, t0)) c))))
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Only (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, t0) =
          pop2 (S
            (let rec add0 n0 m =
               match n0 with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) qp)) p
        in
        let Coq_pair (s0, s1) = p0 in
        Ok_lrt (lvl, Coq_right, (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
        (Triple (lvl, (S ck), Coq_right, (Mix (NoGreen, NoYellow, NoOrange,
        SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
        NoOrange, SomeRed)), (R ck), (Right (qp, qs, (S ck), (Mix (NoGreen,
        NoYellow, NoOrange, SomeRed)), c0, (pair s0 s1), s)),
        (push_ne_chain (S lvl) ck (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Small (lvl,
          qp, t0)) c))))
      | _ -> assert false (* absurd case *)))

(** val make_stored_suffix :
    nat -> nat -> nat -> nat -> color -> color -> 'a1 suffix -> 'a1 prefix ->
    'a1 chain -> 'a1 suffix -> ('a1 stored_triple, 'a1 suffix) prod **)

let make_stored_suffix lvl ql q ck cl cr sl p child s =
  let Coq_pair (s0, s1) = two p in
  let Coq_pair (p0, s2) =
    eject2 (S
      (let rec add0 n m =
         match n with
         | O -> m
         | S p0 -> S (add0 p0 m)
       in add0 (S (S O)) q)) s
  in
  let Coq_pair (t, s3) = p0 in
  Coq_pair ((Big (lvl, ql, q, ck, cl, cr, (inject2 (add (S O) ql) sl s0 s1),
  child, t)), (pair s3 s2))

(** val make_prefix_stored :
    nat -> nat -> nat -> nat -> color -> color -> 'a1 prefix -> 'a1 chain ->
    'a1 suffix -> 'a1 prefix -> ('a1 prefix, 'a1 stored_triple) prod **)

let make_prefix_stored lvl q qr ck cl cr p child s pr =
  let Coq_pair (p0, t) =
    pop2 (S
      (let rec add0 n m =
         match n with
         | O -> m
         | S p0 -> S (add0 p0 m)
       in add0 (S (S O)) q)) p
  in
  let Coq_pair (s0, s1) = p0 in
  let Coq_pair (s2, s3) = two s in
  Coq_pair ((pair s0 s1), (Big (lvl, q, qr, ck, cl, cr, t, child,
  (push2 (add (S O) qr) s2 s3 pr))))

(** val stored_of_right :
    nat -> nat -> color -> 'a1 suffix -> 'a1 triple -> ('a1 stored_triple,
    'a1 suffix) prod **)

let stored_of_right _ ql _ sl = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (_, qs, nc) ->
           make_stored_suffix lvl ql (add (S (S (S O))) qs) (S nc) cl cr sl p
             c s
         | Ec (_, qs) ->
           (match c with
            | Empty _ ->
              make_stored_suffix lvl ql (add O qs) O (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)) (Mix (SomeGreen, NoYellow,
                NoOrange, NoRed)) sl p (Empty (S lvl)) s
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Right (_, qs, _, _, _, p, s) ->
        make_stored_suffix lvl ql qs (S ck) cl cr sl p c s
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Right (_, qs, _, _, _, p, s) ->
        make_stored_suffix lvl ql qs (S O) c0 c0 sl p c s
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Right (_, qs, _, _, _, p, s) ->
        make_stored_suffix lvl ql qs (S (S O)) (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) cr sl p c s
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Right (_, qs, _, _, _, p, s) ->
        make_stored_suffix lvl ql qs (S ck) (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) sl p
          c s
      | _ -> assert false (* absurd case *)))

(** val stored_of_left :
    nat -> nat -> color -> 'a1 triple -> 'a1 prefix -> ('a1 prefix, 'a1
    stored_triple) prod **)

let stored_of_left _ qr _ tl pr =
  let Triple (lvl, _, _, _, _, _, _, r, n, c) = tl in
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, _, nc) ->
           make_prefix_stored lvl (add (S (S (S O))) qp) qr (S nc) cl cr p c
             s pr
         | Ec (qp, _) ->
           (match c with
            | Empty _ ->
              make_prefix_stored lvl (add O qp) qr O (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)) (Mix (SomeGreen, NoYellow,
                NoOrange, NoRed)) p (Empty (S lvl)) s pr
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Left (qp, _, _, _, _, p, s) ->
        make_prefix_stored lvl qp qr (S ck) cl cr p c s pr
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Left (qp, _, _, _, _, p, s) ->
        make_prefix_stored lvl qp qr (S O) c0 c0 p c s pr
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Left (qp, _, _, _, _, p, s) ->
        make_prefix_stored lvl qp qr (S (S O)) (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) cr p c s pr
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Left (qp, _, _, _, _, p, s) ->
        make_prefix_stored lvl qp qr (S ck) (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) p c
          s pr
      | _ -> assert false (* absurd case *)))

(** val left_of_pair_obligations_obligation_2 : nat -> nat **)

let left_of_pair_obligations_obligation_2 qs4 =
  qs4

(** val left_of_pair :
    nat -> color -> color -> 'a1 triple -> 'a1 triple -> 'a1 triple **)

let left_of_pair _ _ cr tl tr =
  let Triple (lvl, _, _, _, _, _, _, r, n, c) = tl in
  (match r with
   | G (_, cl, cr0) ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, nc) ->
           let Coq_pair (s0, s1) = stored_of_right lvl (S O) cr s tr in
           Triple (lvl, (S nc), Coq_left, (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), cl, cr0, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (G ((S nc), cl, cr0)), (Left ((add (S (S (S O))) qp),
           (add (S (S (S O))) qs), (S nc), (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), (Gc (qp, qs, nc)), p, s1)),
           (inject_ne_chain (S lvl) nc cl cr0 c s0))
         | Ec (qp, qs) ->
           (match c with
            | Empty _ ->
              let Coq_pair (s0, t) = pop (S O) s in
              let Coq_pair (s1, s2) = stored_of_right lvl O cr t tr in
              Triple (lvl, (S O), Coq_left, (Mix (NoGreen, NoYellow,
              SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (OS (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed))), (Left ((add (S O) (add O qp)),
              (add (S O) (left_of_pair_obligations_obligation_2 qs)), (S O),
              (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Oc ((add O qp),
              (left_of_pair_obligations_obligation_2 qs), O)),
              (inject (add (S (S (S (S (S O))))) (add O qp)) p s0), s2)),
              (single_chain (S lvl) s1))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr0) ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (s0, s1) = stored_of_right lvl (S O) cr s tr in
        Triple (lvl, (S ck), Coq_left, (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)), cl, cr0, cl, (Y (ck, cl, cr0)), (Left (qp, qs, (S ck), (Mix
        (NoGreen, SomeYellow, NoOrange, NoRed)), c0, p, s1)),
        (inject_ne_chain (S lvl) ck cl cr0 c s0))
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Left (qp, qs, _, _, c1, p, s) ->
        let Coq_pair (s0, s1) = stored_of_right lvl (S O) cr s tr in
        Triple (lvl, (S O), Coq_left, (Mix (NoGreen, NoYellow, SomeOrange,
        NoRed)), c0, c0, c0, (OS c0), (Left (qp, qs, (S O), (Mix (NoGreen,
        NoYellow, SomeOrange, NoRed)), c1, p, s1)),
        (inject_ne_chain (S lvl) O c0 c0 c s0))
      | _ -> assert false (* absurd case *))
   | OP cr0 ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (s0, s1) = stored_of_right lvl (S O) cr s tr in
        Triple (lvl, (S (S O)), Coq_left, (Mix (NoGreen, NoYellow,
        SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        cr0, cr0, (OP cr0), (Left (qp, qs, (S (S O)), (Mix (NoGreen,
        NoYellow, SomeOrange, NoRed)), c0, p, s1)),
        (inject_ne_chain (S lvl) (S O) (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) cr0 c s0))
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (s0, s1) = stored_of_right lvl (S O) cr s tr in
        Triple (lvl, (S ck), Coq_left, (Mix (NoGreen, NoYellow, NoOrange,
        SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
        NoOrange, SomeRed)), (R ck), (Left (qp, qs, (S ck), (Mix (NoGreen,
        NoYellow, NoOrange, SomeRed)), c0, p, s1)),
        (inject_ne_chain (S lvl) ck (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) c s0))
      | _ -> assert false (* absurd case *)))

(** val right_of_pair_obligations_obligation_2 : nat -> nat **)

let right_of_pair_obligations_obligation_2 qs4 =
  qs4

(** val right_of_pair :
    nat -> color -> color -> 'a1 triple -> 'a1 triple -> 'a1 triple **)

let right_of_pair _ cl _ tl = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl0, cr) ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, nc) ->
           let Coq_pair (p0, s0) = stored_of_left lvl (S O) cl tl p in
           Triple (lvl, (S nc), Coq_right, (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), cl0, cr, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (G ((S nc), cl0, cr)), (Right ((add (S (S (S O))) qp),
           (add (S (S (S O))) qs), (S nc), (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), (Gc (qp, qs, nc)), p0, s)),
           (push_ne_chain (S lvl) nc cl0 cr s0 c))
         | Ec (_, qs) ->
           (match c with
            | Empty _ ->
              let Coq_pair (t, s0) = eject (S O) p in
              let Coq_pair (p0, s1) = stored_of_left lvl O cl tl t in
              Triple (lvl, (S O), Coq_right, (Mix (NoGreen, NoYellow,
              SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (OS (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed))), (Right
              ((add (S O) (right_of_pair_obligations_obligation_2 qs)),
              (add (S O) (add O qs)), (S O), (Mix (NoGreen, NoYellow,
              SomeOrange, NoRed)), (Oc
              ((right_of_pair_obligations_obligation_2 qs), (add O qs), O)),
              p0, (push (add (S (S (S (S (S O))))) (add O qs)) s0 s))),
              (single_chain (S lvl) s1))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl0, cr) ->
     (match n with
      | Right (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = stored_of_left lvl (S O) cl tl p in
        Triple (lvl, (S ck), Coq_right, (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)), cl0, cr, cl0, (Y (ck, cl0, cr)), (Right (qp, qs, (S ck),
        (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), c0, p0, s)),
        (push_ne_chain (S lvl) ck cl0 cr s0 c))
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Right (qp, qs, _, _, c1, p, s) ->
        let Coq_pair (p0, s0) = stored_of_left lvl (S O) cl tl p in
        Triple (lvl, (S O), Coq_right, (Mix (NoGreen, NoYellow, SomeOrange,
        NoRed)), c0, c0, c0, (OS c0), (Right (qp, qs, (S O), (Mix (NoGreen,
        NoYellow, SomeOrange, NoRed)), c1, p0, s)),
        (push_ne_chain (S lvl) O c0 c0 s0 c))
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Right (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = stored_of_left lvl (S O) cl tl p in
        Triple (lvl, (S (S O)), Coq_right, (Mix (NoGreen, NoYellow,
        SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        cr, cr, (OP cr), (Right (qp, qs, (S (S O)), (Mix (NoGreen, NoYellow,
        SomeOrange, NoRed)), c0, p0, s)),
        (push_ne_chain (S lvl) (S O) (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) cr s0 c))
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Right (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = stored_of_left lvl (S O) cl tl p in
        Triple (lvl, (S ck), Coq_right, (Mix (NoGreen, NoYellow, NoOrange,
        SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
        NoOrange, SomeRed)), (R ck), (Right (qp, qs, (S ck), (Mix (NoGreen,
        NoYellow, NoOrange, SomeRed)), c0, p0, s)),
        (push_ne_chain (S lvl) ck (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) s0 c))
      | _ -> assert false (* absurd case *)))

(** val make_left :
    nat -> nat -> color -> color -> 'a1 chain -> 'a1 left_right_triple **)

let make_left _ _ _ _ = function
| Empty lvl -> Not_enough (lvl, Coq_left, (V0 (S (S (S (S (S (S O))))))))
| Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
  left_of_only hlvl c0
    (triple_of_chain hlvl Coq_only c0 (Single (hlvl, tlvl, ck, Coq_only, c0,
      cl, cr, r, p, c1)))
| Pair (lvl, cl, cr, c0, c1) ->
  Ok_lrt (lvl, Coq_left, cl,
    (left_of_pair lvl cl cr (triple_of_chain lvl Coq_left cl c0)
      (triple_of_chain lvl Coq_right cr c1)))

(** val make_right :
    nat -> nat -> color -> color -> 'a1 chain -> 'a1 left_right_triple **)

let make_right _ _ _ _ = function
| Empty lvl -> Not_enough (lvl, Coq_right, (V0 (S (S (S (S (S (S O))))))))
| Single (hlvl, tlvl, ck, _, c0, cl, cr, r, p, c1) ->
  right_of_only hlvl c0
    (triple_of_chain hlvl Coq_only c0 (Single (hlvl, tlvl, ck, Coq_only, c0,
      cl, cr, r, p, c1)))
| Pair (lvl, cl, cr, c0, c1) ->
  Ok_lrt (lvl, Coq_right, cr,
    (right_of_pair lvl cl cr (triple_of_chain lvl Coq_left cl c0)
      (triple_of_chain lvl Coq_right cr c1)))

(** val semi_concat :
    nat -> 'a1 semi_cadeque -> 'a1 semi_cadeque -> 'a1 semi_cadeque **)

let semi_concat _ s1 s2 =
  let Semi (_, ck, cl, cr, c) = s1 in
  let Semi (lvl, ck0, cl0, cr0, c0) = s2 in
  (match make_left lvl ck cl cr c with
   | Not_enough (lvl0, _, v) ->
     vector_push (semi_cadeque_seq lvl0) (stored_triple_seq lvl0)
       (semi_push lvl0) (S (S (S (S (S (S O)))))) v (Semi (lvl0, ck0, cl0,
       cr0, c0))
   | Ok_lrt (lvl0, _, cpkt, t) ->
     (match make_right lvl0 ck0 cl0 cr0 c0 with
      | Not_enough (lvl1, _, v) ->
        vector_inject (semi_cadeque_seq lvl1) (stored_triple_seq lvl1)
          (semi_inject lvl1) (Semi (lvl1, ck, cpkt, cr, c)) (S (S (S (S (S (S
          O)))))) v
      | Ok_lrt (lvl1, _, cpkt0, t0) ->
        Semi (lvl1, (S (S O)), cpkt, cpkt0, (Pair (lvl1, cpkt, cpkt0,
          (chain_of_triple lvl1 Coq_left cpkt t),
          (chain_of_triple lvl1 Coq_right cpkt0 t0))))))

(** val orange : nat -> nat -> color -> 'a1 chain -> regularity **)

let orange _ _ _ = function
| Empty _ -> assert false (* absurd case *)
| Single (_, _, _, _, _, _, _, _, _, _) ->
  OS (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
| Pair (_, _, cr, _, _) -> OP cr

(** val pop_left_green_obligations_obligation_1 : nat -> nat **)

let pop_left_green_obligations_obligation_1 qs =
  qs

(** val pop_left_green_obligations_obligation_4 : nat -> nat **)

let pop_left_green_obligations_obligation_4 qs4 =
  qs4

(** val pop_left_green_obligations_obligation_6 : nat -> nat **)

let pop_left_green_obligations_obligation_6 qs1 =
  qs1

(** val pop_left_green_obligations_obligation_8 : nat -> nat **)

let pop_left_green_obligations_obligation_8 qs2 =
  qs2

(** val pop_left_green_obligations_obligation_10 : nat -> nat **)

let pop_left_green_obligations_obligation_10 qs2 =
  qs2

(** val pop_left_green :
    nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) prod **)

let pop_left_green _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, nc) ->
           let Coq_pair (s0, t) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S (S O)), Coq_left, cl, (Triple (lvl,
           (S nc), Coq_left, (Mix (NoGreen, SomeYellow, NoOrange, NoRed)),
           cl, cr, cl, (Y (nc, cl, cr)), (Left ((add (S (S O)) qp),
           (add (S (S O)) (pop_left_green_obligations_obligation_1 qs)), (S
           nc), (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), (Yc (qp,
           (pop_left_green_obligations_obligation_1 qs), nc)), t, s)), c)))))
         | Ec (qp, qs) ->
           (match c with
            | Empty _ ->
              let Coq_pair (s0, t) =
                pop (S
                  (let rec add0 n0 m =
                     match n0 with
                     | O -> m
                     | S p0 -> S (add0 p0 m)
                   in add0 (S (S (S O))) (add O qp))) p
              in
              (match has5 (add O qp) t with
               | Coq_inl p0 ->
                 let Coq_pair (s1, s2) = two s in
                 Coq_pair (s0, (Six_elements (lvl, Coq_left, (Coq_pair
                 ((Coq_pair (p0, s1)), s2)))))
               | Coq_inr p0 ->
                 Coq_pair (s0, (Ok_pt (lvl, (S (S O)), Coq_left, (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Triple (lvl, O,
                   Coq_left, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
                   NoOrange, NoRed)))), (Left
                   ((add O
                      (iter (S (S (S (S (S O))))) pred
                        (add (S (S (S (S O)))) (add O qp)))),
                   (add O (pop_left_green_obligations_obligation_4 qs)), O,
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec
                   ((iter (S (S (S (S (S O))))) pred
                      (add (S (S (S (S O)))) (add O qp))),
                   (pop_left_green_obligations_obligation_4 qs))), p0, s)),
                   (Empty (S lvl))))))))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, _, cr) ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Yc (qp, qs, _) ->
           let Coq_pair (s0, t) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S (S O)), Coq_left, cr, (Triple (lvl,
           (S ck), Coq_left, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)),
           (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), cr, cr,
           (orange (S lvl) ck cr c), (Left ((add (S O) qp),
           (add (S O) (pop_left_green_obligations_obligation_6 qs)), (S ck),
           (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Oc (qp,
           (pop_left_green_obligations_obligation_6 qs), ck)), t, s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OS _ ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S (S O)), Coq_left, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S O), Coq_left, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R O),
           (Left ((add O qp),
           (add O (pop_left_green_obligations_obligation_8 qs)), (S O), (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Rc (qp,
           (pop_left_green_obligations_obligation_8 qs), O)), t, s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S (S O)), Coq_left, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S (S O)), Coq_left,
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R (S O)),
           (Left ((add O qp),
           (add O (pop_left_green_obligations_obligation_10 qs)), (S (S O)),
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Rc (qp,
           (pop_left_green_obligations_obligation_10 qs), (S O))), t, s)),
           c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R _ -> assert false (* absurd case *))

(** val eject_right_green_obligations_obligation_1 : nat -> nat **)

let eject_right_green_obligations_obligation_1 qs =
  qs

(** val eject_right_green_obligations_obligation_4 : nat -> nat **)

let eject_right_green_obligations_obligation_4 qs4 =
  qs4

(** val eject_right_green_obligations_obligation_6 : nat -> nat **)

let eject_right_green_obligations_obligation_6 qs0 =
  qs0

(** val eject_right_green_obligations_obligation_8 : nat -> nat **)

let eject_right_green_obligations_obligation_8 qs2 =
  qs2

(** val eject_right_green_obligations_obligation_10 : nat -> nat **)

let eject_right_green_obligations_obligation_10 qs2 =
  qs2

(** val eject_right_green :
    nat -> 'a1 triple -> ('a1 partial_triple, 'a1 stored_triple) prod **)

let eject_right_green _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (_, qs, nc) ->
           let Coq_pair (t, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S (S O)), Coq_right, cl, (Triple (lvl, (S
           nc), Coq_right, (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), cl,
           cr, cl, (Y (nc, cl, cr)), (Right
           ((add (S (S O)) (eject_right_green_obligations_obligation_1 qs)),
           (add (S (S O)) qs), (S nc), (Mix (NoGreen, SomeYellow, NoOrange,
           NoRed)), (Yc ((eject_right_green_obligations_obligation_1 qs), qs,
           nc)), p, t)), c)))), s0)
         | Ec (_, qs) ->
           (match c with
            | Empty _ ->
              let Coq_pair (t, s0) =
                eject (S
                  (let rec add0 n0 m =
                     match n0 with
                     | O -> m
                     | S p0 -> S (add0 p0 m)
                   in add0 (S (S (S O))) (add O qs))) s
              in
              (match has5 (add O qs) t with
               | Coq_inl p0 ->
                 let Coq_pair (p1, s1) = p0 in
                 let Coq_pair (p2, s2) = p1 in
                 let Coq_pair (s3, s4) = p2 in
                 Coq_pair ((Six_elements (lvl, Coq_right, (Coq_pair
                 ((Coq_pair ((Coq_pair ((Coq_pair ((two p), s3)), s4)), s2)),
                 s1)))), s0)
               | Coq_inr p0 ->
                 Coq_pair ((Ok_pt (lvl, (S (S O)), Coq_right, (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Triple (lvl, O,
                   Coq_right, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
                   NoOrange, NoRed)))), (Right
                   ((add O (eject_right_green_obligations_obligation_4 qs)),
                   (add O
                     (iter (S (S (S (S (S O))))) pred
                       (add (S (S (S (S O)))) (add O qs)))), O, (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec
                   ((eject_right_green_obligations_obligation_4 qs),
                   (iter (S (S (S (S (S O))))) pred
                     (add (S (S (S (S O)))) (add O qs))))), p, p0)), (Empty
                   (S lvl)))))), s0))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, _, cr) ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Yc (_, qs, _) ->
           let Coq_pair (t, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S (S O)), Coq_right, cr, (Triple (lvl, (S
           ck), Coq_right, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cr, cr,
           (orange (S lvl) ck cr c), (Right
           ((add (S O) (eject_right_green_obligations_obligation_6 qs)),
           (add (S O) qs), (S ck), (Mix (NoGreen, NoYellow, SomeOrange,
           NoRed)), (Oc ((eject_right_green_obligations_obligation_6 qs), qs,
           ck)), p, t)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OS _ ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (_, qs, _) ->
           let Coq_pair (t, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S (S O)), Coq_right, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S O), Coq_right,
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R O),
           (Right ((add O (eject_right_green_obligations_obligation_8 qs)),
           (add O qs), (S O), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
           (Rc ((eject_right_green_obligations_obligation_8 qs), qs, O)), p,
           t)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match n with
      | Right (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (_, qs, _) ->
           let Coq_pair (t, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S (S O)), Coq_right, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S (S O)), Coq_right,
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R (S O)),
           (Right ((add O (eject_right_green_obligations_obligation_10 qs)),
           (add O qs), (S (S O)), (Mix (NoGreen, NoYellow, NoOrange,
           SomeRed)), (Rc ((eject_right_green_obligations_obligation_10 qs),
           qs, (S O))), p, t)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R _ -> assert false (* absurd case *))

(** val pop_only_green :
    nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) prod **)

let pop_only_green _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Only_end (q, p) ->
        (match c with
         | Empty _ ->
           let Coq_pair (s, t0) = pop q p in
           (match has1 q t0 with
            | Some p0 ->
              Coq_pair (s, (Ok_pt (lvl, (S O), Coq_only, (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)), (Triple (lvl, O, Coq_only, (Mix
                (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
                NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
                NoRed)), (G (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                (Mix (SomeGreen, NoYellow, NoOrange, NoRed)))), (Only_end
                ((iter (S O) pred q), p0)), (Empty (S lvl)))))))
            | None -> Coq_pair (s, (Zero_element (lvl, Coq_only))))
         | _ -> assert false (* absurd case *))
      | Only (_, _, nc, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S O), Coq_only, cl, (Triple (lvl, (S
           nc), Coq_only, (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), cl,
           cr, cl, (Y (nc, cl, cr)), (Only ((add (S (S O)) qp),
           (add (S (S O)) (S
             (let rec add0 n0 m =
                match n0 with
                | O -> m
                | S p0 -> S (add0 p0 m)
              in add0 O qs))), nc, (Mix (NoGreen, SomeYellow, NoOrange,
           NoRed)), (Yc (qp, (S
           (let rec add0 n0 m =
              match n0 with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs)), nc)), t0, s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, _, cr) ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Yc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S O), Coq_only, cr, (Triple (lvl, (S
           ck), Coq_only, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cr, cr,
           (orange (S lvl) ck cr c), (Only ((add (S O) qp),
           (add (S O) (S
             (let rec add0 n0 m =
                match n0 with
                | O -> m
                | S p0 -> S (add0 p0 m)
              in add0 O qs))), ck, (Mix (NoGreen, NoYellow, SomeOrange,
           NoRed)), (Oc (qp, (S
           (let rec add0 n0 m =
              match n0 with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qs)), ck)), t0, s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OS _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S O), Coq_only, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R O),
           (Only ((add O qp), (add O (add (S O) qs)), O, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Rc (qp, (add (S O) qs), O)), t0,
           s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           Coq_pair (s0, (Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S (S O)), Coq_only,
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R (S O)),
           (Only ((add O qp), (add O (add (S O) qs)), (S O), (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Rc (qp, (add (S O) qs), (S O))),
           t0, s)), c)))))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R _ -> assert false (* absurd case *))

(** val eject_only_green :
    nat -> 'a1 triple -> ('a1 partial_triple, 'a1 stored_triple) prod **)

let eject_only_green _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Only_end (q, p) ->
        (match c with
         | Empty _ ->
           let Coq_pair (t0, s) = eject q p in
           (match has1 q t0 with
            | Some p0 ->
              Coq_pair ((Ok_pt (lvl, (S O), Coq_only, (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)), (Triple (lvl, O, Coq_only, (Mix
                (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
                NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
                NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
                NoRed)), (G (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                (Mix (SomeGreen, NoYellow, NoOrange, NoRed)))), (Only_end
                ((iter (S O) pred q), p0)), (Empty (S lvl)))))), s)
            | None -> Coq_pair ((Zero_element (lvl, Coq_only)), s))
         | _ -> assert false (* absurd case *))
      | Only (_, _, nc, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, _) ->
           let Coq_pair (t0, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S O), Coq_only, cl, (Triple (lvl, (S nc),
           Coq_only, (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), cl, cr,
           cl, (Y (nc, cl, cr)), (Only
           ((add (S (S O)) (S
              (let rec add0 n0 m =
                 match n0 with
                 | O -> m
                 | S p0 -> S (add0 p0 m)
               in add0 O qp))), (add (S (S O)) qs), nc, (Mix (NoGreen,
           SomeYellow, NoOrange, NoRed)), (Yc ((S
           (let rec add0 n0 m =
              match n0 with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qp)), qs, nc)), p, t0)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, _, cr) ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Yc (qp, qs, _) ->
           let Coq_pair (t0, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S O), Coq_only, cr, (Triple (lvl, (S ck),
           Coq_only, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cr, cr,
           (orange (S lvl) ck cr c), (Only
           ((add (S O) (S
              (let rec add0 n0 m =
                 match n0 with
                 | O -> m
                 | S p0 -> S (add0 p0 m)
               in add0 O qp))), (add (S O) qs), ck, (Mix (NoGreen, NoYellow,
           SomeOrange, NoRed)), (Oc ((S
           (let rec add0 n0 m =
              match n0 with
              | O -> m
              | S p0 -> S (add0 p0 m)
            in add0 O qp)), qs, ck)), p, t0)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OS _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (t0, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Triple (lvl, (S O), Coq_only, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R O), (Only
           ((add O (add (S O) qp)), (add O qs), O, (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Rc ((add (S O) qp), qs, O)), p, t0)), c)))),
           s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (t0, s0) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Coq_pair ((Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Triple (lvl, (S (S O)), Coq_only, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R (S O)),
           (Only ((add O (add (S O) qp)), (add O qs), (S O), (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Rc ((add (S O) qp), qs, (S O))),
           p, t0)), c)))), s0)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R _ -> assert false (* absurd case *))

(** val sandwich_only_green :
    nat -> 'a1 triple -> ('a1 stored_triple, 'a1 partial_triple) sandwich **)

let sandwich_only_green _ = function
| Triple (lvl, _, _, _, _, _, _, r, n, c) ->
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Only_end (q, p) ->
        (match c with
         | Empty _ ->
           let Coq_pair (s, t0) = pop q p in
           (match has1 q t0 with
            | Some p0 ->
              let Coq_pair (t1, s0) = eject (iter (S O) pred q) p0 in
              (match has1 (iter (S O) pred q) t1 with
               | Some p1 ->
                 Sandwich (s, (Ok_pt (lvl, (S O), Coq_only, (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (Triple (lvl, O, Coq_only,
                   (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
                   (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
                   NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow,
                   NoOrange, NoRed)), (G (O, (Mix (SomeGreen, NoYellow,
                   NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
                   NoRed)))), (Only_end
                   ((iter (S O) pred (iter (S O) pred q)), p1)), (Empty (S
                   lvl)))))), s0)
               | None -> Sandwich (s, (Zero_element (lvl, Coq_only)), s0))
            | None -> Alone s)
         | _ -> assert false (* absurd case *))
      | Only (_, _, nc, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qp))) p
           in
           let Coq_pair (t1, s1) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S (S O))) qs))) s
           in
           Sandwich (s0, (Ok_pt (lvl, (S O), Coq_only, cl, (Triple (lvl, (S
           nc), Coq_only, (Mix (NoGreen, SomeYellow, NoOrange, NoRed)), cl,
           cr, cl, (Y (nc, cl, cr)), (Only ((add (S (S O)) qp),
           (add (S (S O)) qs), nc, (Mix (NoGreen, SomeYellow, NoOrange,
           NoRed)), (Yc (qp, qs, nc)), t0, t1)), c)))), s1)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, _, cr) ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Yc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qp))) p
           in
           let Coq_pair (t1, s1) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S (S O)) qs))) s
           in
           Sandwich (s0, (Ok_pt (lvl, (S O), Coq_only, cr, (Triple (lvl, (S
           ck), Coq_only, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Mix
           (SomeGreen, NoYellow, NoOrange, NoRed)), cr, cr,
           (orange (S lvl) ck cr c), (Only ((add (S O) qp), (add (S O) qs),
           ck, (Mix (NoGreen, NoYellow, SomeOrange, NoRed)), (Oc (qp, qs,
           ck)), t0, t1)), c)))), s1)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OS _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           let Coq_pair (t1, s1) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Sandwich (s0, (Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S O), Coq_only, (Mix
           (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R O),
           (Only ((add O qp), (add O qs), O, (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Rc (qp, qs, O)), t0, t1)), c)))), s1)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | OP _ ->
     (match n with
      | Only (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Oc (qp, qs, _) ->
           let Coq_pair (s0, t0) =
             pop (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qp))) p
           in
           let Coq_pair (t1, s1) =
             eject (S
               (let rec add0 n0 m =
                  match n0 with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S (S O))) (add (S O) qs))) s
           in
           Sandwich (s0, (Ok_pt (lvl, (S O), Coq_only, (Mix (NoGreen,
           NoYellow, NoOrange, SomeRed)), (Triple (lvl, (S (S O)), Coq_only,
           (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Mix (SomeGreen,
           NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (R (S O)),
           (Only ((add O qp), (add O qs), (S O), (Mix (NoGreen, NoYellow,
           NoOrange, SomeRed)), (Rc (qp, qs, (S O))), t0, t1)), c)))), s1)
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | R _ -> assert false (* absurd case *))

(** val adapt_to_prefix :
    nat -> nat -> nat -> nat -> color -> coloring -> coloring **)

let adapt_to_prefix _ _ q _ _ = function
| Gc (_, qs, nc) -> Gc (q, qs, nc)
| Yc (_, qs, nc) ->
  Yc ((S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 O q)), qs, nc)
| Oc (_, qs, nc) ->
  Oc ((S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S O) q)), qs, nc)
| Rc (_, qs, nc) ->
  Rc ((S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S (S O)) q)), qs, nc)
| Ec (_, qs) ->
  Ec ((S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S (S O)) q)), qs)

(** val only_of_right :
    nat -> color -> 'a1 six_stored_triple -> 'a1 triple -> 'a1 triple **)

let only_of_right _ _ six tr =
  let Coq_pair (p, s) = six in
  let Coq_pair (p0, s0) = p in
  let Coq_pair (p1, s1) = p0 in
  let Coq_pair (p2, s2) = p1 in
  let Coq_pair (s3, s4) = p2 in
  let Triple (lvl, _, _, _, _, _, _, r, n, c) = tr in
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Right (_, _, _, _, c0, p3, s5) ->
        (match c0 with
         | Gc (qp, qs, nc) ->
           Triple (lvl, (S nc), Coq_only, (Mix (SomeGreen, NoYellow,
             NoOrange, NoRed)), cl, cr, (Mix (SomeGreen, NoYellow, NoOrange,
             NoRed)), (G ((S nc), cl, cr)), (Only ((add (S (S (S O))) O),
             (add (S (S (S O))) qs), nc, (Mix (SomeGreen, NoYellow, NoOrange,
             NoRed)),
             (adapt_to_prefix (add (S (S (S O))) qp) (add (S (S (S O))) qs) O
               (S nc) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Gc (qp,
               qs, nc))), (push6 (S (S O)) s3 s4 s2 s1 s0 s p3), s5)), c)
         | Ec (_, qs) ->
           (match c with
            | Empty _ ->
              let Coq_pair (s6, s7) = two p3 in
              Triple (lvl, O, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen, NoYellow,
              NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)))), (Only_end ((S
              (let rec add0 n0 m =
                 match n0 with
                 | O -> m
                 | S p4 -> S (add0 p4 m)
               in add0 (S (S (S (S O)))) (S (S
                    (add (S (S (S (S (S O))))) (add O qs)))))),
              (push6 (S (S (add (S (S (S (S (S O))))) (add O qs)))) s3 s4 s2
                s1 s0 s
                (push2 (add (S (S (S (S (S O))))) (add O qs)) s6 s7 s5)))),
              (Empty (S lvl)))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Right (qp, qs, _, _, c0, p3, s5) ->
        Triple (lvl, (S ck), Coq_only, (Mix (NoGreen, SomeYellow, NoOrange,
          NoRed)), cl, cr, cl, (Y (ck, cl, cr)), (Only
          ((add (S (S (S O))) O), qs, ck, (Mix (NoGreen, SomeYellow,
          NoOrange, NoRed)),
          (adapt_to_prefix qp qs O (S ck) (Mix (NoGreen, SomeYellow,
            NoOrange, NoRed)) c0), (push6 (S (S O)) s3 s4 s2 s1 s0 s p3),
          s5)), c)
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Right (qp, qs, _, _, c1, p3, s5) ->
        Triple (lvl, (S O), Coq_only, (Mix (NoGreen, NoYellow, SomeOrange,
          NoRed)), c0, c0, c0, (OS c0), (Only ((add (S (S (S O))) O), qs, O,
          (Mix (NoGreen, NoYellow, SomeOrange, NoRed)),
          (adapt_to_prefix qp qs O (S O) (Mix (NoGreen, NoYellow, SomeOrange,
            NoRed)) c1), (push6 (S (S O)) s3 s4 s2 s1 s0 s p3), s5)), c)
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Right (qp, qs, _, _, c0, p3, s5) ->
        Triple (lvl, (S (S O)), Coq_only, (Mix (NoGreen, NoYellow,
          SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
          cr, cr, (OP cr), (Only ((add (S (S (S O))) O), qs, (S O), (Mix
          (NoGreen, NoYellow, SomeOrange, NoRed)),
          (adapt_to_prefix qp qs O (S (S O)) (Mix (NoGreen, NoYellow,
            SomeOrange, NoRed)) c0), (push6 (S (S O)) s3 s4 s2 s1 s0 s p3),
          s5)), c)
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Right (qp, qs, _, _, c0, p3, s5) ->
        Triple (lvl, (S ck), Coq_only, (Mix (NoGreen, NoYellow, NoOrange,
          SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
          (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
          NoOrange, SomeRed)), (R ck), (Only ((add (S (S (S O))) O), qs, ck,
          (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
          (adapt_to_prefix qp qs O (S ck) (Mix (NoGreen, NoYellow, NoOrange,
            SomeRed)) c0), (push6 (S (S O)) s3 s4 s2 s1 s0 s p3), s5)), c)
      | _ -> assert false (* absurd case *)))

(** val adapt_to_suffix :
    nat -> nat -> nat -> nat -> color -> coloring -> coloring **)

let adapt_to_suffix _ _ q _ _ = function
| Gc (qp, _, nc) -> Gc (qp, q, nc)
| Yc (qp, _, nc) ->
  Yc (qp, (S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 O q)), nc)
| Oc (qp, _, nc) ->
  Oc (qp, (S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S O) q)), nc)
| Rc (qp, _, nc) ->
  Rc (qp, (S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S (S O)) q)), nc)
| Ec (qp, _) ->
  Ec (qp, (S
    (let rec add0 n m =
       match n with
       | O -> m
       | S p -> S (add0 p m)
     in add0 (S (S O)) q)))

(** val only_of_left :
    nat -> color -> 'a1 triple -> 'a1 six_stored_triple -> 'a1 triple **)

let only_of_left _ _ tl six =
  let Triple (lvl, _, _, _, _, _, _, r, n, c) = tl in
  (match r with
   | G (_, cl, cr) ->
     (match n with
      | Left (_, _, _, _, c0, p, s) ->
        (match c0 with
         | Gc (qp, qs, nc) ->
           let Coq_pair (p0, s0) = six in
           let Coq_pair (p1, s1) = p0 in
           let Coq_pair (p2, s2) = p1 in
           let Coq_pair (p3, s3) = p2 in
           let Coq_pair (s4, s5) = p3 in
           Triple (lvl, (S nc), Coq_only, (Mix (SomeGreen, NoYellow,
           NoOrange, NoRed)), cl, cr, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)), (G ((S nc), cl, cr)), (Only ((add (S (S (S O))) qp),
           (add (S (S (S O))) O), nc, (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)),
           (adapt_to_suffix (add (S (S (S O))) qp) (add (S (S (S O))) qs) O
             (S nc) (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) (Gc (qp, qs,
             nc))), p, (inject6 (S (S O)) s s4 s5 s3 s2 s1 s0))), c)
         | Ec (qp, _) ->
           (match c with
            | Empty _ ->
              let Coq_pair (p0, s0) = six in
              let Coq_pair (p1, s1) = p0 in
              let Coq_pair (p2, s2) = p1 in
              let Coq_pair (p3, s3) = p2 in
              let Coq_pair (s4, s5) = p3 in
              let Coq_pair (s6, s7) = two s in
              Triple (lvl, O, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
              (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
              NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen, NoYellow,
              NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
              NoRed)))), (Only_end ((S
              (let rec add0 n0 m =
                 match n0 with
                 | O -> m
                 | S p4 -> S (add0 p4 m)
               in add0 (S (S (S (S O)))) (S (S
                    (add (S (S (S (S (S O))))) (add O qp)))))),
              (inject6 (S (S (add (S (S (S (S (S O))))) (add O qp))))
                (inject2 (add (S (S (S (S (S O))))) (add O qp)) p s6 s7) s4
                s5 s3 s2 s1 s0))), (Empty (S lvl)))
            | _ -> assert false (* absurd case *))
         | _ -> assert false (* absurd case *))
      | _ -> assert false (* absurd case *))
   | Y (ck, cl, cr) ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = six in
        let Coq_pair (p1, s1) = p0 in
        let Coq_pair (p2, s2) = p1 in
        let Coq_pair (p3, s3) = p2 in
        let Coq_pair (s4, s5) = p3 in
        Triple (lvl, (S ck), Coq_only, (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)), cl, cr, cl, (Y (ck, cl, cr)), (Only (qp,
        (add (S (S (S O))) O), ck, (Mix (NoGreen, SomeYellow, NoOrange,
        NoRed)),
        (adapt_to_suffix qp qs O (S ck) (Mix (NoGreen, SomeYellow, NoOrange,
          NoRed)) c0), p, (inject6 (S (S O)) s s4 s5 s3 s2 s1 s0))), c)
      | _ -> assert false (* absurd case *))
   | OS c0 ->
     (match n with
      | Left (qp, qs, _, _, c1, p, s) ->
        let Coq_pair (p0, s0) = six in
        let Coq_pair (p1, s1) = p0 in
        let Coq_pair (p2, s2) = p1 in
        let Coq_pair (p3, s3) = p2 in
        let Coq_pair (s4, s5) = p3 in
        Triple (lvl, (S O), Coq_only, (Mix (NoGreen, NoYellow, SomeOrange,
        NoRed)), c0, c0, c0, (OS c0), (Only (qp, (add (S (S (S O))) O), O,
        (Mix (NoGreen, NoYellow, SomeOrange, NoRed)),
        (adapt_to_suffix qp qs O (S O) (Mix (NoGreen, NoYellow, SomeOrange,
          NoRed)) c1), p, (inject6 (S (S O)) s s4 s5 s3 s2 s1 s0))), c)
      | _ -> assert false (* absurd case *))
   | OP cr ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = six in
        let Coq_pair (p1, s1) = p0 in
        let Coq_pair (p2, s2) = p1 in
        let Coq_pair (p3, s3) = p2 in
        let Coq_pair (s4, s5) = p3 in
        Triple (lvl, (S (S O)), Coq_only, (Mix (NoGreen, NoYellow,
        SomeOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        cr, cr, (OP cr), (Only (qp, (add (S (S (S O))) O), (S O), (Mix
        (NoGreen, NoYellow, SomeOrange, NoRed)),
        (adapt_to_suffix qp qs O (S (S O)) (Mix (NoGreen, NoYellow,
          SomeOrange, NoRed)) c0), p,
        (inject6 (S (S O)) s s4 s5 s3 s2 s1 s0))), c)
      | _ -> assert false (* absurd case *))
   | R ck ->
     (match n with
      | Left (qp, qs, _, _, c0, p, s) ->
        let Coq_pair (p0, s0) = six in
        let Coq_pair (p1, s1) = p0 in
        let Coq_pair (p2, s2) = p1 in
        let Coq_pair (p3, s3) = p2 in
        let Coq_pair (s4, s5) = p3 in
        Triple (lvl, (S ck), Coq_only, (Mix (NoGreen, NoYellow, NoOrange,
        SomeRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (NoGreen, NoYellow,
        NoOrange, SomeRed)), (R ck), (Only (qp, (add (S (S (S O))) O), ck,
        (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
        (adapt_to_suffix qp qs O (S ck) (Mix (NoGreen, NoYellow, NoOrange,
          SomeRed)) c0), p, (inject6 (S (S O)) s s4 s5 s3 s2 s1 s0))), c)
      | _ -> assert false (* absurd case *)))

(** val pop_pair_green :
    nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod **)

let pop_pair_green _ = function
| Pair (lvl, _, _, c0, c1) ->
  let Coq_pair (s, p) =
    pop_left_green lvl
      (triple_of_chain lvl Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) c0)
  in
  (match p with
   | Zero_element (_, _) -> assert false (* absurd case *)
   | Six_elements (lvl0, _, s0) ->
     Coq_pair (s, (Semi (lvl0, (S O), (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
       (chain_of_triple lvl0 Coq_only (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed))
         (only_of_right lvl0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed)) s0
           (triple_of_chain lvl0 Coq_right (Mix (SomeGreen, NoYellow,
             NoOrange, NoRed)) c1))))))
   | Ok_pt (lvl0, _, _, c2, t) ->
     Coq_pair (s, (Semi (lvl0, (S (S O)), c2, (Mix (SomeGreen, NoYellow,
       NoOrange, NoRed)), (Pair (lvl0, c2, (Mix (SomeGreen, NoYellow,
       NoOrange, NoRed)), (chain_of_triple lvl0 Coq_left c2 t), c1))))))
| _ -> assert false (* absurd case *)

(** val eject_pair_green :
    nat -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod **)

let eject_pair_green _ = function
| Pair (lvl, _, _, c0, c1) ->
  let Coq_pair (p, s) =
    eject_right_green lvl
      (triple_of_chain lvl Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) c1)
  in
  (match p with
   | Zero_element (_, _) -> assert false (* absurd case *)
   | Six_elements (lvl0, _, s0) ->
     Coq_pair ((Semi (lvl0, (S O), (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
       (chain_of_triple lvl0 Coq_only (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed))
         (only_of_left lvl0 (Mix (SomeGreen, NoYellow, NoOrange, NoRed))
           (triple_of_chain lvl0 Coq_left (Mix (SomeGreen, NoYellow,
             NoOrange, NoRed)) c0) s0)))), s)
   | Ok_pt (lvl0, _, _, c2, t) ->
     Coq_pair ((Semi (lvl0, (S (S O)), (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), c2, (Pair (lvl0, (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), c2, c0, (chain_of_triple lvl0 Coq_right c2 t))))), s))
| _ -> assert false (* absurd case *)

(** val sandwich_pair_green :
    nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) sandwich **)

let sandwich_pair_green _ = function
| Pair (lvl, _, _, c0, c1) ->
  let Coq_pair (s, p) =
    pop_left_green lvl
      (triple_of_chain lvl Coq_left (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)) c0)
  in
  (match p with
   | Zero_element (_, _) -> assert false (* absurd case *)
   | Six_elements (_, _, s0) ->
     let Coq_pair (p0, s1) = s0 in
     let Coq_pair (p1, s2) = p0 in
     let Coq_pair (p2, s3) = p1 in
     let Coq_pair (p3, s4) = p2 in
     let Coq_pair (s5, s6) = p3 in
     let Coq_pair (p4, s7) =
       eject_right_green lvl
         (triple_of_chain lvl Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)) c1)
     in
     (match p4 with
      | Zero_element (_, _) -> assert false (* absurd case *)
      | Six_elements (lvl0, _, s8) ->
        let Coq_pair (p5, s9) = s8 in
        let Coq_pair (p6, s10) = p5 in
        let Coq_pair (p7, s11) = p6 in
        let Coq_pair (p8, s12) = p7 in
        let Coq_pair (s13, s14) = p8 in
        Sandwich (s, (Semi (lvl0, (S O), (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Single (lvl0,
        (S lvl0), O, Coq_only, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
        (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
        NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)))),
        (Packet (lvl0, lvl0, O, Coq_only, Coq_only, SomeGreen, NoRed, (Hole
        (lvl0, Coq_only)), (Only_end ((S
        (let rec add0 n m =
           match n with
           | O -> m
           | S p9 -> S (add0 p9 m)
         in add0 (S (S (S (S O)))) (add (S (S (S (S (S (S O)))))) O))),
        (inject6 (add (S (S (S (S (S (S O)))))) O)
          (push6 O s5 s6 s4 s3 s2 s1 empty) s13 s14 s12 s11 s10 s9))))),
        (Empty (S lvl0)))))), s7)
      | Ok_pt (lvl0, _, _, c2, t) ->
        Sandwich (s, (Semi (lvl0, (S O), c2, c2,
          (chain_of_triple lvl0 Coq_only c2
            (only_of_right lvl0 c2 (Coq_pair ((Coq_pair ((Coq_pair ((Coq_pair
              ((Coq_pair (s5, s6)), s4)), s3)), s2)), s1)) t)))), s7))
   | Ok_pt (_, _, _, c2, t) ->
     let Coq_pair (p0, s0) =
       eject_right_green lvl
         (triple_of_chain lvl Coq_right (Mix (SomeGreen, NoYellow, NoOrange,
           NoRed)) c1)
     in
     (match p0 with
      | Zero_element (_, _) -> assert false (* absurd case *)
      | Six_elements (lvl0, _, s1) ->
        Sandwich (s, (Semi (lvl0, (S O), c2, c2,
          (chain_of_triple lvl0 Coq_only c2 (only_of_left lvl0 c2 t s1)))),
          s0)
      | Ok_pt (lvl0, _, _, c3, t0) ->
        Sandwich (s, (Semi (lvl0, (S (S O)), c2, c3, (Pair (lvl0, c2, c3,
          (chain_of_triple lvl0 Coq_left c2 t),
          (chain_of_triple lvl0 Coq_right c3 t0))))), s0)))
| _ -> assert false (* absurd case *)

(** val pop_green :
    nat -> nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) prod **)

let pop_green lvl ck c =
  match ck with
  | O ->
    let Coq_pair (s, p) =
      pop_only_green lvl
        (triple_of_chain lvl Coq_only (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) c)
    in
    (match p with
     | Zero_element (lvl0, _) ->
       Coq_pair (s, (Semi (lvl0, O, (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Empty
         lvl0))))
     | Six_elements (_, _, _) -> assert false (* absurd case *)
     | Ok_pt (lvl0, _, _, c0, t) ->
       Coq_pair (s, (Semi (lvl0, (S O), c0, c0,
         (chain_of_triple lvl0 Coq_only c0 t)))))
  | S n ->
    (match n with
     | O -> pop_pair_green lvl c
     | S _ -> assert false (* absurd case *))

(** val eject_green :
    nat -> nat -> 'a1 chain -> ('a1 semi_cadeque, 'a1 stored_triple) prod **)

let eject_green lvl ck c =
  match ck with
  | O ->
    let Coq_pair (p, s) =
      eject_only_green lvl
        (triple_of_chain lvl Coq_only (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)) c)
    in
    (match p with
     | Zero_element (lvl0, _) ->
       Coq_pair ((Semi (lvl0, O, (Mix (SomeGreen, NoYellow, NoOrange,
         NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Empty
         lvl0))), s)
     | Six_elements (_, _, _) -> assert false (* absurd case *)
     | Ok_pt (lvl0, _, _, c0, t) ->
       Coq_pair ((Semi (lvl0, (S O), c0, c0,
         (chain_of_triple lvl0 Coq_only c0 t))), s))
  | S n ->
    (match n with
     | O -> eject_pair_green lvl c
     | S _ -> assert false (* absurd case *))

(** val sandwich_green :
    nat -> nat -> 'a1 chain -> ('a1 stored_triple, 'a1 semi_cadeque) sandwich **)

let sandwich_green lvl ck c =
  match ck with
  | O ->
    (match sandwich_only_green lvl
             (triple_of_chain lvl Coq_only (Mix (SomeGreen, NoYellow,
               NoOrange, NoRed)) c) with
     | Alone a -> Alone a
     | Sandwich (a, b, a0) ->
       (match b with
        | Zero_element (lvl0, _) ->
          Sandwich (a, (Semi (lvl0, O, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Empty
            lvl0))), a0)
        | Six_elements (_, _, _) -> assert false (* absurd case *)
        | Ok_pt (lvl0, _, _, c0, t) ->
          Sandwich (a, (Semi (lvl0, (S O), c0, c0,
            (chain_of_triple lvl0 Coq_only c0 t))), a0)))
  | S n ->
    (match n with
     | O -> sandwich_pair_green lvl c
     | S _ -> assert false (* absurd case *))

(** val make_green_prefix :
    nat -> nat -> nat -> 'a1 prefix -> 'a1 prefix -> 'a1 semi_cadeque -> ('a1
    green_buffer, 'a1 semi_cadeque) prod **)

let make_green_prefix lvl q qstored p pstored child =
  let Coq_pair (p0, s) = has3p qstored pstored in
  let Coq_pair (p1, s0) = p0 in
  let Coq_pair (s1, s2) = p1 in
  (match s with
   | Coq_inl v ->
     Coq_pair ((Gbuf
       ((let rec add0 n m =
           match n with
           | O -> m
           | S p2 -> S (add0 p2 m)
         in add0 q (vector_size (S (S O)) v)),
       (inject_vector (S (S O))
         (add (S (S (S O))) (add (S (S (S (S (S O))))) q))
         (inject3 (add (S (S (S (S (S O))))) q) p s1 s2 s0) v))), child)
   | Coq_inr p2 ->
     Coq_pair ((Gbuf (q,
       (inject3 (add (S (S (S (S (S O))))) q) p s1 s2 s0))),
       (semi_push (S lvl) (Small (lvl, (iter (S (S (S O))) pred qstored),
         p2)) child)))

(** val make_green_suffix :
    nat -> nat -> nat -> 'a1 semi_cadeque -> 'a1 suffix -> 'a1 suffix -> ('a1
    semi_cadeque, 'a1 green_buffer) prod **)

let make_green_suffix lvl q qstored child sstored s =
  let Coq_pair (s0, p) = has3s qstored sstored in
  (match s0 with
   | Coq_inl v ->
     let Coq_pair (p0, s1) = p in
     let Coq_pair (s2, s3) = p0 in
     Coq_pair (child, (Gbuf
     ((let rec add0 n m =
         match n with
         | O -> m
         | S p1 -> S (add0 p1 m)
       in add0 q (vector_size (S (S O)) v)),
     (push_vector (S (S O)) (add (S (S (S O))) (add (S (S (S (S (S O))))) q))
       v (push3 (add (S (S (S (S (S O))))) q) s2 s3 s1 s)))))
   | Coq_inr p0 ->
     let Coq_pair (p1, s1) = p in
     let Coq_pair (s2, s3) = p1 in
     Coq_pair
     ((semi_inject (S lvl) child (Small (lvl,
        (iter (S (S (S O))) pred qstored), p0))), (Gbuf (q,
     (push3 (add (S (S (S (S (S O))))) q) s2 s3 s1 s)))))

(** val extract_prefix :
    nat -> 'a1 stored_triple -> 'a1 semi_cadeque -> ('a1 stored_buffer, 'a1
    semi_cadeque) prod **)

let extract_prefix lvl stored child =
  match stored with
  | Ground _ -> assert false (* absurd case *)
  | Small (_, q, s) -> Coq_pair ((Sbuf (q, s)), child)
  | Big (_, qp, qs, ck, cl, cr, p, c, s) ->
    Coq_pair ((Sbuf (qp, p)),
      (semi_concat (S lvl) (Semi ((S lvl), ck, cl, cr, c))
        (semi_push (S lvl) (Small (lvl, qs, s)) child)))

(** val extract_suffix :
    nat -> 'a1 semi_cadeque -> 'a1 stored_triple -> ('a1 semi_cadeque, 'a1
    stored_buffer) prod **)

let extract_suffix lvl child = function
| Ground _ -> assert false (* absurd case *)
| Small (_, q, s) -> Coq_pair (child, (Sbuf (q, s)))
| Big (_, qp, qs, ck, cl, cr, p, c, s) ->
  Coq_pair
    ((semi_concat (S lvl) (semi_inject (S lvl) child (Small (lvl, qp, p)))
       (Semi ((S lvl), ck, cl, cr, c))), (Sbuf (qs, s)))

(** val ensure_green_prefix :
    nat -> nat -> nat -> 'a1 prefix -> 'a1 chain -> ('a1 green_buffer, 'a1
    semi_cadeque) prod **)

let ensure_green_prefix lvl q ck p child =
  let Coq_pair (s, s0) = pop_green (S lvl) ck child in
  let Coq_pair (s1, s2) = extract_prefix lvl s s0 in
  let Sbuf (q0, t) = s1 in make_green_prefix lvl q q0 p t s2

(** val ensure_green_suffix :
    nat -> nat -> nat -> 'a1 chain -> 'a1 suffix -> ('a1 semi_cadeque, 'a1
    green_buffer) prod **)

let ensure_green_suffix lvl q ck child s =
  let Coq_pair (s0, s1) = eject_green (S lvl) ck child in
  let Coq_pair (s2, s3) = extract_suffix lvl s0 s1 in
  let Sbuf (q0, t) = s3 in make_green_suffix lvl q q0 s2 t s

(** val green_of_red_left_obligations_obligation_1 : nat -> nat **)

let green_of_red_left_obligations_obligation_1 q =
  q

(** val green_of_red_left_obligations_obligation_3 : nat -> nat **)

let green_of_red_left_obligations_obligation_3 ck1 =
  ck1

(** val green_of_red_left_obligations_obligation_5 : nat -> nat **)

let green_of_red_left_obligations_obligation_5 q =
  q

(** val green_of_red_left :
    nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1
    chain **)

let green_of_red_left hlvl tlvl hk ck bd red child =
  match red with
  | Left (_, _, _, _, c, p, s) ->
    (match c with
     | Rc (qp, _, _) ->
       let Coq_pair (g, s0) = ensure_green_prefix tlvl (add O qp) ck p child
       in
       let Gbuf (q, t) = g in
       let Semi (_, _, _, _, c0) = s0 in
       (match c0 with
        | Empty _ ->
          Single (hlvl, (S tlvl), O, hk, (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen,
            NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
            NoRed)))), (Packet (hlvl, tlvl, O, hk, Coq_left, SomeGreen,
            NoRed, bd, (Left
            ((add O (S
               (let rec add0 n m =
                  match n with
                  | O -> m
                  | S p0 -> S (add0 p0 m)
                in add0 (S (S O)) q))),
            (add O (green_of_red_left_obligations_obligation_1 q)), O, (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)), (Ec ((S
            (let rec add0 n m =
               match n with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) q)),
            (green_of_red_left_obligations_obligation_1 q))), t, s)))),
            (Empty (S tlvl)))
        | Single (_, tlvl0, ck0, _, c1, cl, cr, r, p0, c2) ->
          Single (hlvl, (S tlvl), (S O), hk, (Mix (SomeGreen, NoYellow,
            NoOrange, NoRed)), c1, c1, (G ((S O), c1, c1)), (Packet (hlvl,
            tlvl, (S O), hk, Coq_left, SomeGreen, NoRed, bd, (Left
            ((add (S (S (S O))) q),
            (add (S (S (S O)))
              (green_of_red_left_obligations_obligation_3 ck0)), (S O), (Mix
            (SomeGreen, NoYellow, NoOrange, NoRed)), (Gc (q,
            (green_of_red_left_obligations_obligation_3 ck0), O)), t, s)))),
            (Single ((S tlvl), tlvl0, ck0, Coq_only, c1, cl, cr, r, p0, c2)))
        | Pair (_, cl, cr, c1, c2) ->
          Single (hlvl, (S tlvl), (S (S O)), hk, (Mix (SomeGreen, NoYellow,
            NoOrange, NoRed)), cl, cr, (G ((S (S O)), cl, cr)), (Packet
            (hlvl, tlvl, (S (S O)), hk, Coq_left, SomeGreen, NoRed, bd, (Left
            ((add (S (S (S O))) q),
            (add (S (S (S O))) (green_of_red_left_obligations_obligation_5 q)),
            (S (S O)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Gc (q,
            (green_of_red_left_obligations_obligation_5 q), (S O))), t,
            s)))), (Pair ((S tlvl), cl, cr, c1, c2))))
     | _ -> assert false (* absurd case *))
  | _ -> assert false (* absurd case *)

(** val green_of_red_right_obligations_obligation_1 : nat -> nat **)

let green_of_red_right_obligations_obligation_1 q =
  q

(** val green_of_red_right_obligations_obligation_3 : nat -> nat **)

let green_of_red_right_obligations_obligation_3 q =
  q

(** val green_of_red_right_obligations_obligation_5 : nat -> nat **)

let green_of_red_right_obligations_obligation_5 q =
  q

(** val green_of_red_right :
    nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1
    chain **)

let green_of_red_right hlvl tlvl hk ck bd red child =
  match red with
  | Right (_, _, _, _, c, p, s) ->
    (match c with
     | Rc (_, qs, _) ->
       let Coq_pair (s0, g) = ensure_green_suffix tlvl (add O qs) ck child s
       in
       let Semi (_, _, _, _, c0) = s0 in
       (match c0 with
        | Empty _ ->
          let Gbuf (q, t) = g in
          Single (hlvl, (S tlvl), O, hk, (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
          (SomeGreen, NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen,
          NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)))), (Packet (hlvl, tlvl, O, hk, Coq_right, SomeGreen, NoRed,
          bd, (Right
          ((add O (green_of_red_right_obligations_obligation_1 q)),
          (add O (S
            (let rec add0 n m =
               match n with
               | O -> m
               | S p0 -> S (add0 p0 m)
             in add0 (S (S O)) q))), O, (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), (Ec ((green_of_red_right_obligations_obligation_1 q), (S
          (let rec add0 n m =
             match n with
             | O -> m
             | S p0 -> S (add0 p0 m)
           in add0 (S (S O)) q)))), p, t)))), (Empty (S tlvl)))
        | Single (_, tlvl0, ck0, _, c1, cl, cr, r, p0, c2) ->
          let Gbuf (q, t) = g in
          Single (hlvl, (S tlvl), (S O), hk, (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)), c1, c1, (G ((S O), c1, c1)), (Packet (hlvl,
          tlvl, (S O), hk, Coq_right, SomeGreen, NoRed, bd, (Right
          ((add (S (S (S O))) (green_of_red_right_obligations_obligation_3 q)),
          (add (S (S (S O))) q), (S O), (Mix (SomeGreen, NoYellow, NoOrange,
          NoRed)), (Gc ((green_of_red_right_obligations_obligation_3 q), q,
          O)), p, t)))), (Single ((S tlvl), tlvl0, ck0, Coq_only, c1, cl, cr,
          r, p0, c2)))
        | Pair (_, cl, cr, c1, c2) ->
          let Gbuf (q, t) = g in
          Single (hlvl, (S tlvl), (S (S O)), hk, (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)), cl, cr, (G ((S (S O)), cl, cr)), (Packet (hlvl,
          tlvl, (S (S O)), hk, Coq_right, SomeGreen, NoRed, bd, (Right
          ((add (S (S (S O))) (green_of_red_right_obligations_obligation_5 q)),
          (add (S (S (S O))) q), (S (S O)), (Mix (SomeGreen, NoYellow,
          NoOrange, NoRed)), (Gc
          ((green_of_red_right_obligations_obligation_5 q), q, (S O))), p,
          t)))), (Pair ((S tlvl), cl, cr, c1, c2))))
     | _ -> assert false (* absurd case *))
  | _ -> assert false (* absurd case *)

(** val make_green_only :
    nat -> nat -> nkind -> nat -> nat -> 'a1 body -> 'a1 prefix -> 'a1
    semi_cadeque -> 'a1 prefix -> 'a1 chain **)

let make_green_only hlvl tlvl hk qp qs bd p child s =
  let Semi (_, _, _, _, c) = child in
  (match c with
   | Empty _ ->
     (match has3p8 qs s with
      | Coq_inl p0 ->
        let Coq_pair (p1, v) = p0 in
        let Coq_pair (p2, s0) = p1 in
        let Coq_pair (p3, s1) = p2 in
        let Coq_pair (p4, s2) = p3 in
        let Coq_pair (p5, s3) = p4 in
        let Coq_pair (p6, s4) = p5 in
        let Coq_pair (p7, s5) = p6 in
        let Coq_pair (s6, s7) = p7 in
        Single (hlvl, (S tlvl), O, hk, (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (G (O, (Mix (SomeGreen,
        NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)))), (Packet (hlvl, tlvl, O, hk, Coq_only, SomeGreen, NoRed,
        bd, (Only_end ((S
        (let rec add0 n m =
           match n with
           | O -> m
           | S p8 -> S (add0 p8 m)
         in add0
              (let rec add0 n m =
                 match n with
                 | O -> m
                 | S p8 -> S (add0 p8 m)
               in add0 (S (S (S (S (S (S O))))))
                    (add (S (S (S (S (S (S (S (S O)))))))) qp))
              (vector_size (S (S O)) v))),
        (inject_vector (S (S O))
          (add (S (S (S (S (S (S (S (S O))))))))
            (add (S (S (S (S (S (S (S (S O)))))))) qp))
          (inject8 (add (S (S (S (S (S (S (S (S O)))))))) qp) p s6 s7 s5 s4
            s3 s2 s1 s0) v))))), (Empty (S tlvl)))
      | Coq_inr p0 ->
        let Coq_pair (t, p1) = p0 in
        Single (hlvl, (S tlvl), (S O), hk, (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
        (SomeGreen, NoYellow, NoOrange, NoRed)), (G ((S O), (Mix (SomeGreen,
        NoYellow, NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
        NoRed)))), (Packet (hlvl, tlvl, (S O), hk, Coq_only, SomeGreen,
        NoRed, bd, (Only ((add (S (S (S O))) qp),
        (add (S (S (S O)))
          (iter (S (S (S (S (S (S (S (S O)))))))) pred
            (add (S (S (S (S (S O))))) qs))), O, (Mix (SomeGreen, NoYellow,
        NoOrange, NoRed)), (Gc (qp,
        (iter (S (S (S (S (S (S (S (S O)))))))) pred
          (add (S (S (S (S (S O))))) qs)), O)), p, p1)))),
        (single_chain (S tlvl) (Small (tlvl, O, t)))))
   | Single (_, tlvl0, ck, _, c0, cl, cr, r, p0, c1) ->
     Single (hlvl, (S tlvl), (S O), hk, (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), c0, c0, (G ((S O), c0, c0)), (Packet (hlvl, tlvl, (S O), hk,
       Coq_only, SomeGreen, NoRed, bd, (Only ((add (S (S (S O))) qp),
       (add (S (S (S O))) qs), O, (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), (Gc (qp, qs, O)), p, s)))), (Single ((S tlvl), tlvl0, ck,
       Coq_only, c0, cl, cr, r, p0, c1)))
   | Pair (_, cl, cr, c0, c1) ->
     Single (hlvl, (S tlvl), (S (S O)), hk, (Mix (SomeGreen, NoYellow,
       NoOrange, NoRed)), cl, cr, (G ((S (S O)), cl, cr)), (Packet (hlvl,
       tlvl, (S (S O)), hk, Coq_only, SomeGreen, NoRed, bd, (Only
       ((add (S (S (S O))) qp), (add (S (S (S O))) qs), (S O), (Mix
       (SomeGreen, NoYellow, NoOrange, NoRed)), (Gc (qp, qs, (S O))), p,
       s)))), (Pair ((S tlvl), cl, cr, c0, c1))))

(** val green_of_red_only :
    nat -> nat -> nkind -> nat -> 'a1 body -> 'a1 node -> 'a1 chain -> 'a1
    chain **)

let green_of_red_only hlvl tlvl hk ck bd red child =
  match red with
  | Only (_, _, _, _, c, p, s) ->
    (match c with
     | Rc (qp, qs, _) ->
       (match has8 (add O qp) p with
        | Coq_inl p0 ->
          let Coq_pair (p1, v) = p0 in
          let Coq_pair (p2, s0) = p1 in
          let Coq_pair (p3, s1) = p2 in
          let Coq_pair (p4, s2) = p3 in
          let Coq_pair (s3, s4) = p4 in
          (match has8 (add O qs) s with
           | Coq_inl p5 ->
             let Coq_pair (p6, v0) = p5 in
             let Coq_pair (p7, s5) = p6 in
             let Coq_pair (p8, s6) = p7 in
             let Coq_pair (p9, s7) = p8 in
             let Coq_pair (s8, s9) = p9 in
             (match sandwich_green (S tlvl) ck child with
              | Alone a ->
                (match a with
                 | Ground _ -> assert false (* absurd case *)
                 | Small (_, q, s10) ->
                   Single (hlvl, (S tlvl), O, hk, (Mix (SomeGreen, NoYellow,
                     NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
                     NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                     (G (O, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)),
                     (Mix (SomeGreen, NoYellow, NoOrange, NoRed)))), (Packet
                     (hlvl, tlvl, O, hk, Coq_only, SomeGreen, NoRed, bd,
                     (Only_end ((S
                     (let rec add0 n m =
                        match n with
                        | O -> m
                        | S p10 -> S (add0 p10 m)
                      in add0
                           (let rec add0 n m =
                              match n with
                              | O -> m
                              | S p10 -> S (add0 p10 m)
                            in add0 (S (S (S O)))
                                 (add
                                   (add (S (S (S (S (S O)))))
                                     (add (S (S (S O))) q))
                                   (vector_size (S (S O)) v)))
                           (vector_size (S (S O)) v0))),
                     (inject_5vector (S (S O))
                       (add (add (S (S (S (S (S O))))) (add (S (S (S O))) q))
                         (vector_size (S (S O)) v))
                       (push_5vector (S (S O)) (add (S (S (S O))) q) s3 s4 s2
                         s1 s0 v s10) s8 s9 s7 s6 s5 v0))))), (Empty (S
                     tlvl)))
                 | Big (_, qp0, qs0, ck0, cl, cr, p10, c0, s10) ->
                   make_green_only hlvl tlvl hk
                     (let rec add0 n m =
                        match n with
                        | O -> m
                        | S p11 -> S (add0 p11 m)
                      in add0 qp0 (vector_size (S (S O)) v))
                     (let rec add0 n m =
                        match n with
                        | O -> m
                        | S p11 -> S (add0 p11 m)
                      in add0 qs0 (vector_size (S (S O)) v0)) bd
                     (push_5vector (S (S O)) (add (S (S (S O))) qp0) s3 s4 s2
                       s1 s0 v p10) (Semi ((S tlvl), ck0, cl, cr, c0))
                     (inject_5vector (S (S O)) (add (S (S (S O))) qs0) s10 s8
                       s9 s7 s6 s5 v0))
              | Sandwich (a, b, a0) ->
                let Coq_pair (s10, s11) = extract_prefix tlvl a b in
                let Sbuf (q, t) = s10 in
                let Coq_pair (s12, s13) = extract_suffix tlvl s11 a0 in
                let Sbuf (q0, t0) = s13 in
                make_green_only hlvl tlvl hk
                  (let rec add0 n m =
                     match n with
                     | O -> m
                     | S p10 -> S (add0 p10 m)
                   in add0 q (vector_size (S (S O)) v))
                  (let rec add0 n m =
                     match n with
                     | O -> m
                     | S p10 -> S (add0 p10 m)
                   in add0 q0 (vector_size (S (S O)) v0)) bd
                  (push_5vector (S (S O)) (add (S (S (S O))) q) s3 s4 s2 s1
                    s0 v t) s12
                  (inject_5vector (S (S O)) (add (S (S (S O))) q0) t0 s8 s9
                    s7 s6 s5 v0))
           | Coq_inr p5 ->
             let Coq_pair (g, s5) =
               ensure_green_prefix tlvl (add O qp) ck p child
             in
             let Gbuf (q, t) = g in
             make_green_only hlvl tlvl hk q
               (iter (S (S (S (S (S (S (S (S O)))))))) pred
                 (add (S (S (S (S (S O))))) (add O qs))) bd t s5 p5)
        | Coq_inr p0 ->
          (match has8 (add O qs) s with
           | Coq_inl _ ->
             let Coq_pair (s0, g) =
               ensure_green_suffix tlvl (add O qs) ck child s
             in
             let Gbuf (q, t) = g in
             make_green_only hlvl tlvl hk
               (iter (S (S (S (S (S (S (S (S O)))))))) pred
                 (add (S (S (S (S (S O))))) (add O qp))) q bd p0 s0 t
           | Coq_inr p1 ->
             Single (hlvl, (S tlvl), (S ck), hk, (Mix (SomeGreen, NoYellow,
               NoOrange, NoRed)), (Mix (SomeGreen, NoYellow, NoOrange,
               NoRed)), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (G ((S
               ck), (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix
               (SomeGreen, NoYellow, NoOrange, NoRed)))), (Packet (hlvl,
               tlvl, (S ck), hk, Coq_only, SomeGreen, NoRed, bd, (Only
               ((add (S (S (S O)))
                  (iter (S (S (S (S (S (S (S (S O)))))))) pred
                    (add (S (S (S (S (S O))))) (add O qp)))),
               (add (S (S (S O)))
                 (iter (S (S (S (S (S (S (S (S O)))))))) pred
                   (add (S (S (S (S (S O))))) (add O qs)))), ck, (Mix
               (SomeGreen, NoYellow, NoOrange, NoRed)), (Gc
               ((iter (S (S (S (S (S (S (S (S O)))))))) pred
                  (add (S (S (S (S (S O))))) (add O qp))),
               (iter (S (S (S (S (S (S (S (S O)))))))) pred
                 (add (S (S (S (S (S O))))) (add O qs))), ck)), p0, p1)))),
               child)))
     | _ -> assert false (* absurd case *))
  | _ -> assert false (* absurd case *)

(** val ensure_green_obligations_obligation_4 : nat -> nat **)

let ensure_green_obligations_obligation_4 qs3 =
  qs3

(** val ensure_green_obligations_obligation_6 : nat -> nat **)

let ensure_green_obligations_obligation_6 qs3 =
  qs3

(** val ensure_green :
    nat -> nat -> nkind -> color -> color -> 'a1 chain -> 'a1 chain **)

let rec ensure_green _ _ _ _ _ = function
| Empty lvl -> Empty lvl
| Single (hlvl, tlvl, _, nk, _, _, _, r, p, c0) ->
  (match r with
   | G (ck, cl, cr) ->
     Single (hlvl, tlvl, ck, nk, (Mix (SomeGreen, NoYellow, NoOrange,
       NoRed)), cl, cr, (G (ck, cl, cr)), p, c0)
   | R ck ->
     let Packet (hlvl0, tlvl0, _, hk, _, _, _, b, n) = p in
     (match n with
      | Only_end (_, _) -> assert false (* absurd case *)
      | Only (_, _, _, _, c1, p0, s) ->
        (match c1 with
         | Rc (qp, qs, _) ->
           green_of_red_only hlvl0 tlvl0 hk ck b (Only ((add O qp),
             (add O qs), ck, (Mix (NoGreen, NoYellow, NoOrange, SomeRed)),
             (Rc (qp, qs, ck)), p0, s)) c0
         | _ -> assert false (* absurd case *))
      | Left (_, _, _, _, c1, p0, s) ->
        (match c1 with
         | Rc (qp, qs, _) ->
           green_of_red_left hlvl0 tlvl0 hk ck b (Left ((add O qp),
             (add O (ensure_green_obligations_obligation_4 qs)), (S ck), (Mix
             (NoGreen, NoYellow, NoOrange, SomeRed)), (Rc (qp,
             (ensure_green_obligations_obligation_4 qs), ck)), p0, s)) c0
         | _ -> assert false (* absurd case *))
      | Right (_, _, _, _, c1, p0, s) ->
        (match c1 with
         | Rc (_, qs, _) ->
           green_of_red_right hlvl0 tlvl0 hk ck b (Right
             ((add O (ensure_green_obligations_obligation_6 qs)), (add O qs),
             (S ck), (Mix (NoGreen, NoYellow, NoOrange, SomeRed)), (Rc
             ((ensure_green_obligations_obligation_6 qs), qs, ck)), p0, s)) c0
         | _ -> assert false (* absurd case *)))
   | _ -> assert false (* absurd case *))
| Pair (lvl, cl, cr, c0, c1) ->
  Pair (lvl, (Mix (SomeGreen, NoYellow, NoOrange, NoRed)), (Mix (SomeGreen,
    NoYellow, NoOrange, NoRed)), (ensure_green lvl (S O) Coq_left cl cl c0),
    (Obj.magic ensure_green lvl (S O) Coq_right cr cr c1))
