From Coq Require Import Program List Arith Lia.
Import ListNotations.
From Equations Require Import Equations.
From Hammer Require Import Tactics.
From AAC_tactics Require Import AAC.
From AAC_tactics Require Import Instances.
Import Instances.Lists.

From Theory.color Require Import GYR.

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for products. *)
Inductive prodN (A : Type) : nat -> Type :=
  | prodZ : A -> prodN A 0
  | prodS {n : nat} : prodN A n -> prodN A n -> prodN A (S n).
Arguments prodZ {A}.
Arguments prodS {A n}.

(* A type for sized options. *)
Inductive optionN (A : Type) (lvl : nat) : nat -> Type :=
  | NoneN : optionN A lvl 0
  | SomeN : prodN A lvl -> optionN A lvl 1.
Arguments NoneN {A lvl}.
Arguments SomeN {A lvl}.

(* In the following types, an natural number parameter is introduced : the
   [size] of the type. The size is simply the number of [prodN A lvl] that are
   stored in the structure encoded. *)

(* A type for sized buffers. *)
Inductive buffer (A : Type) (lvl : nat) : nat -> color -> Type :=
  | B0         : buffer A lvl 0 red
  | B1 {y r}   : prodN A lvl -> buffer A lvl 1 (Mix NoGreen y r)
  | B2 {g y r} : prodN A lvl -> prodN A lvl -> buffer A lvl 2 (Mix g y r)
  | B3 {g y r} : prodN A lvl -> prodN A lvl -> prodN A lvl ->
                 buffer A lvl 3 (Mix g y r)
  | B4 {y r}   : prodN A lvl -> prodN A lvl -> prodN A lvl ->
                 prodN A lvl -> buffer A lvl 4 (Mix NoGreen y r)
  | B5         : prodN A lvl -> prodN A lvl -> prodN A lvl ->
                 prodN A lvl -> prodN A lvl -> buffer A lvl 5 red.
Arguments B0 {A lvl}.
Arguments B1 {A lvl y r}.
Arguments B2 {A lvl g y r}.
Arguments B3 {A lvl g y r}.
Arguments B4 {A lvl y r}.
Arguments B5 {A lvl}.

(* A type for sized packets. *)
Inductive packet (A : Type) (lvl : nat) : nat -> nat -> nat -> color -> Type :=
  | Hole {size : nat} : packet A lvl lvl size size uncolored
  | Packet {hlvl psize pktsize ssize hsize C y} :
      buffer A lvl psize C ->
      packet A (S lvl) hlvl pktsize hsize (Mix NoGreen y NoRed) ->
      buffer A lvl ssize C ->
      packet A lvl hlvl (psize + pktsize + pktsize + ssize) hsize C.
Arguments Hole {A lvl size}.
Arguments Packet {A lvl hlvl psize pktsize ssize hsize C y}.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> Type :=
  | G {g r} : regularity green  (Mix g NoYellow r)
  | Y       : regularity yellow green
  | R       : regularity red    green.

(* A type for sized chains. *)
Inductive chain (A : Type) (lvl : nat) : nat -> color -> Type :=
  | Ending {size : nat} {C : color} :
      buffer A lvl size C ->
      chain A lvl size green
  | Chain {hlvl size hsize : nat} {C1 C2 : color} :
      regularity C1 C2 ->
      packet A lvl hlvl size hsize C1 ->
      chain A hlvl hsize C2 ->
      chain A lvl size C1.
Arguments Ending {A lvl size C}.
Arguments Chain {A lvl hlvl size hsize C1 C2}.

(* A type decomposing buffers according to their number of elements.
   Buffers with 0 or 1 element are decomposed into [Underflow];
   buffers with 2 or 3 elements are decomposed into [Ok];
   buffers with 4 or 5 elements are decomposed into [Overflow]. *)
Inductive decompose (A : Type) (lvl : nat) : nat -> Type :=
  | Underflow {size : nat} : optionN A lvl size -> decompose A lvl size
  | Ok {size : nat} : buffer A lvl size green -> decompose A lvl size
  | Overflow {size : nat} :
    buffer A lvl size green -> prodN A (S lvl) -> decompose A lvl (S (S size)).
Arguments Underflow {A lvl size}.
Arguments Ok {A lvl size}.
Arguments Overflow {A lvl size}.

(* A type decomposing a buffer into its first element, a central buffer, and
   its last element. If such a decomposition is not possible, an option
   representing the buffer is returned with [Alone]. *)
Inductive sandwich (A : Type) (lvl : nat) : nat -> Type :=
  | Alone {s : nat} : optionN A lvl s -> sandwich A lvl s
  | Sandwich {s : nat} {C : color} :
    prodN A lvl -> buffer A lvl s C -> prodN A lvl -> sandwich A lvl (2 + s).
Arguments Alone {A lvl s}.
Arguments Sandwich {A lvl s C}.

(* A type for sized deque. *)
Inductive deque (A : Type) (size : nat) : Type :=
  | T {g y} : chain A 0 size (Mix g y NoRed) -> deque A size.
Arguments T {A size g y}.

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* Model functions are transparent. *)
Set Equations Transparent.

(* The [app] function and the singleton list are made opaque. *)
Opaque app.
Definition singleton {A : Type} (x : A) : list A := [x].
Opaque singleton.

(* Returns the sequence associated to a product. *)
Equations prodN_seq {A n} : prodN A n -> list A :=
prodN_seq (prodZ a) := [a];
prodN_seq (prodS p1 p2) := prodN_seq p1 ++ prodN_seq p2.

(* Returns the sequence associated to an option. *)
Equations optionN_seq {A lvl size} : optionN A lvl size -> list A :=
optionN_seq NoneN := [];
optionN_seq (SomeN x) := prodN_seq x.

(* Returns the sequence associated to a buffer. *)
Equations buffer_seq {A lvl size C} : buffer A lvl size C -> list A :=
buffer_seq B0 := [];
buffer_seq (B1 a) := prodN_seq a;
buffer_seq (B2 a b) := prodN_seq a ++ prodN_seq b;
buffer_seq (B3 a b c) := prodN_seq a ++ prodN_seq b ++ prodN_seq c;
buffer_seq (B4 a b c d) := prodN_seq a ++ prodN_seq b ++ prodN_seq c ++
                           prodN_seq d;
buffer_seq (B5 a b c d e) := prodN_seq a ++ prodN_seq b ++ prodN_seq c ++
                             prodN_seq d ++ prodN_seq e.

(* Returns the sequence associated to a packet, provided the sequence
   associated to its hole. *)
Equations packet_seq {A lvl hlvl pktsize hsize C} :
  packet A lvl hlvl pktsize hsize C -> list A -> list A :=
packet_seq Hole l := l;
packet_seq (Packet p pkt s) l :=
  buffer_seq p ++ packet_seq pkt l ++ buffer_seq s.

(* Returns the sequence associated to a chain. *)
Equations chain_seq {A lvl size C} : chain A lvl size C -> list A :=
chain_seq (Ending b) := buffer_seq b;
chain_seq (Chain _ pkt c) := packet_seq pkt (chain_seq c).

(* Returns the first 4 elements of the sequence associated to a decomposed
   buffer.*)
Equations decompose_main_seq {A lvl size} : decompose A lvl size -> list A :=
decompose_main_seq (Underflow opt) := optionN_seq opt;
decompose_main_seq (Ok b) := buffer_seq b;
decompose_main_seq (Overflow b _) := buffer_seq b.

(* Returns the sequence associated to a decomposed buffer from the 5th element
   to the end  *)
Equations decompose_rest_seq {A lvl size} : decompose A lvl size -> list A :=
decompose_rest_seq (Underflow _) := [];
decompose_rest_seq (Ok _) := [];
decompose_rest_seq (Overflow _ p) := prodN_seq p.

(* Returns the sequence associated to a sandwiched buffer. *)
Equations sandwich_seq {A lvl size} : sandwich A lvl size -> list A :=
sandwich_seq (Alone opt) := optionN_seq opt;
sandwich_seq (Sandwich x b y) := prodN_seq x ++ buffer_seq b ++ prodN_seq y.

(* Returns the sequence associated to a deque. *)
Equations deque_seq {A size} : deque A size -> list A :=
deque_seq (T dq) := chain_seq dq.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Notation for dependent types hiding the property on [x]. *)
Notation "? x" := (@exist _ _ x _) (at level 100).

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations on lists generated by [Equations]. *)
#[export] Hint Rewrite <-app_assoc : rlist.
#[export] Hint Rewrite app_nil_r : rlist.
#[export] Hint Rewrite map_app : rlist.
#[export] Hint Rewrite concat_app : rlist.

(* Setting the default tactics for obligations to be [hauto] using the [rlist]
   hint database. *)
#[local] Obligation Tactic := try (cbn; hauto db:rlist).

(* Pushes on a green buffer. *)
Equations green_push {A lvl size}
  (x : prodN A lvl) (b : buffer A lvl size green) :
  { b' : buffer A lvl (S size) yellow |
    buffer_seq b' = prodN_seq x ++ buffer_seq b } :=
green_push x (B2 a b)   := ? B3 x a b;
green_push x (B3 a b c) := ? B4 x a b c.

(* Injects on a green buffer. *)
Equations green_inject {A lvl size}
  (b : buffer A lvl size green) (x : prodN A lvl) :
  { b' : buffer A lvl (S size) yellow |
    buffer_seq b' = buffer_seq b ++ prodN_seq x } :=
green_inject (B2 a b)   x := ? B3 a b x;
green_inject (B3 a b c) x := ? B4 a b c x.

(* Pops off a green buffer. *)
Equations green_pop {A lvl size} (b : buffer A lvl size green) :
  { '(x, b') : prodN A lvl * buffer A lvl (Nat.pred size) yellow |
    buffer_seq b = prodN_seq x ++ buffer_seq b' } :=
green_pop (B2 a b)   := ? (a, B1 b);
green_pop (B3 a b c) := ? (a, B2 b c).

(* Ejects off a green buffer. *)
Equations green_eject {A lvl size} (b : buffer A lvl size green) :
  { '(b', x) : buffer A lvl (Nat.pred size) yellow * prodN A lvl |
    buffer_seq b = buffer_seq b' ++ prodN_seq x } :=
green_eject (B2 a b)   := ? (B1 a, b);
green_eject (B3 a b c) := ? (B2 a b, c).

(* Pushes on a yellow buffer. *)
Equations yellow_push {A lvl size}
  (x : prodN A lvl) (b : buffer A lvl size yellow) :
  { b' : buffer A lvl (S size) red |
    buffer_seq b' = prodN_seq x ++ buffer_seq b } :=
yellow_push x (B1 a)       := ? B2 x a;
yellow_push x (B2 a b)     := ? B3 x a b;
yellow_push x (B3 a b c)   := ? B4 x a b c;
yellow_push x (B4 a b c d) := ? B5 x a b c d.

(* Injects on a yellow buffer. *)
Equations yellow_inject {A lvl size}
  (b : buffer A lvl size yellow) (x : prodN A lvl) :
  { b' : buffer A lvl (S size) red |
    buffer_seq b' = buffer_seq b ++ prodN_seq x } :=
yellow_inject (B1 a)       x := ? B2 a x;
yellow_inject (B2 a b)     x := ? B3 a b x;
yellow_inject (B3 a b c)   x := ? B4 a b c x;
yellow_inject (B4 a b c d) x := ? B5 a b c d x.

(* Pops off a yellow buffer. *)
Equations yellow_pop {A lvl size} (b : buffer A lvl size yellow) :
  { '(x, b') : prodN A lvl * buffer A lvl (Nat.pred size) red |
    buffer_seq b = prodN_seq x ++ buffer_seq b' } :=
yellow_pop (B1 a)       := ? (a, B0);
yellow_pop (B2 a b)     := ? (a, B1 b);
yellow_pop (B3 a b c)   := ? (a, B2 b c);
yellow_pop (B4 a b c d) := ? (a, B3 b c d).

(* Ejects off a yellow buffer. *)
Equations yellow_eject {A lvl size} (b : buffer A lvl size yellow) :
  { '(b', x) : buffer A lvl (Nat.pred size) red * prodN A lvl |
    buffer_seq b = buffer_seq b' ++ prodN_seq x } :=
yellow_eject (B1 a)       := ? (B0, a);
yellow_eject (B2 a b)     := ? (B1 a, b);
yellow_eject (B3 a b c)   := ? (B2 a b, c);
yellow_eject (B4 a b c d) := ? (B3 a b c, d).

(* Pushes on a buffer, and returns a green chain. *)
Equations buffer_push {A lvl size C}
  (x : prodN A lvl) (b : buffer A lvl size C) :
  { c : chain A lvl (S size) green |
    chain_seq c = prodN_seq x ++ buffer_seq b } :=
buffer_push x B0 := ? Ending (B1 x);
buffer_push x (B1 a) := ? Ending (B2 x a);
buffer_push x (B2 a b) := ? Ending (B3 x a b);
buffer_push x (B3 a b c) := ? Ending (B4 x a b c);
buffer_push x (B4 a b c d) := ? Ending (B5 x a b c d);
buffer_push x (B5 a b c d e) :=
  ? Chain G (Packet (B3 x a b) Hole (B3 c d e)) (Ending B0).

(* Injects on a buffer, and returns a green chain. *)
Equations buffer_inject {A lvl size C}
  (b : buffer A lvl size C) (x : prodN A lvl) :
  { c : chain A lvl (S size) green |
    chain_seq c = buffer_seq b ++ prodN_seq x } :=
buffer_inject B0 x := ? Ending (B1 x);
buffer_inject (B1 a) x := ? Ending (B2 a x);
buffer_inject (B2 a b) x := ? Ending (B3 a b x);
buffer_inject (B3 a b c) x := ? Ending (B4 a b c x);
buffer_inject (B4 a b c d) x := ? Ending (B5 a b c d x);
buffer_inject (B5 a b c d e) x :=
  ? Chain G (Packet (B3 a b c) Hole (B3 d e x)) (Ending B0).

(* Pops off a buffer, and returns an option.
   Extra properties about the size of the buffer are returned. *)
Equations buffer_pop {A lvl size C} (b : buffer A lvl size C) :
  { o : option (prodN A lvl * buffer A lvl (Nat.pred size) red) |
    match o with
    | None => buffer_seq b = [] /\ size = 0
    | Some (x, b') => buffer_seq b = prodN_seq x ++ buffer_seq b' /\
                      exists s, size = S s
    end } :=
buffer_pop B0 := ? None;
buffer_pop (B1 a) := ? Some (a, B0);
buffer_pop (B2 a b) := ? Some (a, B1 b);
buffer_pop (B3 a b c) := ? Some (a, B2 b c);
buffer_pop (B4 a b c d) := ? Some (a, B3 b c d);
buffer_pop (B5 a b c d e) := ? Some (a, B4 b c d e).

(* Ejects off a buffer, and returns an option.
   Extra properties about the size of the buffer are returned. *)
Equations buffer_eject {A lvl size C} (b : buffer A lvl size C) :
  { o : option (buffer A lvl (Nat.pred size) red * prodN A lvl) |
    match o with
    | None => buffer_seq b = [] /\ size = 0
    | Some (b', x) => buffer_seq b = buffer_seq b' ++ prodN_seq x /\
                      exists s, size = S s
    end } :=
buffer_eject B0 := ? None;
buffer_eject (B1 a) := ? Some (B0, a);
buffer_eject (B2 a b) := ? Some (B1 a, b);
buffer_eject (B3 a b c) := ? Some (B2 a b, c);
buffer_eject (B4 a b c d) := ? Some (B3 a b c, d);
buffer_eject (B5 a b c d e) := ? Some (B4 a b c d, e).

(* Pushes then ejects. *)
Equations prefix_rot {A lvl size C} (x : prodN A lvl) (b : buffer A lvl size C) :
  { '(b', y) : buffer A lvl size C * prodN A lvl |
    prodN_seq x ++ buffer_seq b = buffer_seq b' ++ prodN_seq y } :=
prefix_rot x B0 := ? (B0, x);
prefix_rot x (B1 a) := ? (B1 x, a);
prefix_rot x (B2 a b) := ? (B2 x a, b);
prefix_rot x (B3 a b c) := ? (B3 x a b, c);
prefix_rot x (B4 a b c d) := ? (B4 x a b c, d);
prefix_rot x (B5 a b c d e) := ? (B5 x a b c d, e).

(* Injects then pops. *)
Equations suffix_rot {A lvl size C} (b : buffer A lvl size C) (y : prodN A lvl) :
  { '(x, b') : prodN A lvl * buffer A lvl size C |
    buffer_seq b ++ prodN_seq y = prodN_seq x ++ buffer_seq b' } :=
suffix_rot B0 x := ? (x, B0);
suffix_rot (B1 a) x := ? (a, B1 x);
suffix_rot (B2 a b) x := ? (a, B2 b x);
suffix_rot (B3 a b c) x := ? (a, B3 b c x);
suffix_rot (B4 a b c d) x := ? (a, B4 b c d x);
suffix_rot (B5 a b c d e) x := ? (a, B5 b c d e x).

(* Merges an option and a pair to create a green buffer. *)
Equations prefix23 {A lvl s} (o : optionN A lvl s) (p: prodN A (S lvl)) :
  { b : buffer A lvl (2 + s) green |
    buffer_seq b = optionN_seq o ++ prodN_seq p } :=
prefix23  NoneN    (prodS b c) := ? B2 b c;
prefix23 (SomeN a) (prodS b c) := ? B3 a b c.

(* Merges a pair and an option to create a green buffer. *)
Equations suffix23 {A lvl s} (p : prodN A (S lvl)) (o : optionN A lvl s) :
  { b : buffer A lvl (2 + s) green |
    buffer_seq b = prodN_seq p ++ optionN_seq o } :=
suffix23 (prodS a b)  NoneN    := ? B2 a b;
suffix23 (prodS a b) (SomeN c) := ? B3 a b c.

(* Merges an element and an option to create a yellow buffer. *)
Equations suffix12 {A lvl s} (x : prodN A lvl) (o : optionN A lvl s) :
  { b : buffer A lvl (S s) yellow |
    buffer_seq b = prodN_seq x ++ optionN_seq o } :=
suffix12 x  NoneN    := ? B1 x;
suffix12 x (SomeN y) := ? B2 x y.

(* Returns the decomposed version of a buffer. Here, it is a prefix
   decomposition: when the buffer has 4 or 5 elements, those at the end are
   set appart. *)
Equations prefix_decompose {A lvl size C} (b : buffer A lvl size C) :
  { dec : decompose A lvl size |
    buffer_seq b = decompose_main_seq dec ++ decompose_rest_seq dec } :=
prefix_decompose B0 := ? Underflow NoneN;
prefix_decompose (B1 a) := ? Underflow (SomeN a);
prefix_decompose (B2 a b) := ? Ok (B2 a b);
prefix_decompose (B3 a b c) := ? Ok (B3 a b c);
prefix_decompose (B4 a b c d) := ? Overflow (B2 a b) (prodS c d);
prefix_decompose (B5 a b c d e) := ? Overflow (B3 a b c) (prodS d e).

(* Returns the decomposed version of a buffer. Here, it is a suffix
   decomposition: when the buffer has 4 or 5 elements, those at the start are
   set appart. *)
Equations suffix_decompose {A lvl size C} (b : buffer A lvl size C) :
  { dec : decompose A lvl size |
    buffer_seq b = decompose_rest_seq dec ++ decompose_main_seq dec } :=
suffix_decompose B0 := ? Underflow NoneN;
suffix_decompose (B1 a) := ? Underflow (SomeN a);
suffix_decompose (B2 a b) := ? Ok (B2 a b);
suffix_decompose (B3 a b c) := ? Ok (B3 a b c);
suffix_decompose (B4 a b c d) := ? Overflow (B2 c d) (prodS a b);
suffix_decompose (B5 a b c d e) := ? Overflow (B3 c d e) (prodS a b).

(* Returns the sandwiched version of a buffer. *)
Equations buffer_unsandwich {A lvl size C} (b : buffer A lvl size C) :
  { sw : sandwich A lvl size | buffer_seq b = sandwich_seq sw } :=
buffer_unsandwich B0 := ? Alone NoneN;
buffer_unsandwich (B1 a) := ? Alone (SomeN a);
buffer_unsandwich (B2 a b) := ? Sandwich a B0 b;
buffer_unsandwich (B3 a b c) := ? Sandwich a (B1 b) c;
buffer_unsandwich (B4 a b c d) := ? Sandwich a (B2 b c) d;
buffer_unsandwich (B5 a b c d e) := ? Sandwich a (B3 b c d) e.

(* In the following, n-buffer denote a buffer of level n. *)

(* Converts a n-buffer to a (n+1)-buffer. If the buffer has an odd number of
   elements, the first is returned via an option. *)
Equations buffer_halve {A lvl size C} (b : buffer A lvl size C) :
  { '(o, b') : optionN A lvl (size mod 2) * buffer A (S lvl) (size / 2) red |
    buffer_seq b = optionN_seq o ++ buffer_seq b' } :=
buffer_halve B0 := ? (NoneN, B0);
buffer_halve (B1 a) := ? (SomeN a, B0);
buffer_halve (B2 a b) := ? (NoneN, B1 (prodS a b));
buffer_halve (B3 a b c) := ? (SomeN a, B1 (prodS b c));
buffer_halve (B4 a b c d) := ? (NoneN, B2 (prodS a b) (prodS c d));
buffer_halve (B5 a b c d e) := ? (SomeN a, B2 (prodS b c) (prodS d e)).

(* Makes a non-red buffer yellow. *)
Equations to_yellow {A lvl s g y} :
  buffer A lvl s (Mix g y NoRed) -> buffer A lvl s yellow :=
to_yellow (B1 a) := B1 a;
to_yellow (B2 a b) := B2 a b;
to_yellow (B3 a b c) := B3 a b c;
to_yellow (B4 a b c d) := B4 a b c d.

(* Proves the sequence of a buffer remains the same when it is made yellow. *)
Lemma to_yellow_seq [A lvl size g y] (b : buffer A lvl size (Mix g y NoRed)) :
  buffer_seq (to_yellow b) = buffer_seq b.
Proof.
  dependent destruction b; reflexivity.
Qed.

(* Makes a buffer of any color red. *)
Equations to_red {A lvl s C} : buffer A lvl s C -> buffer A lvl s red :=
to_red B0 := B0;
to_red (B1 a) := B1 a;
to_red (B2 a b) := B2 a b;
to_red (B3 a b c) := B3 a b c;
to_red (B4 a b c d) := B4 a b c d;
to_red (B5 a b c d e) := B5 a b c d e.

(* Proves the sequence of a buffer remains the same when it is made red. *)
Lemma to_red_seq [A lvl size C] (b : buffer A lvl size C) :
  buffer_seq (to_red b) = buffer_seq b.
Proof.
  destruct b; reflexivity.
Qed.

(* The last two lemmas are added to the hint database. *)
#[export] Hint Rewrite to_yellow_seq : rlist.
#[export] Hint Rewrite to_red_seq : rlist.

(* Takes a n-buffer of any color and a green (n+1)-buffer, rearranges elements
   contained in them, and returns a green buffer and a yellow buffer of pairs.
   The order of elements is preserved. *)
Equations green_prefix_concat {A lvl size1 size2 C}
  (b1 : buffer A lvl size1 C)
  (b2 : buffer A (S lvl) size2 green) :
  { '(b1', b2') : buffer A lvl (2 + size1 mod 2) green *
                  buffer A (S lvl) (Nat.pred (size1 / 2 + size2)) yellow |
    buffer_seq b1 ++ buffer_seq b2 =
    buffer_seq b1' ++ buffer_seq b2' } :=
green_prefix_concat B0 b2 with green_pop b2 => {
  | ? (prodS a b, b3) := ? (B2 a b, b3) };
green_prefix_concat (B1 a) b2 with green_pop b2 => {
  | ? (prodS b c, b3) := ? (B3 a b c, b3) };
green_prefix_concat (B2 a b) b2 := ? (B2 a b, to_yellow b2);
green_prefix_concat (B3 a b c) b2 := ? (B3 a b c, to_yellow b2);
green_prefix_concat (B4 a b c d) b2 with green_push (prodS c d) b2 => {
  | ? b3 := ? (B2 a b, b3) };
green_prefix_concat (B5 a b c d e) b2 with green_push (prodS d e) b2 => {
  | ? b3 := ? (B3 a b c, b3) }.

(* Takes a green (n+1)-buffer and a n-buffer of any color, rearranges elements
   contained in them, and returns a yellow buffer of pairs and a green buffer.
   The order of elements is preserved. *)
Equations green_suffix_concat {A lvl size1 size2 C}
  (b1 : buffer A (S lvl) size1 green)
  (b2 : buffer A lvl size2 C) :
  { '(b1', b2') : buffer A (S lvl) (Nat.pred (size2 / 2 + size1)) yellow *
                  buffer A lvl (2 + size2 mod 2) green |
    buffer_seq b1 ++ buffer_seq b2 =
    buffer_seq b1' ++ buffer_seq b2' } :=
green_suffix_concat b1 B0 with green_eject b1 => {
  | ? (b3, prodS a b) := ? (b3, B2 a b) };
green_suffix_concat b1 (B1 c) with green_eject b1 => {
  | ? (b3, prodS a b) := ? (b3, B3 a b c) };
green_suffix_concat b1 (B2 a b) := ? (to_yellow b1, B2 a b);
green_suffix_concat b1 (B3 a b c) := ? (to_yellow b1, B3 a b c);
green_suffix_concat b1 (B4 a b c d) with green_inject b1 (prodS a b) => {
  | ? b3 := ? (b3, B2 c d) };
green_suffix_concat b1 (B5 a b c d e) with green_inject b1 (prodS a b) => {
  | ? b3 := ? (b3, B3 c d e) }.

(* Takes a n-buffer of any color and a yellow (n+1)-buffer, rearranges elements
   contained in them, and returns a green buffer and a buffer of pairs of any
   color.
   The order of elements is preserved. *)
Equations yellow_prefix_concat {A lvl size1 size2 C}
  (b1 : buffer A lvl size1 C)
  (b2 : buffer A (S lvl) size2 yellow) :
  { '(b1', b2') : buffer A lvl (2 + size1 mod 2) green *
                  buffer A (S lvl) (Nat.pred (size1 / 2 + size2)) red |
    buffer_seq b1 ++ buffer_seq b2 =
    buffer_seq b1' ++ buffer_seq b2' } :=
yellow_prefix_concat B0 b2 with yellow_pop b2 => {
  | ? (prodS a b, b3) := ? (B2 a b, b3) };
yellow_prefix_concat (B1 a) b2 with yellow_pop b2 => {
  | ? (prodS b c, b3) := ? (B3 a b c, b3) };
yellow_prefix_concat (B2 a b) b2 := ? (B2 a b, to_red b2);
yellow_prefix_concat (B3 a b c) b2 := ? (B3 a b c, to_red b2);
yellow_prefix_concat (B4 a b c d) b2 with yellow_push (prodS c d) b2 => {
  | ? b3 := ? (B2 a b, b3) };
yellow_prefix_concat (B5 a b c d e) b2 with yellow_push (prodS d e) b2 => {
  | ? b3 := ? (B3 a b c, b3) }.

(* Takes a yellow (n+1)-buffer and a n-buffer of any color, rearranges elements
   contained in them, and returns a buffer of pairs of any color and a green
   buffer.
   The order of elements is preserved. *)
Equations yellow_suffix_concat {A lvl size1 size2 C}
  (b1 : buffer A (S lvl) size1 yellow)
  (b2 : buffer A lvl size2 C) :
  { '(b1', b2') : buffer A (S lvl) (Nat.pred (size2 / 2 + size1)) red *
                  buffer A lvl (2 + size2 mod 2) green |
    buffer_seq b1 ++ buffer_seq b2 =
    buffer_seq b1' ++ buffer_seq b2' } :=
yellow_suffix_concat b1 B0 with yellow_eject b1 => {
  | ? (b3, prodS a b) := ? (b3, B2 a b) };
yellow_suffix_concat b1 (B1 c) with yellow_eject b1 => {
  | ? (b3, prodS a b) := ? (b3, B3 a b c) };
yellow_suffix_concat b1 (B2 a b) := ? (to_red b1, B2 a b);
yellow_suffix_concat b1 (B3 a b c) := ? (to_red b1, B3 a b c);
yellow_suffix_concat b1 (B4 a b c d) with yellow_inject b1 (prodS a b) => {
  | ? b3 := ? (b3, B2 c d) };
yellow_suffix_concat b1 (B5 a b c d e) with yellow_inject b1 (prodS a b) => {
  | ? b3 := ? (b3, B3 c d e) }.

(* Pushes an option on a buffer, and returns a green chain. *)
Equations green_opt_push {A lvl sizeb sizeo}
    (o : optionN A lvl sizeo) (b : buffer A lvl sizeb green) :
  { c : chain A lvl (sizeo + sizeb) green |
    chain_seq c = optionN_seq o ++ buffer_seq b } :=
green_opt_push NoneN b := ? Ending (to_red b);
green_opt_push (SomeN a) (B2 b c) := ? Ending (B3 a b c);
green_opt_push (SomeN a) (B3 b c d) := ? Ending (B4 a b c d).

(* Injects an option on a buffer, and returns a green chain. *)
Equations green_opt_inject {A lvl sizeb sizeo}
    (b : buffer A lvl sizeb green) (o : optionN A lvl sizeo) :
  { c : chain A lvl (sizeo + sizeb) green |
    chain_seq c = buffer_seq b ++ optionN_seq o } :=
green_opt_inject b NoneN := ? Ending (to_red b);
green_opt_inject (B2 a b) (SomeN c) := ? Ending (B3 a b c);
green_opt_inject (B3 a b c) (SomeN d) := ? Ending (B4 a b c d).

(* Creates a green chain from 3 options. *)
Equations chain_of_opt3 {A lvl s1 s2 s3}
  (o1 : optionN A lvl s1)
  (o2 : optionN A (S lvl) s2)
  (o3 : optionN A lvl s3) :
  { c : chain A lvl (s1 + s2 + s2 + s3) green |
    chain_seq c = optionN_seq o1 ++ optionN_seq o2 ++ optionN_seq o3 } :=
chain_of_opt3 NoneN NoneN NoneN := ? Ending B0;
chain_of_opt3 (SomeN a) NoneN NoneN := ? Ending (B1 a);
chain_of_opt3 NoneN NoneN (SomeN a) := ? Ending (B1 a);
chain_of_opt3 (SomeN a) NoneN (SomeN b) := ? Ending (B2 a b);
chain_of_opt3 NoneN (SomeN (prodS a b)) NoneN := ? Ending (B2 a b);
chain_of_opt3 (SomeN a) (SomeN (prodS b c)) NoneN := ? Ending (B3 a b c);
chain_of_opt3 NoneN (SomeN (prodS a b)) (SomeN c) := ? Ending (B3 a b c);
chain_of_opt3 (SomeN a) (SomeN (prodS b c)) (SomeN d) := ? Ending (B4 a b c d).

(* Provided the equality of two natural numbers, translates a chain of size the first into a chain of size the second. *)
Equations translate {A lvl size1 size2 C} (c : chain A lvl size1 C) :
  size1 = size2 -> { c' : chain A lvl size2 C | chain_seq c' = chain_seq c } :=
translate c eq_refl := ? c.

(* Proves that all natural numbers [n] can be writen as [n mod 2] plus 2 times
   [n / 2]. *)
Lemma dec_by_2 : forall (n : nat), n mod 2 + n / 2 + n / 2 = n.
Proof.
  intro n.
  assert (n / 2 + n / 2 = 2 * (n / 2)) as H. { lia. }
  rewrite <-Nat.add_assoc, H, Nat.add_comm.
  symmetry.
  apply Nat.div_mod.
  lia.
Qed.

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations on natural numbers generated by [Equations]. *)
#[export] Hint Rewrite plus_O_n : rnat.
#[export] Hint Rewrite <-plus_n_O : rnat.
#[export] Hint Rewrite plus_Sn_m : rnat.
#[export] Hint Rewrite <-plus_n_Sm : rnat.
#[export] Hint Rewrite dec_by_2 : rnat.

(* Takes a prefix buffer, a child buffer, and a suffix buffer, and rearranges
   all elements contained in these buffers to form a green chain.
   The order of elements is preserved. *)
Equations make_small {A lvl size1 size2 size3 C1 C2 C3}
  (b1 : buffer A lvl size1 C1)
  (b2 : buffer A (S lvl) size2 C2)
  (b3 : buffer A lvl size3 C3) :
  { c : chain A lvl (size1 + size2 + size2 + size3) green |
    chain_seq c = buffer_seq b1 ++ buffer_seq b2 ++ buffer_seq b3 } :=
make_small b1 b2 b3 with prefix_decompose b1, suffix_decompose b3 => {
  | ? Underflow p1, ? Underflow s1 with buffer_unsandwich b2 => {
    | ? Alone opt with chain_of_opt3 p1 opt s1 => { | ? c := ? c };
    | ? Sandwich ab rest cd with prefix23 p1 ab, suffix23 cd s1 => {
      | ? p, ? s with translate (Chain G (Packet p Hole s) (Ending rest)) _ => {
        | ? c := ? c } } };
  | ? Underflow p1, ? Ok s1 with buffer_pop b2 => {
    | ? None with green_opt_push p1 s1 => {
      | ? c with translate c _ => { | ? c' := ? c' } };
    | ? Some (cd, rest) with prefix23 p1 cd => {
      | ? p with translate (Chain G (Packet p Hole s1) (Ending rest)) _ => {
        | ? c := ? c } } };
  | ? Underflow p1, ? Overflow s1 ab with suffix_rot b2 ab => {
    | ? (cd, center) with prefix23 p1 cd => { | ? p
      with
        translate (Chain G (Packet p Hole s1) (Ending (to_red center))) _ => {
          | ? c := ? c } } };
  | ? Ok p1, ? Underflow s1 with buffer_eject b2 => {
    | ? None with green_opt_inject p1 s1 => {
      | ? c with translate c _ => { | ? c' := ? c' } };
    | ? Some (rest, ab) with suffix23 ab s1 => {
      | ? s with translate (Chain G (Packet p1 Hole s) (Ending rest)) _ => {
        | ? c := ? c } } };
  | ? Ok p1, ? Ok s1 := ? Chain G (Packet p1 Hole s1) (Ending (to_red b2));
  | ? Ok p1, ? Overflow s1 ab with buffer_inject b2 ab => {
    | ? c2 with translate (Chain G (Packet p1 Hole s1) c2) _ => { | ? c := ? c } };
  | ? Overflow p1 cd, ? Underflow s1 with prefix_rot cd b2 => {
    | ? (center, ab) with suffix23 ab s1 => { | ? s
      with
        translate (Chain G (Packet p1 Hole s) (Ending (to_red center))) _ => {
          | ? c := ? c } } };
  | ? Overflow p1 cd, ? Ok s1 with buffer_push cd b2 => {
    | ? c2
      with translate (Chain G (Packet p1 Hole s1) c2) _ => { | ? c := ? c } };
  | ? Overflow p1 cd, ? Overflow s1 ab with buffer_halve b2 => {
    | ? (x, rest) with suffix12 cd x => {
      | ? p :=
        let c := Chain G (Packet p1 (Packet p Hole (B1 ab)) s1) (Ending rest) in
        let '? c' := translate c _ in ? c' } } }.
Next Obligation.
  cbn. intros * Hb1 * Hp * Hb2 Hb3 * Hc.
  rewrite Hc, to_red_seq, Hp.
  aac_rewrite <-Hb2.
  hauto db:rlist.
Qed.
Next Obligation.
  cbn. intros * Hb1 * Hb2 * Hs Hb3 * Hc.
  rewrite Hc, to_red_seq, Hs.
  aac_rewrite <-Hb2.
  hauto db:rlist.
Qed.
Next Obligation.
  intros.
  autorewrite with rnat.
  reflexivity.
Qed.

(* Proves that if a natural number represents the size of a non-red buffer,
   then it's the successor of another natural number. *)
Lemma yellow_size {A lvl size g y} (b : buffer A lvl size (Mix g y NoRed)) :
  exists p, size = S p.
Proof.
  dependent destruction b.
  - exists 0. reflexivity.
  - exists 1. reflexivity.
  - exists 2. reflexivity.
  - exists 3. reflexivity.
Qed.

(* A tactic to facilitate the use of the previous lemma. *)
Tactic Notation "yellow_size" hyp(b) "as" ident(H) :=
  pose (yellow_size b) as H;
  destruct H as [? H];
  rewrite H.

(* Makes a red chain green. *)
Equations green_of_red {A lvl size} (c : chain A lvl size red) :
  { c' : chain A lvl size green | chain_seq c' = chain_seq c } :=
green_of_red (Chain R (Packet p1 Hole s1) (Ending b))
  with make_small p1 b s1 => { | ? c' := ? c' };
green_of_red (Chain R (Packet p1 Hole s1) (Chain G (Packet p2 child s2) c))
  with green_prefix_concat p1 p2, green_suffix_concat s2 s1 => {
  | ? (p1', p2'), ? (s2', s1') :=
    let c := Chain G (Packet p1' (Packet p2' child s2') s1') c in
    let '? c' := translate c _ in ? c' };
green_of_red (Chain R (Packet p1 (Packet p2 child s2) s1) c)
  with yellow_prefix_concat p1 (to_yellow p2),
       yellow_suffix_concat (to_yellow s2) s1 => {
  | ? (p1', p2'), ? (s2', s1') :=
  let c := Chain G (Packet p1' Hole s1') (Chain R (Packet p2' child s2') c) in
  let '? c' := translate c _ in ? c' }.
Next Obligation.
  intros * Hp * Hs * c.
  yellow_size p2 as Hpsize0.
  yellow_size s2 as Hssize0.
  pose (dec_by_2 psize) as Hpsize. rewrite <-Hpsize at 4.
  pose (dec_by_2 ssize) as Hssize. rewrite <-Hssize at 4.
  hauto.
Qed.
Next Obligation.
  cbn. intros * Hp * Hs * Hc'.
  rewrite Hc'.
  aac_rewrite <-Hp.
  hauto db:rlist.
Qed.
Next Obligation.
  intros * Hp * Hs * c.
  yellow_size p2 as Hpsize0.
  yellow_size s2 as Hssize0.
  pose (dec_by_2 psize) as Hpsize. rewrite <-Hpsize at 4.
  pose (dec_by_2 ssize) as Hssize. rewrite <-Hssize at 4.
  hauto.
Qed.
Next Obligation.
  cbn. intros * Hp * Hs * Hc'.
  rewrite Hc'.
  aac_rewrite <-Hp.
  hauto db:rlist.
Qed.

(* Makes a green or red chain green. *)
Equations ensure_green {A lvl size g r}
  (cd : chain A lvl size (Mix g NoYellow r)) :
  { cd' : chain A lvl size green | chain_seq cd' = chain_seq cd } :=
ensure_green (Ending b)      := ? Ending b;
ensure_green (Chain G pkt c) := ? Chain G pkt c;
ensure_green (Chain R pkt c) := green_of_red (Chain R pkt c).

(* Takes a prefix non-red buffer, a child packet and a suffix non-red buffer,
   and a following green or red chain, and makes a deque out of them. *)
Equations make_yellow {A lvl ps pkts ss cs size} {gp yp ypkt gs ys gc rc}
  (p : buffer A 0 ps (Mix gp yp NoRed))
  (pkt : packet A 1 lvl pkts cs (Mix NoGreen ypkt NoRed))
  (s : buffer A 0 ss (Mix gs ys NoRed))
  (c : chain A lvl cs (Mix gc NoYellow rc)) :
  size = ps + pkts + pkts + ss ->
  { d : deque A size | deque_seq d =
    buffer_seq p ++ packet_seq pkt (chain_seq c) ++ buffer_seq s } :=
make_yellow p1 child s1 c eq_refl with ensure_green c => {
  | ? c' => ? T (Chain Y (Packet (to_yellow p1) child (to_yellow s1)) c') }.

(* Takes a prefix buffer of any color, a child packet and a suffix buffer of
   any color, and a following green chain, and makes a deque out of them. *)
Equations make_red {A lvl ps pkts ss cs size} {Cp ypkt Cs}
  (p : buffer A 0 ps Cp)
  (pkt : packet A 1 lvl pkts cs (Mix NoGreen ypkt NoRed))
  (s : buffer A 0 ss Cs)
  (c : chain A lvl cs green) :
  size = ps + pkts + pkts + ss ->
  { d : deque A size | deque_seq d =
    buffer_seq p ++ packet_seq pkt (chain_seq c) ++ buffer_seq s } :=
make_red p1 child s1 c eq_refl
  with green_of_red (Chain R (Packet (to_red p1) child (to_red s1)) c) => {
    | ? c' => ? T c' }.

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(* The empty deque. *)
Equations empty {A} : { d : deque A 0 | deque_seq d = [] } :=
empty := ? T (Ending B0).

(* Pushes on a deque. *)
Equations push {A : Type} {size : nat} (x : A) (d : deque A size) :
  { d' : deque A (S size) | deque_seq d' = [x] ++ deque_seq d } :=
push x (T (Ending b)) with buffer_push (prodZ x) b => { | ? b' => ? T b' };
push x (T (Chain G (Packet p1 child s1) c))
  with green_push (prodZ x) p1 => {
    | ? p1' with make_yellow p1' child s1 c eq_refl => { | ? d' => ? d' } };
push x (T (Chain Y (Packet p1 child s1) c))
  with yellow_push (prodZ x) p1 => {
    | ? p1' with make_red p1' child s1 c eq_refl => { | ? d' => ? d' } }.

(* Injects on a deque. *)
Equations inject {A : Type} {size : nat} (d : deque A size) (x : A) :
  { d' : deque A (S size) | deque_seq d' = deque_seq d ++ [x] } :=
inject (T (Ending b)) x with buffer_inject b (prodZ x) => { | ? b' => ? T b' };
inject (T (Chain G (Packet p1 child s1) c)) x
  with green_inject s1 (prodZ x) => {
    | ? s1' with make_yellow p1 child s1' c _ => { | ? d' => ? d' } };
inject (T (Chain Y (Packet p1 child s1) c)) x
  with yellow_inject s1 (prodZ x) => {
    | ? s1' with make_red p1 child s1' c _ => { | ? d' => ? d' } }.

(* Pops off a deque. *)
Equations option_pop {A : Type} {size : nat} (d : deque A size) :
  { o : option (A * deque A (Nat.pred size)) |
    match o with
    | None => deque_seq d = [] /\ size = 0
    | Some (x, d') => deque_seq d = [x] ++ deque_seq d'
    end } :=
option_pop (T (Ending b)) with buffer_pop b => {
  | ? None := ? None;
  | ? Some (prodZ x, b') := ? Some (x, T (Ending b')) };
option_pop (T (Chain G (Packet p1 child s1) c)) with green_pop p1 => {
  | ? (prodZ x, p1') with make_yellow p1' child s1 c _ => {
    | ? d' => ? Some (x, d') } };
option_pop (T (Chain Y (Packet p1 child s1) c)) with yellow_pop p1 => {
  | ? (prodZ x, p1') with make_red p1' child s1 c _ => {
    | ? d' => ? Some (x, d') } }.
Next Obligation.
  intros * Hp1 child s1 c.
  yellow_size p1 as H.
  hauto.
Qed.
Next Obligation.
  intros * Hp1 child s1 c.
  yellow_size p1 as H.
  hauto.
Qed.

(* Pops off a non-empty deque. *)
Equations pop {A : Type} {size : nat} (d : deque A (S size)) :
  { '(x, d') : A * deque A size | deque_seq d = [x] ++ deque_seq d' } :=
pop d with option_pop d => {
  | ? None := _;
  | ? Some (x, d') := ? (x, d') }.

(* Ejects off a deque. *)
Equations option_eject {A : Type} {size : nat} (d : deque A size) :
  { o : option (deque A (Nat.pred size) * A) |
    match o with
    | None => deque_seq d = [] /\ size = 0
    | Some (d', x) => deque_seq d = deque_seq d' ++ [x]
    end } :=
option_eject (T (Ending b)) with buffer_eject b => {
  | ? None := ? None;
  | ? Some (b', prodZ x) := ? Some (T (Ending b'), x) };
option_eject (T (Chain G (Packet p1 child s1) c)) with green_eject s1 => {
  | ? (s1',prodZ  x) with make_yellow p1 child s1' c _ => {
    | ? d' => ? Some (d', x) } };
option_eject (T (Chain Y (Packet p1 child s1) c)) with yellow_eject s1 => {
  | ? (s1', prodZ x) with make_red p1 child s1' c _ => {
    | ? d' => ? Some (d', x) } }.
Next Obligation.
  intros * p1 child * Hs1 c.
  yellow_size s1 as H.
  hauto.
Qed.
Next Obligation.
  intros * p1 child * Hs1 c.
  yellow_size s1 as H.
  hauto.
Qed.

(* Ejects off a non-empty deque. *)
Equations eject {A : Type} {size : nat} (d : deque A (S size)) :
  { '(d', x) : deque A size * A | deque_seq d = deque_seq d' ++ [x] } :=
eject d with option_eject d => {
  | ? None := _;
  | ? Some (d', x) := ? (d', x) }.