From Coq Require Import Program List.
Import ListNotations.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.
From Hammer Require Import Tactics.
From AAC_tactics Require Import AAC.
From AAC_tactics Require Import Instances.
Import Instances.Lists.

From Color Require Import color.
Import GYR.

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for buffers. *)
Inductive buffer : Type -> color -> Type :=
  | B0 {A}       :                          buffer A red
  | B1 {A y r}   : A                     -> buffer A (Mix NoGreen y r)
  | B2 {A g y r} : A -> A                -> buffer A (Mix g y r)
  | B3 {A g y r} : A -> A -> A           -> buffer A (Mix g y r)
  | B4 {A y r}   : A -> A -> A -> A      -> buffer A (Mix NoGreen y r)
  | B5 {A}       : A -> A -> A -> A -> A -> buffer A red.

(* A type for packets. *)
Inductive packet : Type -> Type -> color -> Type :=
  | Hole {A} : packet A A uncolored
  | Packet {A B C y} : buffer A C ->
                       packet (A * A) B (Mix NoGreen y NoRed) ->
                       buffer A C ->
                       packet A B C.

(* A type for the regularity relation. *)
Inductive regularity : color -> color -> Type :=
  | G {g r} : regularity green  (Mix g NoYellow r)
  | Y       : regularity yellow green
  | R       : regularity red    green.

(* A type for chains. *)
Inductive chain : Type -> color -> Type :=
  | Ending {A} : buffer A red -> chain A green
  | Chain {A B C1 C2} :
      regularity C1 C2 -> packet A B C1 -> chain B C2 -> chain A C1.

(* A type decomposing buffers according to their number of elements.
   Buffers with 0 or 1 element are decomposed into [Underflow];
   buffers with 2 or 3 elements are decomposed into [Ok];
   buffers with 4 or 5 elements are decomposed into [Overflow]. *)
Inductive decompose : Type -> Type :=
  | Underflow {A} : option A -> decompose A
  | Ok {A} : buffer A green -> decompose A
  | Overflow {A} : buffer A green -> A * A -> decompose A.

(* A type decomposing a buffer into its first element, a central buffer, and
   its last element. If such a decomposition is not possible, an option
   representing the buffer is returned with [Alone]. *)
Inductive sandwich : Type -> Type :=
  | Alone {A} : option A -> sandwich A
  | Sandwich {A} : A -> buffer A red -> A -> sandwich A.

(* A type for deques. *)
Inductive deque : Type -> Type :=
  | T {A g y} : chain A (Mix g y NoRed) -> deque A.

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* Model functions are transparent. *)
Set Equations Transparent.

(* The [app] function and the singleton list are made opaque. *)
Opaque app.
Definition singleton {A : Type} (x : A) : list A := [x].
Opaque singleton.

(* Returns the sequence associated to a pair. *)
Equations pair_seq {A} : A * A -> list A :=
pair_seq (a, b) := [a] ++ [b].

(* Returns the sequence associated to an option. *)
Equations option_seq {A} : option A -> list A :=
option_seq None := [];
option_seq (Some x) := [x].

(* Returns the sequence associated to a buffer. *)
Equations buffer_seq {A C} : buffer A C -> list A :=
buffer_seq B0 := [];
buffer_seq (B1 a) := [a];
buffer_seq (B2 a b) := [a] ++ [b];
buffer_seq (B3 a b c) := [a] ++ [b] ++ [c];
buffer_seq (B4 a b c d) := [a] ++ [b] ++ [c] ++ [d];
buffer_seq (B5 a b c d e) := [a] ++ [b] ++ [c] ++ [d] ++ [e].

(* Transforms a list of pairs into a list of basic elements, while preserving
   the order of elements. *)
Equations flattenp {A} : list (A * A) -> list A :=
flattenp [] := [];
flattenp (p :: l) := pair_seq p ++ flattenp l.

(* Proves that [flattenp] is distributive over [app]. *)
Lemma flattenp_app [A] (l1 l2 : list (A * A)) :
  flattenp (l1 ++ l2) = flattenp l1 ++ flattenp l2.
Proof.
  revert l2. induction l1; intro.
  - hauto.
  - rewrite <-app_comm_cons.
    hauto.
Qed.

(* Returns the sequence associated to a packet, provided the sequence
   associated to its hole. *)
Equations packet_seq {A B C} : packet A B C -> list B -> list A :=
packet_seq Hole l := l;
packet_seq (Packet p pkt s) l :=
  buffer_seq p ++ flattenp (packet_seq pkt l) ++ buffer_seq s.

(* Returns the sequence associated to a chain. *)
Equations chain_seq {A C} : chain A C -> list A :=
chain_seq (Ending b) := buffer_seq b;
chain_seq (Chain _ pkt c) := packet_seq pkt (chain_seq c).

(* Returns the first 4 elements of the sequence associated to a decomposed
   buffer. *)
Equations decompose_main_seq {A : Type} : decompose A -> list A :=
decompose_main_seq (Underflow o) := option_seq o;
decompose_main_seq (Ok b) := buffer_seq b;
decompose_main_seq (Overflow b _) := buffer_seq b.

(* Returns the sequence associated to a decomposed buffer from the 5th element
   to the end  *)
Equations decompose_rest_seq {A : Type} : decompose A -> list A :=
decompose_rest_seq (Underflow _) := [];
decompose_rest_seq (Ok _) := [];
decompose_rest_seq (Overflow _ (x, y)) := [x] ++ [y].

(* Returns the sequence associated to a sandwiched buffer. *)
Equations sandwich_seq {A : Type} : sandwich A -> list A :=
sandwich_seq (Alone None) := [];
sandwich_seq (Alone (Some x)) := [x];
sandwich_seq (Sandwich x b y) := [x] ++ buffer_seq b ++ [y].

(* Returns the sequence associated to a deque. *)
Equations deque_seq {A} : deque A -> list A :=
deque_seq (T c) := chain_seq c.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Notation for dependent types hiding the property on [x]. *)
Notation "? x" := (@exist _ _ x _) (at level 100).

(* Pushes on a green buffer. *)
Equations green_push {A : Type} (x : A) (b : buffer A green) :
  { b' : buffer A yellow | buffer_seq b' = [x] ++ buffer_seq b } :=
green_push x (B2 a b)   := ? B3 x a b;
green_push x (B3 a b c) := ? B4 x a b c.

(* Injects on a green buffer. *)
Equations green_inject {A : Type} (b : buffer A green) (x : A) :
  { b' : buffer A yellow | buffer_seq b' = buffer_seq b ++ [x] } :=
green_inject (B2 a b)   x := ? B3 a b x;
green_inject (B3 a b c) x := ? B4 a b c x.

(* Pops off a green buffer. *)
Equations green_pop {A : Type} (b : buffer A green) :
  { '(x, b') : A * buffer A yellow | buffer_seq b = [x] ++ buffer_seq b' } :=
green_pop (B2 a b)   := ? (a, B1 b);
green_pop (B3 a b c) := ? (a, B2 b c).

(* Ejects off a green buffer. *)
Equations green_eject {A : Type} (b : buffer A green) :
  { '(b', x) : buffer A yellow * A | buffer_seq b = buffer_seq b' ++ [x] } :=
green_eject (B2 a b)   := ? (B1 a, b);
green_eject (B3 a b c) := ? (B2 a b, c).

(* Pushes on a yellow buffer. *)
Equations yellow_push {A : Type} (x : A) (b : buffer A yellow) :
  { b' : buffer A red | buffer_seq b' = [x] ++ buffer_seq b } :=
yellow_push x (B1 a)       := ? B2 x a;
yellow_push x (B2 a b)     := ? B3 x a b;
yellow_push x (B3 a b c)   := ? B4 x a b c;
yellow_push x (B4 a b c d) := ? B5 x a b c d.

(* Injects on a yellow buffer. *)
Equations yellow_inject {A : Type} (b : buffer A yellow) (x : A) :
  { b' : buffer A red | buffer_seq b' = buffer_seq b ++ [x] } :=
yellow_inject (B1 a)       x := ? B2 a x;
yellow_inject (B2 a b)     x := ? B3 a b x;
yellow_inject (B3 a b c)   x := ? B4 a b c x;
yellow_inject (B4 a b c d) x := ? B5 a b c d x.

(* Pops off a yellow buffer. *)
Equations yellow_pop {A : Type} (b : buffer A yellow) :
  { '(x, b') : A * buffer A red | buffer_seq b = [x] ++ buffer_seq b' } :=
yellow_pop (B1 a)       := ? (a, B0);
yellow_pop (B2 a b)     := ? (a, B1 b);
yellow_pop (B3 a b c)   := ? (a, B2 b c);
yellow_pop (B4 a b c d) := ? (a, B3 b c d).

(* Ejects off a yellow buffer. *)
Equations yellow_eject {A : Type} (b : buffer A yellow) :
  { '(b', x) : buffer A red * A | buffer_seq b = buffer_seq b' ++ [x] } :=
yellow_eject (B1 a)       := ? (B0, a);
yellow_eject (B2 a b)     := ? (B1 a, b);
yellow_eject (B3 a b c)   := ? (B2 a b, c);
yellow_eject (B4 a b c d) := ? (B3 a b c, d).

(* Pushes on a buffer, and returns a green chain. *)
Equations buffer_push {A C} (x : A) (b : buffer A C) :
  { c : chain A green | chain_seq c = [x] ++ buffer_seq b } :=
buffer_push x B0 := ? Ending (B1 x);
buffer_push x (B1 a) := ? Ending (B2 x a);
buffer_push x (B2 a b) := ? Ending (B3 x a b);
buffer_push x (B3 a b c) := ? Ending (B4 x a b c);
buffer_push x (B4 a b c d) := ? Ending (B5 x a b c d);
buffer_push x (B5 a b c d e) :=
    ? Chain G (Packet (B3 x a b) Hole (B3 c d e)) (Ending B0).

(* Injects on a buffer, and returns a green chain. *)
Equations buffer_inject {A C} (b : buffer A C) (x : A) :
  { c : chain A green | chain_seq c = buffer_seq b ++ [x] } :=
buffer_inject B0 x := ? Ending (B1 x);
buffer_inject (B1 a) x := ? Ending (B2 a x);
buffer_inject (B2 a b) x := ? Ending (B3 a b x);
buffer_inject (B3 a b c) x := ? Ending (B4 a b c x);
buffer_inject (B4 a b c d) x := ? Ending (B5 a b c d x);
buffer_inject (B5 a b c d e) x :=
    ? Chain G (Packet (B3 a b c) Hole (B3 d e x)) (Ending B0).

(* Pops off a buffer, and returns an option. *)
Equations buffer_pop {A C} (b : buffer A C) :
  { o : option (A * buffer A red) |
    buffer_seq b =
    match o with
    | None => []
    | Some (x, b') => [x] ++ buffer_seq b'
    end } :=
buffer_pop B0 := ? None;
buffer_pop (B1 a) := ? Some (a, B0);
buffer_pop (B2 a b) := ? Some (a, B1 b);
buffer_pop (B3 a b c) := ? Some (a, B2 b c);
buffer_pop (B4 a b c d) := ? Some (a, B3 b c d);
buffer_pop (B5 a b c d e) := ? Some (a, B4 b c d e).

(* Ejects off a buffer, and returns an option. *)
Equations buffer_eject {A C} (b : buffer A C) :
  { o : option (buffer A red * A) |
    buffer_seq b =
    match o with
    | None => []
    | Some (b', x) => buffer_seq b' ++ [x]
    end } :=
buffer_eject B0 := ? None;
buffer_eject (B1 a) := ? Some (B0, a);
buffer_eject (B2 a b) := ? Some (B1 a, b);
buffer_eject (B3 a b c) := ? Some (B2 a b, c);
buffer_eject (B4 a b c d) := ? Some (B3 a b c, d);
buffer_eject (B5 a b c d e) := ? Some (B4 a b c d, e).

(* Pushes then ejects. *)
Equations prefix_rot {A C} (x : A) (b : buffer A C) :
  { '(b', y) : buffer A C * A | [x] ++ buffer_seq b = buffer_seq b' ++ [y] } :=
prefix_rot x B0 := ? (B0, x);
prefix_rot x (B1 a) := ? (B1 x, a);
prefix_rot x (B2 a b) := ? (B2 x a, b);
prefix_rot x (B3 a b c) := ? (B3 x a b, c);
prefix_rot x (B4 a b c d) := ? (B4 x a b c, d);
prefix_rot x (B5 a b c d e) := ? (B5 x a b c d, e).

(* Injects then pops. *)
Equations suffix_rot {A C} (b : buffer A C) (y : A) :
  { '(x, b') : A * buffer A C | buffer_seq b ++ [y] = [x] ++ buffer_seq b' } :=
suffix_rot B0 x := ? (x, B0);
suffix_rot (B1 a) x := ? (a, B1 x);
suffix_rot (B2 a b) x := ? (a, B2 b x);
suffix_rot (B3 a b c) x := ? (a, B3 b c x);
suffix_rot (B4 a b c d) x := ? (a, B4 b c d x);
suffix_rot (B5 a b c d e) x := ? (a, B5 b c d e x).

(* Merges an option and a pair to create a green buffer. *)
Equations prefix23 {A} (o : option A) (p: A * A) :
  { b : buffer A green | buffer_seq b = option_seq o ++ pair_seq p } :=
prefix23  None    (b, c) := ? B2 b c;
prefix23 (Some a) (b, c) := ? B3 a b c.

(* Merges a pair and an option to create a green buffer. *)
Equations suffix23 {A} (p : A * A) (o : option A) :
  { b : buffer A green | buffer_seq b = pair_seq p ++ option_seq o } :=
suffix23 (a, b)  None    := ? B2 a b;
suffix23 (a, b) (Some c) := ? B3 a b c.

(* Merges an element and an option to create a yellow buffer. *)
Equations suffix12 {A} (x : A) (o : option A) :
  { b : buffer A yellow | buffer_seq b = [x] ++ option_seq o } :=
suffix12 x  None    := ? B1 x;
suffix12 x (Some y) := ? B2 x y.

(* Returns the decomposed version of a buffer. Here, it is a prefix
   decomposition: when the buffer has 4 or 5 elements, those at the end are
   set appart. *)
Equations prefix_decompose {A C} (b : buffer A C) :
  { dec : decompose A | buffer_seq b = decompose_main_seq dec ++ decompose_rest_seq dec } :=
prefix_decompose B0 := ? Underflow None;
prefix_decompose (B1 a) := ? Underflow (Some a);
prefix_decompose (B2 a b) := ? Ok (B2 a b);
prefix_decompose (B3 a b c) := ? Ok (B3 a b c);
prefix_decompose (B4 a b c d) := ? Overflow (B2 a b) (c, d);
prefix_decompose (B5 a b c d e) := ? Overflow (B3 a b c) (d, e).

(* Returns the decomposed version of a buffer. Here, it is a suffix
   decomposition: when the buffer has 4 or 5 elements, those at the start are
   set appart. *)
Equations suffix_decompose {A C} (b : buffer A C) :
  { dec : decompose A | buffer_seq b = decompose_rest_seq dec ++ decompose_main_seq dec } :=
suffix_decompose B0 := ? Underflow None;
suffix_decompose (B1 a) := ? Underflow (Some a);
suffix_decompose (B2 a b) := ? Ok (B2 a b);
suffix_decompose (B3 a b c) := ? Ok (B3 a b c);
suffix_decompose (B4 a b c d) := ? Overflow (B2 c d) (a, b);
suffix_decompose (B5 a b c d e) := ? Overflow (B3 c d e) (a, b).

(* Returns the sandwiched version of a buffer. *)
Equations buffer_unsandwich {A C} (b : buffer A C) :
  { sw : sandwich A | buffer_seq b = sandwich_seq sw } :=
buffer_unsandwich B0 := ? Alone None;
buffer_unsandwich (B1 a) := ? Alone (Some a);
buffer_unsandwich (B2 a b) := ? Sandwich a B0 b;
buffer_unsandwich (B3 a b c) := ? Sandwich a (B1 b) c;
buffer_unsandwich (B4 a b c d) := ? Sandwich a (B2 b c) d;
buffer_unsandwich (B5 a b c d e) := ? Sandwich a (B3 b c d) e.

(* Converts a buffer to a buffer of pairs. If the buffer has an odd number of
   elements, the first is returned via an option. *)
Equations buffer_halve {A C} (b : buffer A C) :
  { '(o, b') : option A * buffer (A * A) red |
    buffer_seq b = option_seq o ++ flattenp (buffer_seq b') } :=
buffer_halve B0 := ? (None, B0);
buffer_halve (B1 a) := ? (Some a, B0);
buffer_halve (B2 a b) := ? (None, B1 (a, b));
buffer_halve (B3 a b c) := ? (Some a, B1 (b, c));
buffer_halve (B4 a b c d) := ? (None, B2 (a, b) (c, d));
buffer_halve (B5 a b c d e) := ? (Some a, B2 (b, c) (d, e)).

(* Makes a non-red buffer yellow. *)
Equations to_yellow {A g y} : buffer A (Mix g y NoRed) -> buffer A yellow :=
to_yellow (B1 a) := B1 a;
to_yellow (B2 a b) := B2 a b;
to_yellow (B3 a b c) := B3 a b c;
to_yellow (B4 a b c d) := B4 a b c d.

(* Proves the sequence of a buffer remains the same when it is made yellow. *)
Lemma to_yellow_seq [A g y] (b : buffer A (Mix g y NoRed)) :
  buffer_seq (to_yellow b) = buffer_seq b.
Proof.
  dependent destruction b; reflexivity.
Qed.

(* Makes a buffer of any color red. *)
Equations to_red {A C} : buffer A C -> buffer A red :=
to_red B0 := B0;
to_red (B1 a) := B1 a;
to_red (B2 a b) := B2 a b;
to_red (B3 a b c) := B3 a b c;
to_red (B4 a b c d) := B4 a b c d;
to_red (B5 a b c d e) := B5 a b c d e.

(* Proves the sequence of a buffer remains the same when it is made red. *)
Lemma to_red_seq [A C] (b : buffer A C) :
  buffer_seq (to_red b) = buffer_seq b.
Proof.
  destruct b; reflexivity.
Qed.

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations generated by [Equations]. *)
#[export] Hint Rewrite <-app_assoc : rlist.
#[export] Hint Rewrite app_nil_r : rlist.
#[export] Hint Rewrite flattenp_app : rlist.
#[export] Hint Rewrite to_yellow_seq : rlist.
#[export] Hint Rewrite to_red_seq : rlist.

(* Setting the default tactics for obligations to be [hauto] using the [rlist]
   hint database. *)
#[local] Obligation Tactic := try (cbn; hauto db:rlist).

(* Takes a buffer of any color and a green buffer of pairs, rearranges elements
   contained in them, and returns a green buffer and a yellow buffer of pairs.
   The order of elements is preserved. *)
Equations green_prefix_concat {A C}
  (b1 : buffer A C)
  (b2 : buffer (A * A) green) :
  { '(b1', b2') : buffer A green * buffer (A * A) yellow |
    buffer_seq b1  ++ flattenp (buffer_seq b2) =
    buffer_seq b1' ++ flattenp (buffer_seq b2') } :=
green_prefix_concat b1 b2 with prefix_decompose b1 => {
  | ? Underflow opt with green_pop b2 => {
    | ? (ab, b) with prefix23 opt ab => {
      | ? prefix := ? (prefix, b) } };
  | ? Ok b := ? (b, to_yellow b2);
  | ? Overflow b ab with green_push ab b2 => {
    | ? suffix := ? (b, suffix) } }.

(* Takes a green buffer of pairs and a buffer of any color, rearranges elements
   contained in them, and returns a yellow buffer of pairs and a green buffer.
   The order of elements is preserved. *)
Equations green_suffix_concat {A C}
  (b1 : buffer (A * A) green)
  (b2 : buffer A C) :
  { '(b1', b2') : buffer (A * A) yellow * buffer A green |
    flattenp (buffer_seq b1)  ++ buffer_seq b2 =
    flattenp (buffer_seq b1') ++ buffer_seq b2' } :=
green_suffix_concat b1 b2 with suffix_decompose b2 => {
  | ? Underflow opt with green_eject b1 => {
    | ? (b, ab) with suffix23 ab opt => {
      | ? suffix := ? (b, suffix) } };
  | ? Ok b := ? (to_yellow b1, b);
  | ? Overflow b ab with green_inject b1 ab => {
    | ? prefix := ? (prefix, b) } }.

(* Takes a buffer of any color and a yellow buffer of pairs, rearranges
   elements contained in them, and returns a green buffer and a buffer of pairs
   of any color.
   The order of elements is preserved. *)
Equations yellow_prefix_concat {A B}
  (b1 : buffer A B)
  (b2 : buffer (A * A) yellow) :
  { '(b1', b2') : buffer A green * buffer (A * A) red |
    buffer_seq b1  ++ flattenp (buffer_seq b2) =
    buffer_seq b1' ++ flattenp (buffer_seq b2') } :=
yellow_prefix_concat b1 b2 with prefix_decompose b1 => {
  | ? Underflow opt with yellow_pop b2 => {
    | ? (ab, b) with prefix23 opt ab => {
      | ? prefix := ? (prefix, b) } };
  | ? Ok b := ? (b, to_red b2);
  | ? Overflow b ab with yellow_push ab b2 => {
    | ? suffix := ? (b, suffix) } }.

(* Takes a yellow buffer of pairs and a buffer of any color, rearranges
   elements contained in them, and returns a buffer of pairs of any color and a
   green buffer.
   The order of elements is preserved. *)
Equations yellow_suffix_concat {A B}
  (b1 : buffer (A * A) yellow)
  (b2 : buffer A B) :
  { '(b1', b2') : buffer (A * A) red * buffer A green |
    flattenp (buffer_seq b1) ++ buffer_seq b2 =
    flattenp (buffer_seq b1') ++ buffer_seq b2' } :=
yellow_suffix_concat b1 b2 with suffix_decompose b2 => {
  | ? Underflow opt with yellow_eject b1 => {
    | ? (b, ab) with suffix23 ab opt => {
      | ? suffix := ? (b, suffix) } };
  | ? Ok b := ? (to_red b1, b);
  | ? Overflow b ab with yellow_inject b1 ab => {
    | ? prefix := ? (prefix, b) } }.

(* Creates a green chain from 3 options. *)
Equations chain_of_opt3 {A}
  (o1 : option A) (o2 : option (A * A)) (o3 : option A) :
  { c : chain A green | chain_seq c =
      option_seq o1 ++ flattenp (option_seq o2) ++ option_seq o3 } :=
chain_of_opt3 None None None := ? Ending B0;
chain_of_opt3 (Some a) None None := ? Ending (B1 a);
chain_of_opt3 None None (Some a) := ? Ending (B1 a);
chain_of_opt3 (Some a) None (Some b) := ? Ending (B2 a b);
chain_of_opt3 None (Some (a, b)) None := ? Ending (B2 a b);
chain_of_opt3 (Some a) (Some (b, c)) None := ? Ending (B3 a b c);
chain_of_opt3 None (Some (a, b)) (Some c) := ? Ending (B3 a b c);
chain_of_opt3 (Some a) (Some (b, c)) (Some d) := ? Ending (B4 a b c d).


(* A new tactic designed for list equalities, applying [flattenp] on them, and
   simplifying them. *)
Local Ltac flattenp_eq H :=
  apply (f_equal flattenp) in H;
  repeat rewrite flattenp_app in H;
  cbn in H.

(* Takes a prefix buffer, a child buffer, and a suffix buffer, and rearranges
   all elements contained in these buffers to form a green chain.
   The order of elements is preserved. *)
Equations make_small {A C1 C2 C3}
  (b1 : buffer A C1)
  (b2 : buffer (A * A) C2)
  (b3 : buffer A C3) :
  { c : chain A green | chain_seq c =
      buffer_seq b1 ++ flattenp (buffer_seq b2) ++ buffer_seq b3 } :=
make_small b1 b2 b3 with prefix_decompose b1, suffix_decompose b3 => {
  | ? Underflow p1, ? Underflow s1 with buffer_unsandwich b2 => {
    | ? Alone opt with chain_of_opt3 p1 opt s1 => { | ? c := ? c };
    | ? Sandwich ab rest cd with prefix23 p1 ab, suffix23 cd s1 => {
      | ? p, ? s := ? Chain G (Packet p Hole s) (Ending rest) } };
  | ? Underflow p1, ? Ok s1 with buffer_pop b2 => {
    | ? None with p1 => {
      | None := ? Ending (to_red s1);
      | Some x with buffer_push x s1 => { | ? c := ? c } };
    | ? Some (cd, rest) with prefix23 p1 cd => {
      | ? p := ? Chain G (Packet p Hole s1) (Ending rest) } };
  | ? Underflow p1, ? Overflow s1 ab with suffix_rot b2 ab => {
    | ? (cd, center) with prefix23 p1 cd => {
      | ? p => ? Chain G (Packet p Hole s1) (Ending (to_red center)) } };
  | ? Ok p1, ? Underflow s1 with buffer_eject b2 => {
    | ? None with s1 => {
      | None := ? Ending (to_red p1);
      | Some x with buffer_inject p1 x => { | ? c := ? c } };
    | ? Some (rest, ab) with suffix23 ab s1 => {
      | ? s := ? Chain G (Packet p1 Hole s) (Ending rest) } };
  | ? Ok p1, ? Ok s1 := ? Chain G (Packet p1 Hole s1) (Ending (to_red b2));
  | ? Ok p1, ? Overflow s1 ab with buffer_inject b2 ab => {
    | ? c2 => ? Chain G (Packet p1 Hole s1) c2 };
  | ? Overflow p1 cd, ? Underflow s1 with prefix_rot cd b2 => {
    | ? (center, ab) with suffix23 ab s1 => {
      | ? s => ? Chain G (Packet p1 Hole s) (Ending (to_red center)) } };
  | ? Overflow p1 cd, ? Ok s1 with buffer_push cd b2 => {
    | ? c2 => ? Chain G (Packet p1 Hole s1) c2 };
  | ? Overflow p1 cd, ? Overflow s1 ab with buffer_halve b2 => {
    | ? (x, rest) with suffix12 cd x => {
      | ? p =>
        ? Chain G (Packet p1 (Packet p Hole (B1 ab)) s1) (Ending rest) } } }.
Next Obligation.
  cbn. intros * Hb1 * Hp * Hb2 Hb3 *.
  rewrite Hp, to_red_seq.
  flattenp_eq Hb2.
  aac_rewrite <-Hb2.
  hauto db:rlist.
Qed.
Next Obligation.
  cbn. intros * Hb1 * Hb2 * Hs Hb3.
  rewrite Hs, to_red_seq.
  flattenp_eq Hb2.
  aac_rewrite <-Hb2.
  hauto db:rlist.
Qed.

(* Makes a red chain green. *)
Equations green_of_red {A : Type} (c : chain A red) :
  { c' : chain A green | chain_seq c' = chain_seq c } :=
green_of_red (Chain R (Packet p1 Hole s1) (Ending b))
  with make_small p1 b s1 => { | ? c' := ? c' };
green_of_red (Chain R (Packet p1 Hole s1) (Chain G (Packet p2 child s2) c))
  with green_prefix_concat p1 p2, green_suffix_concat s2 s1 => {
  | ? (p1', p2'), ? (s2', s1') :=
    ? Chain G (Packet p1' (Packet p2' child s2') s1') c };
green_of_red (Chain R (Packet p1 (Packet p2 child s2) s1) c)
  with yellow_prefix_concat p1 (to_yellow p2),
       yellow_suffix_concat (to_yellow s2) s1 => {
  | ? (p1', p2'), ? (s2', s1') :=
    ? Chain G (Packet p1' Hole s1') (Chain R (Packet p2' child s2') c) }.
Next Obligation.
  cbn. intros * Hp * Hs *.
  autorewrite with rlist.
  aac_rewrite <-Hp.
  hauto db:rlist.
Qed.
Next Obligation.
  cbn. intros * Hp * Hs *.
  autorewrite with rlist.
  aac_rewrite <-Hp.
  hauto db:rlist.
Qed.

(* Makes a green or red chain green. *)
Equations ensure_green {A g r} (c : chain A (Mix g NoYellow r)) :
  { c' : chain A green | chain_seq c' = chain_seq c } :=
ensure_green (Ending b)      := ? Ending b;
ensure_green (Chain G pkt c) := ? Chain G pkt c;
ensure_green (Chain R pkt c) := green_of_red (Chain R pkt c).

(* Takes a prefix non-red buffer, a child packet and a suffix non-red buffer,
   and a following green or red chain, and makes a deque out of them. *)
Equations make_yellow {A B: Type} {g1 y1 y2 g3 y3 g4 r4}
  (p : buffer A (Mix g1 y1 NoRed))
  (pkt : packet (A * A) B (Mix NoGreen y2 NoRed))
  (s : buffer A (Mix g3 y3 NoRed))
  (c : chain B (Mix g4 NoYellow r4)) :
  { d : deque A | deque_seq d =
    buffer_seq p ++ flattenp (packet_seq pkt (chain_seq c)) ++ buffer_seq s } :=
make_yellow p1 child s1 c with ensure_green c => {
  | ? c' => ? T (Chain Y (Packet (to_yellow p1) child (to_yellow s1)) c') }.

(* Takes a prefix buffer of any color, a child packet and a suffix buffer of
   any color, and a following green chain, and makes a deque out of them. *)
Equations make_red {A B: Type} {C1 y2 C3}
  (p : buffer A C1)
  (pkt : packet (A * A) B (Mix NoGreen y2 NoRed))
  (s : buffer A C3)
  (c : chain B green) :
  { d : deque A | deque_seq d =
    buffer_seq p ++ flattenp (packet_seq pkt (chain_seq c)) ++ buffer_seq s } :=
make_red p1 child s1 c
  with green_of_red (Chain R (Packet (to_red p1) child (to_red s1)) c) => {
    | ? c' => ? T c' }.

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(* Pushes on a deque. *)
Equations push {A : Type} (x : A) (d : deque A) :
  { d' : deque A | deque_seq d' = [x] ++ deque_seq d } :=
push x (T (Ending b)) with buffer_push x b => { | ? b' => ? T b' };
push x (T (Chain G (Packet p1 child s1) c)) with green_push x p1 => {
  | ? p1' with make_yellow p1' child s1 c => {
    | ? d' => ? d' } };
push x (T (Chain Y (Packet p1 child s1) c)) with yellow_push x p1 => {
  | ? p1' with make_red p1' child s1 c => {
    | ? d' => ? d' } }.

(* Injects on a deque. *)
Equations inject {A : Type} (d : deque A) (x : A) :
  { d' : deque A | deque_seq d' = deque_seq d ++ [x] } :=
inject (T (Ending b)) x with buffer_inject b x => { | ? b' => ? T b' };
inject (T (Chain G (Packet p1 child s1) c)) x with green_inject s1 x => {
  | ? s1' with make_yellow p1 child s1' c => {
    | ? d' => ? d' } };
inject (T (Chain Y (Packet p1 child s1) c)) x with yellow_inject s1 x => {
  | ? s1' with make_red p1 child s1' c => {
    | ? d' => ? d' } }.

(* Pops off a deque. *)
Equations pop {A : Type} (d : deque A) :
  { o : option (A * deque A) |
    deque_seq d = match o with
                  | None => []
                  | Some (x, d') => [x] ++ deque_seq d'
                  end } :=
pop (T (Ending b)) with buffer_pop b => {
  | ? None := ? None;
  | ? Some (x, b') := ? Some (x, T (Ending b')) };
pop (T (Chain G (Packet p1 child s1) c)) with green_pop p1 => {
  | ? (x, p1') with make_yellow p1' child s1 c => {
    | ? d' => ? Some (x, d') } };
pop (T (Chain Y (Packet p1 child s1) c)) with yellow_pop p1 => {
  | ? (x, p1') with make_red p1' child s1 c => {
    | ? d' => ? Some (x, d') } }.

(* Ejects off a deque. *)
Equations eject {A : Type} (d : deque A) :
  { o : option (deque A * A) |
    deque_seq d = match o with
                  | None => []
                  | Some (d', x) => deque_seq d' ++ [x]
                  end } :=
eject (T (Ending b)) with buffer_eject b => {
  | ? None := ? None;
  | ? Some (b', x) := ? Some (T (Ending b'), x) };
eject (T (Chain G (Packet p1 child s1) c)) with green_eject s1 => {
  | ? (s1', x) with make_yellow p1 child s1' c => {
    | ? d' => ? Some (d', x) } };
eject (T (Chain Y (Packet p1 child s1) c)) with yellow_eject s1 => {
  | ? (s1', x) with make_red p1 child s1' c => {
    | ? d' => ? Some (d', x) } }.