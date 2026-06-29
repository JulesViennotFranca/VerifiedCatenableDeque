From Coq Require Import Program List.
Import ListNotations.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.
From Hammer Require Import Tactics.
From AAC_tactics Require Import AAC.
From AAC_tactics Require Import Instances.
Import Instances.Lists.
From Coq Require Import Lia.

From Deques.Color Require Import GYR.

(* +------------------------------------------------------------------------+ *)
(* |                                Colors                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Colors. *)
Inductive color := Red | Yellow | Green.

(* A color constraint accepts or rejects each color.
   It can also be thought of as a set of colors. *)

Definition color_constraint := color -> Prop.
Implicit Type cc : color_constraint.

Definition valid_c (P : color_constraint -> Prop) (c : color) : Prop :=
  forall cc, cc c -> P cc.

Definition red c := c = Red.
Definition yellow c := c = Yellow.
Definition green c := c = Green.

Definition green_or_red c := c = Green \/ c = Red.
Definition green_or_yellow c := c = Green \/ c = Yellow.

Hint Unfold red : easy.
Hint Unfold yellow : easy.
Hint Unfold green : easy.
Hint Unfold green_or_red : easy.
Hint Unfold green_or_yellow : easy.
  (* this helps [eauto with easy], which the tactic [easy] uses *)

Lemma red_is_not_green : ~ (Red = Green).
Proof. intro; congruence. Qed.

Lemma yellow_is_not_red : ~ (Yellow = Red).
Proof. intro; congruence. Qed.

Lemma red_or_yellow_is_not_green : ~ (Red = Green \/ Yellow = Green).
Proof. intros [|]; congruence. Qed.

Lemma yellow_is_not_green_or_red :
  ~ (Yellow = Green \/ Yellow = Red).
Proof. intros [|]; congruence. Qed.

Lemma red_is_not_green_or_yellow :
  green_or_yellow Red -> False.
Proof. intros [|]; congruence. Qed.

#[local] Hint Resolve
  red_is_not_green
  yellow_is_not_red
  red_or_yellow_is_not_green
  yellow_is_not_green_or_red
  red_is_not_green_or_yellow
: easy.
  (* this helps [eauto with easy], which the tactic [easy] uses *)

(* +------------------------------------------------------------------------+ *)
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A type for buffers. *)
Inductive buffer (A : Type) : Type :=
  | B0 :                          buffer A
  | B1 : A                     -> buffer A
  | B2 : A -> A                -> buffer A
  | B3 : A -> A -> A           -> buffer A
  | B4 : A -> A -> A -> A      -> buffer A
  | B5 : A -> A -> A -> A -> A -> buffer A.
Arguments B0 {A}.
Arguments B1 {A}.
Arguments B2 {A}.
Arguments B3 {A}.
Arguments B4 {A}.
Arguments B5 {A}.

(* The buffered are colored according to their number of elements. *)
Definition colored_buffer {A} (b : buffer A) (c : color) : Prop :=
  match b with
  | B0 | B5 _ _ _ _ _ => c = Red
  | B1 _ | B4 _ _ _ _ => c = Red \/ c = Yellow
  | B2 _ _ | B3 _ _ _ => True
  end.

(* All buffers can be colored red. *)
Lemma colored_buffer_red {A} (b : buffer A) : colored_buffer b Red.
Proof. destruct b; simpl; auto. Qed.

#[local] Hint Resolve colored_buffer_red : easy.

(* A type for packets. *)
Inductive packet (A : Type) : Type -> Type :=
  | Hole : packet A A
  | Packet {B} : buffer A -> packet (A * A) B -> buffer A -> packet A B.
Arguments Hole {A}.
Arguments Packet {A B}.

(* A packet has the same color as its two buffers. *)
Definition colored_packet {A B} (p : packet A B) (c : color) : Prop :=
  match p with
  | Hole => True
  | Packet bp _ bs => colored_buffer bp c /\ colored_buffer bs c
  end.

(* Well-formedness of packets.
   The first packet can have any color;
   the following packets must be yellow or empty. *)
Fixpoint wf_packet {A B} (deep : Prop) (p : packet A B) : Prop :=
  match p with
  | Hole => deep
  | Packet p pkt s =>
    ((deep /\ colored_packet (Packet p pkt s) Yellow) \/ ~deep) /\
    wf_packet True pkt
  end.

(* A type for chains. *)
Inductive chain (A : Type) : Type :=
  | Ending : buffer A -> chain A
  | Chain {B} : packet A B -> chain B -> chain A.
Arguments Ending {A}.
Arguments Chain {A B}.

(* A chain has the same color as its first packet. *)
Definition colored_chain {A} (c : chain A) cc : Prop :=
  match c with
  | Ending _ => True
  | Chain p _ => exists c, cc c /\ colored_packet p c
  end.

(* [regularity p] has type [color_constraint]. It is the color constraint
   imposed by the packet [p] on the chain that follows it. *)
Definition regularity {A B} (p : packet A B) (c : color) : Prop :=
  match p with
  | Hole => True
  | Packet _ _ _ =>
    (* A green packet must be followed with a green or red chain. *)
    (colored_packet p Green /\ green_or_red c) \/
    (* A yellow or red packet must be followed with a green chain. *)
    (~ colored_packet p Green /\ green c)
  end.

Fixpoint wf_chain {A} (c : chain A) : Prop :=
  match c with
  | Ending _ => True
  | Chain p c =>
    (* The top packet must be well-formed. *)
    wf_packet False p /\
    (* The packet [p] imposes a color constraint on the subchain [c]. *)
    colored_chain c (regularity p) /\
    (* The subchain must be well-formed. *)
    wf_chain c
  end.

(* A type decomposing buffers according to their number of elements.
   Buffers with 0 or 1 element are decomposed into [Underflow];
   buffers with 2 or 3 elements are decomposed into [Ok];
   buffers with 4 or 5 elements are decomposed into [Overflow]. *)
Inductive decompose (A : Type) : Type :=
  | Underflow : option A -> decompose A
  | Ok : forall b : buffer A, colored_buffer b Green -> decompose A
  | Overflow :
      forall b : buffer A, colored_buffer b Green -> A * A -> decompose A.
Arguments Underflow {A}.
Arguments Ok {A}.
Arguments Overflow {A}.

(* A type decomposing a buffer into its first element, a central buffer, and
   its last element. If such a decomposition is not possible, an option
   representing the buffer is returned with [Alone]. *)
Inductive sandwich (A : Type) : Type :=
  | Alone : option A -> sandwich A
  | Sandwich : A -> buffer A -> A -> sandwich A.
Arguments Alone {A}.
Arguments Sandwich {A}.

(* A type for deques. *)
Inductive deque (A : Type) : Type :=
  | T : chain A -> deque A.
Arguments T {A}.

Definition wf_deque {A} (d : deque A) : Prop :=
  match d with
  | T c => colored_chain c green_or_yellow /\ wf_chain c
  end.

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
Equations buffer_seq {A} : buffer A -> list A :=
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
Equations packet_seq {A B} : packet A B -> list B -> list A :=
packet_seq Hole l := l;
packet_seq (Packet p pkt s) l :=
  buffer_seq p ++ flattenp (packet_seq pkt l) ++ buffer_seq s.

(* Returns the sequence associated to a chain. *)
Equations chain_seq {A} : chain A -> list A :=
chain_seq (Ending b) := buffer_seq b;
chain_seq (Chain pkt c) := packet_seq pkt (chain_seq c).

(* Returns the first 4 elements of the sequence associated to a decomposed
   buffer. *)
Equations decompose_main_seq {A : Type} : decompose A -> list A :=
decompose_main_seq (Underflow o) := option_seq o;
decompose_main_seq (Ok b _) := buffer_seq b;
decompose_main_seq (Overflow b _ _) := buffer_seq b.

(* Returns the sequence associated to a decomposed buffer from the 5th element
   to the end  *)
Equations decompose_rest_seq {A : Type} : decompose A -> list A :=
decompose_rest_seq (Underflow _) := [];
decompose_rest_seq (Ok _ _) := [];
decompose_rest_seq (Overflow _ _ (x, y)) := [x] ++ [y].

(* Returns the sequence associated to a sandwiched buffer. *)
Equations sandwich_seq {A : Type} : sandwich A -> list A :=
sandwich_seq (Alone None) := [];
sandwich_seq (Alone (Some x)) := [x];
sandwich_seq (Sandwich x b y) := [x] ++ buffer_seq b ++ [y].

(* Returns the sequence associated to a deque. *)
Equations deque_seq {A} : deque A -> list A :=
deque_seq (T c) := chain_seq c.

(* +------------------------------------------------------------------------+ *)
(* |                                Tactics                                 | *)
(* +------------------------------------------------------------------------+ *)

Ltac unpack :=
  match goal with
  |                |- ~ _ => intro
  | h: _ /\ _      |- _   => destruct h
  | h: _ \/ _      |- _   => destruct h
  | h: exists x, _ |- _   => destruct h
  end.

Ltac deduce :=
  simpl in *; repeat unpack.

Ltac crunch :=
  lazymatch goal with
  | |- _ /\ _ =>
      split; crunch
  | |- _ \/ _ => try solve [left; crunch]; right; crunch
  | _ =>
      deduce;
      try tauto;
      try congruence;
      try lia;
      eauto with easy
  end.

Ltac easy :=
  solve [ crunch ].

Ltac pose_destruct p d Heq Hpos :=
  remember d as tmp eqn: Htmp;
  pose (p) as Hp; rewrite <-Htmp in Hp; simpl in Hp;
  destruct tmp; simpl in *;
  rename Htmp into Heq; rename Hp into Hpos.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

Definition falsity {A : Type} {p : Prop} (tp : p) : (p -> False) -> A.
Proof. intro Hf. exfalso. apply Hf. exact tp. Qed.

(* Pushes on a green buffer. *)
Equations green_push {A}
  (x : A) (b : buffer A) (cc : colored_buffer b Green) : buffer A :=
green_push x (B2 a b)   _  := B3 x a b;
green_push x (B3 a b c) _  := B4 x a b c;
green_push _ _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_push_yellow {A}
  (x : A) (b : buffer A) (cc : colored_buffer b Green) :
    colored_buffer (green_push x b cc) Yellow.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - reflexivity.
  - simpl. right. reflexivity.
Qed.

Lemma green_push_seq {A}
  (x : A) (b : buffer A) (cc : colored_buffer b Green) :
    buffer_seq (green_push x b cc) = [x] ++ buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Injects on a green buffer. *)
Equations green_inject {A}
  (b : buffer A) (x : A) (cc : colored_buffer b Green) : buffer A :=
green_inject (B2 a b)   x _  := B3 a b x;
green_inject (B3 a b c) x _  := B4 a b c x;
green_inject _          _ cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_inject_yellow {A}
  (b : buffer A) (x : A) (cc : colored_buffer b Green) :
    colored_buffer (green_inject b x cc) Yellow.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - reflexivity.
  - simpl. right. reflexivity.
Qed.

Lemma green_inject_seq {A}
  (b : buffer A) (x : A) (cc : colored_buffer b Green) :
    buffer_seq (green_inject b x cc) = buffer_seq b ++ [x].
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pops off a green buffer. *)
Equations green_pop {A}
  (b : buffer A) (cc : colored_buffer b Green) : A * buffer A :=
green_pop (B2 a b)   _  := (a, B1 b);
green_pop (B3 a b c) _  := (a, B2 b c);
green_pop _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_pop_yellow {A}
  (b : buffer A) (cc : colored_buffer b Green) :
    let '(x, b') := green_pop b cc in
    colored_buffer b' Yellow.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - simpl. right. reflexivity.
  - reflexivity.
Qed.

Lemma green_pop_seq {A} (b : buffer A) (cc : colored_buffer b Green) :
  let '(x, b') := green_pop b cc in
  [x] ++ buffer_seq b' = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Ejects off a green buffer. *)
Equations green_eject {A}
  (b : buffer A) (cc : colored_buffer b Green) : buffer A * A :=
green_eject (B2 a b)   _  := (B1 a, b);
green_eject (B3 a b c) _  := (B2 a b, c);
green_eject _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_eject_yellow {A}
  (b : buffer A) (cc : colored_buffer b Green) :
    let '(b', x) := green_eject b cc in
    colored_buffer b' Yellow.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - simpl. right. reflexivity.
  - reflexivity.
Qed.

Lemma green_eject_seq {A} (b : buffer A) (cc : colored_buffer b Green) :
  let '(b', x) := green_eject b cc in
  buffer_seq b' ++ [x] = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pushes on a yellow buffer. *)
Equations yellow_push {A}
  (x : A) (b : buffer A) (cc : colored_buffer b Yellow) : buffer A :=
yellow_push x (B1 a)       _  := B2 x a;
yellow_push x (B2 a b)     _  := B3 x a b;
yellow_push x (B3 a b c)   _  := B4 x a b c;
yellow_push x (B4 a b c d) _  := B5 x a b c d;
yellow_push x _            cc := falsity cc _.

Lemma yellow_push_seq {A}
  (x : A) (b : buffer A) (cc : colored_buffer b Yellow) :
    buffer_seq (yellow_push x b cc) = [x] ++ buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Injects on a yellow buffer. *)
Equations yellow_inject {A}
  (b : buffer A) (x : A) (cc : colored_buffer b Yellow) : buffer A :=
yellow_inject (B1 a)       x _  := B2 a x;
yellow_inject (B2 a b)     x _  := B3 a b x;
yellow_inject (B3 a b c)   x _  := B4 a b c x;
yellow_inject (B4 a b c d) x _  := B5 a b c d x;
yellow_inject _            x cc := falsity cc _.

Lemma yellow_inject_seq {A}
  (b : buffer A) (x : A) (cc : colored_buffer b Yellow) :
    buffer_seq (yellow_inject b x cc) = buffer_seq b ++ [x].
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pops off a yellow buffer. *)
Equations yellow_pop {A}
  (b : buffer A) (cc : colored_buffer b Yellow) : A * buffer A :=
yellow_pop (B1 a)       _  := (a, B0);
yellow_pop (B2 a b)     _  := (a, B1 b);
yellow_pop (B3 a b c)   _  := (a, B2 b c);
yellow_pop (B4 a b c d) _  := (a, B3 b c d);
yellow_pop _            cc := falsity cc _.

Lemma yellow_pop_seq {A} (b : buffer A) (cc : colored_buffer b Yellow) :
  let '(x, b') := yellow_pop b cc in
  [x] ++ buffer_seq b' = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Ejects off a yellow buffer. *)
Equations yellow_eject {A}
  (b : buffer A) (cc : colored_buffer b Yellow) : buffer A * A :=
yellow_eject (B1 a)       _  := (B0, a);
yellow_eject (B2 a b)     _  := (B1 a, b);
yellow_eject (B3 a b c)   _  := (B2 a b, c);
yellow_eject (B4 a b c d) _  := (B3 a b c, d);
yellow_eject _            cc := falsity cc _.

Lemma yellow_eject_seq {A} (b : buffer A) (cc : colored_buffer b Yellow) :
  let '(b', x) := yellow_eject b cc in
  buffer_seq b' ++ [x] = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pushes on a buffer, and returns a green chain. *)
Equations buffer_push {A} (x : A) (b : buffer A) : chain A :=
buffer_push x B0             := Ending (B1 x);
buffer_push x (B1 a)         := Ending (B2 x a);
buffer_push x (B2 a b)       := Ending (B3 x a b);
buffer_push x (B3 a b c)     := Ending (B4 x a b c);
buffer_push x (B4 a b c d)   := Ending (B5 x a b c d);
buffer_push x (B5 a b c d e) :=
    Chain (Packet (B3 x a b) Hole (B3 c d e)) (Ending B0).

Lemma buffer_push_wf {A} (x : A) (b : buffer A) : wf_chain (buffer_push x b).
Proof.
  destruct b; simpl; try reflexivity.
  split; split; try reflexivity.
  right. intro. congruence.
Qed.

Lemma buffer_push_green {A} (x : A) (b : buffer A) :
  forall cc, cc Green -> colored_chain (buffer_push x b) cc.
Proof.
  destruct b; simpl; try reflexivity.
  exists Green; easy.
Qed.

Lemma buffer_push_seq {A} (x : A) (b : buffer A) :
    chain_seq (buffer_push x b) = [x] ++ buffer_seq b.
Proof. destruct b; reflexivity. Qed.

(* Injects on a buffer, and returns a green chain. *)
Equations buffer_inject {A} (b : buffer A) (x : A) : chain A :=
buffer_inject B0 x             := Ending (B1 x);
buffer_inject (B1 a) x         := Ending (B2 a x);
buffer_inject (B2 a b) x       := Ending (B3 a b x);
buffer_inject (B3 a b c) x     := Ending (B4 a b c x);
buffer_inject (B4 a b c d) x   := Ending (B5 a b c d x);
buffer_inject (B5 a b c d e) x :=
    Chain (Packet (B3 a b c) Hole (B3 d e x)) (Ending B0).

Lemma buffer_inject_wf {A} (b : buffer A) (x : A) :
  wf_chain (buffer_inject b x).
Proof.
  destruct b; simpl; try reflexivity.
  split; split; try reflexivity.
  right. intro. congruence.
Qed.

Lemma buffer_inject_green {A} (b : buffer A) (x : A) :
  forall cc, cc Green -> colored_chain (buffer_inject b x) cc.
Proof.
  destruct b; simpl; try reflexivity.
  exists Green; easy.
Qed.

Lemma buffer_inject_seq {A} (b : buffer A) (x : A) :
    chain_seq (buffer_inject b x) = buffer_seq b ++ [x].
Proof. destruct b; reflexivity. Qed.

(* Pops off a buffer, and returns an option. *)
Equations buffer_pop {A} (b : buffer A) : option (A * buffer A) :=
buffer_pop B0             := None;
buffer_pop (B1 a)         := Some (a, B0);
buffer_pop (B2 a b)       := Some (a, B1 b);
buffer_pop (B3 a b c)     := Some (a, B2 b c);
buffer_pop (B4 a b c d)   := Some (a, B3 b c d);
buffer_pop (B5 a b c d e) := Some (a, B4 b c d e).

Lemma buffer_pop_seq {A} (b : buffer A) :
    buffer_seq b =
      match buffer_pop b with
        | None => []
        | Some (x, b') => [x] ++ buffer_seq b'
      end.
Proof. destruct b; reflexivity. Qed.

(* Ejects off a buffer, and returns an option. *)
Equations buffer_eject {A} (b : buffer A) : option (buffer A * A) :=
buffer_eject B0             := None;
buffer_eject (B1 a)         := Some (B0, a);
buffer_eject (B2 a b)       := Some (B1 a, b);
buffer_eject (B3 a b c)     := Some (B2 a b, c);
buffer_eject (B4 a b c d)   := Some (B3 a b c, d);
buffer_eject (B5 a b c d e) := Some (B4 a b c d, e).

Lemma buffer_eject_seq {A} (b : buffer A) :
    buffer_seq b =
      match buffer_eject b with
        | None => []
        | Some (b', x) => buffer_seq b' ++ [x]
      end.
Proof. destruct b; reflexivity. Qed.

(* Pushes then ejects. *)
Equations prefix_rot {A} (x : A) (b : buffer A) : buffer A * A :=
prefix_rot x B0             := (B0, x);
prefix_rot x (B1 a)         := (B1 x, a);
prefix_rot x (B2 a b)       := (B2 x a, b);
prefix_rot x (B3 a b c)     := (B3 x a b, c);
prefix_rot x (B4 a b c d)   := (B4 x a b c, d);
prefix_rot x (B5 a b c d e) := (B5 x a b c d, e).

Lemma prefix_rot_seq {A} (x : A) (b : buffer A) :
    let '(b', x') := prefix_rot x b in
    [x] ++ buffer_seq b = buffer_seq b' ++ [x'].
Proof. destruct b; reflexivity. Qed.

(* Injects then pops. *)
Equations suffix_rot {A} (b : buffer A) (x : A) : A * buffer A :=
suffix_rot B0 x             := (x, B0);
suffix_rot (B1 a) x         := (a, B1 x);
suffix_rot (B2 a b) x       := (a, B2 b x);
suffix_rot (B3 a b c) x     := (a, B3 b c x);
suffix_rot (B4 a b c d) x   := (a, B4 b c d x);
suffix_rot (B5 a b c d e) x := (a, B5 b c d e x).

Lemma suffix_rot_seq {A} (b : buffer A) (x : A) :
    let '(x', b') := suffix_rot b x in
    buffer_seq b ++ [x] = [x'] ++ buffer_seq b'.
Proof. destruct b; reflexivity. Qed.

(* Merges an option and a pair to create a green buffer. *)
Equations prefix23 {A} (o : option A) (p: A * A) : buffer A :=
prefix23  None    (b, c) := B2 b c;
prefix23 (Some a) (b, c) := B3 a b c.

Lemma prefix23_green {A} (o : option A) (p : A * A) :
  colored_buffer (prefix23 o p) Green.
Proof. destruct o; destruct p; reflexivity. Qed.

Lemma prefix23_seq {A} (o : option A) (p : A * A) :
  buffer_seq (prefix23 o p) = option_seq o ++ pair_seq p.
Proof. destruct o; destruct p; reflexivity. Qed.

(* Merges a pair and an option to create a green buffer. *)
Equations suffix23 {A} (p : A * A) (o : option A) : buffer A :=
suffix23 (a, b)  None    := B2 a b;
suffix23 (a, b) (Some c) := B3 a b c.

Lemma suffix23_green {A} (p : A * A) (o : option A) :
  colored_buffer (suffix23 p o) Green.
Proof. destruct p; destruct o; reflexivity. Qed.

Lemma suffix23_seq {A} (p : A * A) (o : option A) :
  buffer_seq (suffix23 p o) = pair_seq p ++ option_seq o.
Proof. destruct p; destruct o; reflexivity. Qed.

(* Merges an element and an option to create a yellow buffer. *)
Equations suffix12 {A} (x : A) (o : option A) : buffer A :=
suffix12 x  None    := B1 x;
suffix12 x (Some y) := B2 x y.

Lemma suffix12_yellow {A} (x : A) (o : option A) :
  colored_buffer (suffix12 x o) Yellow.
Proof.
  destruct o; simpl.
  - reflexivity.
  - right. reflexivity.
Qed.

Lemma suffix12_seq {A} (x : A) (o : option A) :
  buffer_seq (suffix12 x o) = [x] ++ option_seq o.
Proof. destruct o; reflexivity. Qed.

(* Returns the decomposed version of a buffer. Here, it is a prefix
   decomposition: when the buffer has 4 or 5 elements, those at the end are
   set appart. *)
Equations prefix_decompose {A} (b : buffer A) : decompose A :=
prefix_decompose B0 := Underflow None;
prefix_decompose (B1 a) := Underflow (Some a);
prefix_decompose (B2 a b) := Ok (B2 a b) I;
prefix_decompose (B3 a b c) := Ok (B3 a b c) I;
prefix_decompose (B4 a b c d) := Overflow (B2 a b) I (c, d);
prefix_decompose (B5 a b c d e) := Overflow (B3 a b c) I (d, e).

Lemma prefix_decompose_seq {A} (b : buffer A) :
  let d := prefix_decompose b in
  buffer_seq b = decompose_main_seq d ++ decompose_rest_seq d.
Proof. destruct b; reflexivity. Qed.

(* Returns the decomposed version of a buffer. Here, it is a suffix
   decomposition: when the buffer has 4 or 5 elements, those at the start are
   set appart. *)
Equations suffix_decompose {A} (b : buffer A) : decompose A :=
suffix_decompose B0 := Underflow None;
suffix_decompose (B1 a) := Underflow (Some a);
suffix_decompose (B2 a b) := Ok (B2 a b) I;
suffix_decompose (B3 a b c) := Ok (B3 a b c) I;
suffix_decompose (B4 a b c d) := Overflow (B2 c d) I (a, b);
suffix_decompose (B5 a b c d e) := Overflow (B3 c d e) I (a, b).

Lemma suffix_decompose_seq {A} (b : buffer A) :
  let d := suffix_decompose b in
  buffer_seq b = decompose_rest_seq d ++ decompose_main_seq d.
Proof. destruct b; reflexivity. Qed.

(* Returns the sandwiched version of a buffer. *)
Equations buffer_unsandwich {A} (b : buffer A) : sandwich A :=
buffer_unsandwich B0 := Alone None;
buffer_unsandwich (B1 a) := Alone (Some a);
buffer_unsandwich (B2 a b) := Sandwich a B0 b;
buffer_unsandwich (B3 a b c) := Sandwich a (B1 b) c;
buffer_unsandwich (B4 a b c d) := Sandwich a (B2 b c) d;
buffer_unsandwich (B5 a b c d e) := Sandwich a (B3 b c d) e.

Lemma buffer_unsandwich_seq {A} (b : buffer A) :
  let s := buffer_unsandwich b in
  buffer_seq b = sandwich_seq s.
Proof. destruct b; reflexivity. Qed.

(* Converts a buffer to a buffer of pairs. If the buffer has an odd number of
   elements, the first is returned via an option. *)
Equations buffer_halve {A} (b : buffer A) : option A * buffer (A * A) :=
buffer_halve B0 := (None, B0);
buffer_halve (B1 a) := (Some a, B0);
buffer_halve (B2 a b) := (None, B1 (a, b));
buffer_halve (B3 a b c) := (Some a, B1 (b, c));
buffer_halve (B4 a b c d) := (None, B2 (a, b) (c, d));
buffer_halve (B5 a b c d e) := (Some a, B2 (b, c) (d, e)).

Lemma buffer_halve_seq {A} (b : buffer A) :
  let '(o, b') := buffer_halve b in
  buffer_seq b = option_seq o ++ flattenp (buffer_seq b').
Proof. destruct b; reflexivity. Qed.

(* A hint database of rewrites to be used when trying to automatically resolve
   obligations generated by [Equations]. *)
#[export] Hint Rewrite <-app_assoc : rlist.
#[export] Hint Rewrite app_nil_r : rlist.
#[export] Hint Rewrite app_nil_l : rlist.
#[export] Hint Rewrite flattenp_app : rlist.

Lemma green_buffer_to_yellow_buffer {A} (b : buffer A) :
  colored_buffer b Green -> colored_buffer b Yellow.
Proof.
  intro cc.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Takes a buffer of any color and a green buffer of pairs, rearranges elements
   contained in them, and returns a green buffer and a yellow buffer of pairs.
   The order of elements is preserved. *)
Equations green_prefix_concat {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Green) :
  buffer A * buffer (A * A) :=
green_prefix_concat b1 b2 cc with prefix_decompose b1 => {
  | Underflow opt with green_pop b2 cc => {
    | (ab, b) := (prefix23 opt ab, b) };
  | Ok b _ := (b, b2);
  | Overflow b _ ab := (b, green_push ab b2 cc) }.

Lemma green_prefix_concat_green_yellow {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Green) :
    let '(b1', b2') := green_prefix_concat b1 b2 cc in
    colored_buffer b1' Green /\ colored_buffer b2' Yellow.
Proof.
  unfold green_prefix_concat.
  destruct (prefix_decompose b1); simpl.
  - pose_destruct (green_pop_yellow b2 cc) (green_pop b2 cc) Heq Hcol.
    split.
    + apply prefix23_green.
    + assumption.
  - split.
    + assumption.
    + apply green_buffer_to_yellow_buffer; assumption.
  - split.
    + assumption.
    + apply green_push_yellow.
Qed.

Lemma green_prefix_concat_seq {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Green) :
    let '(b1', b2') := green_prefix_concat b1 b2 cc in
    buffer_seq b1  ++ flattenp (buffer_seq b2) =
    buffer_seq b1' ++ flattenp (buffer_seq b2').
Proof.
  unfold green_prefix_concat.
  pose_destruct (prefix_decompose_seq b1) (prefix_decompose b1) Heq Hseq.
  - pose_destruct (green_pop_seq b2 cc) (green_pop b2 cc) Heq' Hseq'.
    rewrite prefix23_seq.
    hauto db:rlist.
  - hauto db:rlist.
  - destruct p; simpl.
    rewrite green_push_seq.
    hauto db:rlist.
Qed.

(* Takes a green buffer of pairs and a buffer of any color, rearranges elements
   contained in them, and returns a yellow buffer of pairs and a green buffer.
   The order of elements is preserved. *)
Equations green_suffix_concat {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Green) (b2 : buffer A) :
  buffer (A * A) * buffer A :=
green_suffix_concat b1 cc b2 with suffix_decompose b2 => {
  | Underflow opt with green_eject b1 cc => {
    | (b, ab) := (b, suffix23 ab opt) };
  | Ok b _ := (b1, b);
  | Overflow b _ ab := (green_inject b1 ab cc, b) }.

Lemma green_suffix_concat_yellow_green {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Green) (b2 : buffer A) :
    let '(b1', b2') := green_suffix_concat b1 cc b2 in
    colored_buffer b1' Yellow /\ colored_buffer b2' Green.
Proof.
  unfold green_suffix_concat.
  destruct (suffix_decompose b2); simpl.
  - pose_destruct (green_eject_yellow b1 cc) (green_eject b1 cc) Heq Hcol.
    split.
    + assumption.
    + apply suffix23_green.
  - split.
    + apply green_buffer_to_yellow_buffer; assumption.
    + assumption.
  - split.
    + apply green_inject_yellow.
    + assumption.
Qed.

Lemma green_suffix_concat_seq {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Green) (b2 : buffer A) :
    let '(b1', b2') := green_suffix_concat b1 cc b2 in
    flattenp (buffer_seq b1)  ++ buffer_seq b2 =
    flattenp (buffer_seq b1') ++ buffer_seq b2'.
Proof.
  unfold green_suffix_concat.
  pose_destruct (suffix_decompose_seq b2) (suffix_decompose b2) Heq Hseq.
  - pose_destruct (green_eject_seq b1 cc) (green_eject b1 cc) Heq' Hseq'.
    rewrite suffix23_seq.
    hauto db:rlist.
  - hauto db:rlist.
  - destruct p; simpl.
    rewrite green_inject_seq.
    hauto db:rlist.
Qed.

(* Takes a buffer of any color and a yellow buffer of pairs, rearranges
   elements contained in them, and returns a green buffer and a buffer of pairs
   of any color.
   The order of elements is preserved. *)
Equations yellow_prefix_concat {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Yellow) :
  buffer A * buffer (A * A) :=
yellow_prefix_concat b1 b2 cc with prefix_decompose b1 => {
  | Underflow opt with yellow_pop b2 cc => {
    | (ab, b) := (prefix23 opt ab, b) };
  | Ok b _ := (b, b2);
  | Overflow b _ ab := (b, yellow_push ab b2 cc) }.

Lemma yellow_prefix_concat_green_red {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Yellow) :
    let '(b1', b2') := yellow_prefix_concat b1 b2 cc in
    colored_buffer b1' Green.
Proof.
  unfold yellow_prefix_concat.
  destruct (prefix_decompose b1); simpl.
  - destruct (yellow_pop b2 cc); simpl.
    apply prefix23_green.
  - assumption.
  - assumption.
Qed.

Lemma yellow_prefix_concat_seq {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer b2 Yellow) :
    let '(b1', b2') := yellow_prefix_concat b1 b2 cc in
    buffer_seq b1  ++ flattenp (buffer_seq b2) =
    buffer_seq b1' ++ flattenp (buffer_seq b2').
Proof.
  unfold yellow_prefix_concat.
  pose_destruct (prefix_decompose_seq b1) (prefix_decompose b1) Heq Hseq.
  - pose_destruct (yellow_pop_seq b2 cc) (yellow_pop b2 cc) Heq' Hseq'.
    rewrite prefix23_seq.
    hauto db:rlist.
  - hauto db:rlist.
  - destruct p; simpl.
    rewrite yellow_push_seq.
    hauto db:rlist.
Qed.

(* Takes a yellow buffer of pairs and a buffer of any color, rearranges
   elements contained in them, and returns a buffer of pairs of any color and a
   green buffer.
   The order of elements is preserved. *)
Equations yellow_suffix_concat {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Yellow) (b2 : buffer A) :
  buffer (A * A) * buffer A :=
yellow_suffix_concat b1 cc b2 with suffix_decompose b2 => {
  | Underflow opt with yellow_eject b1 cc => {
    | (b, ab) := (b, suffix23 ab opt) };
  | Ok b _ := (b1, b);
  | Overflow b _ ab := (yellow_inject b1 ab cc, b) }.

Lemma yellow_suffix_concat_red_green {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Yellow) (b2 : buffer A) :
    let '(b1', b2') := yellow_suffix_concat b1 cc b2 in
    colored_buffer b2' Green.
Proof.
  unfold yellow_suffix_concat.
  destruct (suffix_decompose b2); simpl.
  - destruct (yellow_eject b1 cc); simpl.
    apply suffix23_green.
  - assumption.
  - assumption.
Qed.

Lemma yellow_suffix_concat_seq {A}
  (b1 : buffer (A * A)) (cc : colored_buffer b1 Yellow) (b2 : buffer A) :
    let '(b1', b2') := yellow_suffix_concat b1 cc b2 in
    flattenp (buffer_seq b1)  ++ buffer_seq b2 =
    flattenp (buffer_seq b1') ++ buffer_seq b2'.
Proof.
  unfold yellow_suffix_concat.
  pose_destruct (suffix_decompose_seq b2) (suffix_decompose b2) Heq Hseq.
  - pose_destruct (yellow_eject_seq b1 cc) (yellow_eject b1 cc) Heq' Hseq'.
    rewrite suffix23_seq.
    hauto db:rlist.
  - hauto db:rlist.
  - destruct p; simpl.
    rewrite yellow_inject_seq.
    hauto db:rlist.
Qed.

(* Creates a green chain from 3 options. *)
Equations chain_of_opt3 {A}
  (o1 : option A) (o2 : option (A * A)) (o3 : option A) : chain A :=
chain_of_opt3 None None None := Ending B0;
chain_of_opt3 (Some a) None None := Ending (B1 a);
chain_of_opt3 None None (Some a) := Ending (B1 a);
chain_of_opt3 (Some a) None (Some b) := Ending (B2 a b);
chain_of_opt3 None (Some (a, b)) None := Ending (B2 a b);
chain_of_opt3 (Some a) (Some (b, c)) None := Ending (B3 a b c);
chain_of_opt3 None (Some (a, b)) (Some c) := Ending (B3 a b c);
chain_of_opt3 (Some a) (Some (b, c)) (Some d) := Ending (B4 a b c d).

Lemma chain_of_opt3_wf {A}
  (o1 : option A) (o2 : option (A * A)) (o3 : option A) :
  wf_chain (chain_of_opt3 o1 o2 o3).
Proof.
  destruct o1; destruct o2; try destruct p; destruct o3; simpl; exact I.
Qed.

Lemma chain_of_opt3_green {A}
  (o1 : option A) (o2 : option (A * A)) (o3 : option A) :
  colored_chain (chain_of_opt3 o1 o2 o3) green.
Proof.
  destruct o1; destruct o2; try destruct p; destruct o3; simpl; exact I.
Qed.

Lemma chain_of_opt3_seq {A}
  (o1 : option A) (o2 : option (A * A)) (o3 : option A) :
  chain_seq (chain_of_opt3 o1 o2 o3) =
    option_seq o1 ++ flattenp (option_seq o2) ++ option_seq o3.
Proof.
  destruct o1; destruct o2; try destruct p; destruct o3; simpl; hauto db:rlist.
Qed.

(* Takes a prefix buffer, a child buffer, and a suffix buffer, and rearranges
   all elements contained in these buffers to form a green chain.
   The order of elements is preserved. *)
Equations make_small {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (b3 : buffer A) :
  chain A :=
make_small b1 b2 b3 with prefix_decompose b1, suffix_decompose b3 => {
  | Underflow p1, Underflow s1 with buffer_unsandwich b2 => {
    | Alone opt := chain_of_opt3 p1 opt s1 ;
    | Sandwich ab rest cd :=
      Chain (Packet (prefix23 p1 ab) Hole (suffix23 cd s1)) (Ending rest) };
  | Underflow p1, Ok s1 _ with buffer_pop b2 => {
    | None with p1 => {
      | None := Ending s1;
      | Some x := buffer_push x s1 };
    | Some (cd, rest) :=
      Chain (Packet (prefix23 p1 cd) Hole s1) (Ending rest) };
  | Underflow p1, Overflow s1 _ ab with suffix_rot b2 ab => {
    | (cd, center) :=
      Chain (Packet (prefix23 p1 cd) Hole s1) (Ending center) };
  | Ok p1 _, Underflow s1 with buffer_eject b2 => {
    | None with s1 => {
      | None := Ending p1;
      | Some x := buffer_inject p1 x };
    | Some (rest, ab) :=
      Chain (Packet p1 Hole (suffix23 ab s1)) (Ending rest) };
  | Ok p1 _, Ok s1 _ := Chain (Packet p1 Hole s1) (Ending b2);
  | Ok p1 _, Overflow s1 _ ab :=
    Chain (Packet p1 Hole s1) (buffer_inject b2 ab);
  | Overflow p1 _ cd, Underflow s1 with prefix_rot cd b2 => {
    | (center, ab) :=
      Chain (Packet p1 Hole (suffix23 ab s1)) (Ending center) };
  | Overflow p1 _ cd, Ok s1 _ := Chain (Packet p1 Hole s1) (buffer_push cd b2);
  | Overflow p1 _ cd, Overflow s1 _ ab with buffer_halve b2 => {
    | (x, rest) :=
      Chain (Packet p1 (Packet (suffix12 cd x) Hole (B1 ab)) s1) (Ending rest) }
}.

Lemma make_small_wf {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (b3 : buffer A) :
  wf_chain (make_small b1 b2 b3).
Proof.
  unfold make_small. unfold make_small_clause_1.
  remember (prefix_decompose b1) as pd. remember (suffix_decompose b3) as sd.
  destruct pd; destruct sd; simpl.
  - destruct (buffer_unsandwich b2); simpl.
    + apply chain_of_opt3_wf.
    + auto.
  - destruct (buffer_pop b2); simpl.
    + destruct p as [cd rest]; simpl; auto.
    + destruct o; simpl.
      -- apply buffer_push_wf.
      -- exact I.
  - destruct (suffix_rot b2 p) as [cd center]; simpl; auto.
  - destruct (buffer_eject b2); simpl.
    + destruct p as [rest ab]; simpl; auto.
    + destruct o; simpl.
      -- apply buffer_inject_wf.
      -- exact I.
  - auto.
  - split; auto; split.
    + destruct b2; try exists Green; simpl; repeat split; left; easy.
    + apply buffer_inject_wf.
  - destruct (prefix_rot p b2) as [center ab]; simpl; auto.
  - split; auto; split.
    + destruct b2; try exists Green; simpl; repeat split; left; easy.
    + apply buffer_push_wf.
  - destruct (buffer_halve b2) as [x rest]; simpl.
    split; auto; split; auto; split; auto.
    left; split; auto; split; auto.
    apply suffix12_yellow.
Qed.

Lemma make_small_green {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (b3 : buffer A) :
  colored_chain (make_small b1 b2 b3) green.
Proof.
  unfold make_small. unfold make_small_clause_1.
  remember (prefix_decompose b1) as pd. remember (suffix_decompose b3) as sd.
  destruct pd; destruct sd; simpl.
  - destruct (buffer_unsandwich b2); simpl.
    + apply chain_of_opt3_green.
    + exists Green; split; try easy; split.
      -- apply prefix23_green.
      -- apply suffix23_green.
  - destruct (buffer_pop b2); simpl.
    + destruct p as [cd rest]; simpl.
      exists Green; split; try easy; split.
      -- apply prefix23_green.
      -- assumption.
    + destruct o; simpl.
      -- apply buffer_push_green. easy.
      -- exact I.
  - destruct (suffix_rot b2 p) as [cd center]; simpl.
    exists Green; split; try easy; split.
    + apply prefix23_green.
    + assumption.
  - destruct (buffer_eject b2); simpl.
    + destruct p as [rest ab]; simpl.
      exists Green; split; try easy; split.
      -- assumption.
      -- apply suffix23_green.
    + destruct o; simpl.
      -- apply buffer_inject_green. easy.
      -- exact I.
  - exists Green; split; try easy; split; assumption.
  - exists Green; split; try easy; split; assumption.
  - destruct (prefix_rot p b2) as [center ab]; simpl.
    exists Green; split; try easy; split.
    + assumption.
    + apply suffix23_green.
  - exists Green; split; try easy; split; assumption.
  - destruct (buffer_halve b2) as [x rest]; simpl.
    exists Green; split; try easy; split; assumption.
Qed.

Lemma make_small_seq {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (b3 : buffer A) :
  chain_seq (make_small b1 b2 b3) =
  buffer_seq b1 ++ flattenp (buffer_seq b2) ++ buffer_seq b3.
Proof.
  unfold make_small. unfold make_small_clause_1.
  remember (prefix_decompose b1) as pd. remember (suffix_decompose b3) as sd.
  pose (prefix_decompose_seq b1) as Hpds; rewrite <-Heqpd in Hpds; simpl in Hpds.
  pose (suffix_decompose_seq b3) as Hsds; rewrite <-Heqsd in Hsds; simpl in Hsds.
  destruct pd; destruct sd; simpl in *.
  - remember (buffer_unsandwich b2) as bus.
    pose (buffer_unsandwich_seq b2) as Hbuss; rewrite <-Heqbus in Hbuss; simpl in Hbuss.
    destruct bus; simpl in *.
    + rewrite chain_of_opt3_seq; simpl. hauto db:rlist.
    + rewrite prefix23_seq. rewrite suffix23_seq. hauto db:rlist.
  - remember (buffer_pop b2) as bp.
    pose (buffer_pop_seq b2) as Hbps; rewrite <-Heqbp in Hbps; simpl in Hbps.
    destruct bp; simpl in *.
    + destruct p as [cd rest]; simpl. rewrite prefix23_seq. hauto db:rlist.
    + destruct o; simpl in *.
      -- rewrite buffer_push_seq. hauto db:rlist.
      -- hauto db:rlist.
  - remember (suffix_rot b2 p) as sr.
    pose (suffix_rot_seq b2 p) as Hsrs; rewrite <-Heqsr in Hsrs; simpl in Hsrs.
    destruct sr as [cd center]; simpl in *.
    rewrite prefix23_seq.
    replace (pair_seq cd) with (flattenp [cd]) by hauto.
    rewrite Hpds; do 2 rewrite <-app_assoc; f_equal; rewrite app_nil_l.
    rewrite Hsds; do 2 rewrite app_assoc; f_equal.
    rewrite <-flattenp_app; rewrite <-Hsrs; rewrite flattenp_app.
    hauto.
  - remember (buffer_eject b2) as be.
    pose (buffer_eject_seq b2)  as Hbes; rewrite <-Heqbe in Hbes; simpl in Hbes.
    destruct be; simpl in *.
    + destruct p as [rest ab]; simpl. hauto db:rlist.
    + destruct o; simpl in *.
      -- rewrite buffer_inject_seq. hauto db:rlist.
      -- hauto db:rlist.
  - hauto db:rlist.
  - rewrite buffer_inject_seq. hauto db:rlist.
  - remember (prefix_rot p b2) as pr.
    pose (prefix_rot_seq p b2) as Hprs; rewrite <-Heqpr in Hprs; simpl in Hprs.
    destruct pr as [center ab]; simpl in *.
    rewrite suffix23_seq.
    replace (pair_seq ab) with (flattenp [ab]) by hauto.
    rewrite Hpds; rewrite <-app_assoc; f_equal.
    rewrite Hsds; rewrite app_nil_l; do 2 rewrite app_assoc; f_equal.
    rewrite <-flattenp_app; rewrite <-Hprs; rewrite flattenp_app.
    hauto.
  - rewrite buffer_push_seq. hauto db:rlist.
  - remember (buffer_halve b2) as bh.
    pose (buffer_halve_seq b2) as Hbhs; rewrite <-Heqbh in Hbhs; simpl in Hbhs.
    destruct bh as [x rest]; simpl.
    rewrite suffix12_seq.
    hauto db:rlist.
Qed.

Lemma are_green {A} (p1 s1 : buffer A) :
  (colored_buffer p1 Green /\ colored_buffer s1 Green) +
  ~ (colored_buffer p1 Green /\ colored_buffer s1 Green).
Proof.
  destruct p1; destruct s1; try (left; easy); right; easy.
Qed.

Lemma are_yellow {A} (p1 s1 : buffer A) :
  (colored_buffer p1 Yellow /\ colored_buffer s1 Yellow) +
  ~ (colored_buffer p1 Yellow /\ colored_buffer s1 Yellow).
Proof.
  destruct p1; destruct s1; try (left; easy); right; easy.
Qed.

(* Makes a red chain green. *)
Equations ensure_green {A} (c : chain A) : chain A :=
ensure_green (Chain (Packet p1 Hole s1) (Ending b)) := make_small p1 b s1;
ensure_green (Chain (Packet p1 Hole s1) (Chain (Packet p2 p s2) c))
  with are_green p1 s1 => {
  | inl _ := Chain (Packet p1 Hole s1) (Chain (Packet p2 p s2) c);
  | inr _ with are_green p2 s2 => {
    | inl (conj p2g s2g)
      with green_prefix_concat p1 p2 p2g, green_suffix_concat s2 s2g s1 => {
      | (p1', p2'), (s2', s1') :=
        Chain (Packet p1' (Packet p2' p s2') s1') c };
    | inr _ := Chain (Packet p1 Hole s1) (Chain (Packet p2 p s2) c) } };
ensure_green (Chain (Packet p1 (Packet p2 p s2) s1) c)
  with are_green p1 s1 => {
  | inl _ => Chain (Packet p1 (Packet p2 p s2) s1) c
  | inr _ with are_yellow p2 s2 => {
    | inl (conj p2y s2y)
      with yellow_prefix_concat p1 p2 p2y, yellow_suffix_concat s2 s2y s1 => {
      | (p1', p2'), (s2', s1') :=
        Chain (Packet p1' Hole s1') (Chain (Packet p2' p s2') c) }
    | inr _ := Chain (Packet p1 (Packet p2 p s2) s1) c } };
ensure_green c := c.

Lemma colored_buffer_split {A : Type} (b1 b2 : buffer A) cc1 cc2 :
  forall c, cc1 c -> cc2 c ->
    (colored_buffer b1 c /\ colored_buffer b2 c) /\ cc1 c \/
    ~ (colored_buffer b1 c /\ colored_buffer b2 c) /\ cc2 c.
Proof.
  intros c cc1c cc2c.
  destruct b1; destruct b2; destruct c; simpl; easy.
Qed.

Lemma ensure_green_wf {A} (c : chain A) (c_wf : wf_chain c) :
  wf_chain (ensure_green c).
Proof.
  destruct c; simpl; auto.
  destruct p as [|B p1 pkt s1]; simpl.
  - apply (falsity c_wf). unfold wf_chain, wf_packet.
    apply proj1.
  - destruct pkt as [|B p2 pkt s2]; simpl.
    + destruct c as [|B pkt c]; try apply make_small_wf.
      destruct pkt as [|B p2 pkt s2]; try easy.
      destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try exact c_wf; simpl.
      destruct (are_green p2 s2) as [[p2g s2g] | np2s2g]; try exact c_wf; simpl.
      pose_destruct (green_prefix_concat_green_yellow p1 p2 p2g) (green_prefix_concat p1 p2 p2g) Heqp cbp.
      pose_destruct (green_suffix_concat_yellow_green s2 s2g s1) (green_suffix_concat s2 s2g s1) Heqs cbs.
      destruct cbp as [p1'g p2'y]; destruct cbs as [s2'y s1'g].
      destruct c_wf as [p1_wf [cc2 [[p2c p2_wf] [ccc c_wf]]]].
      repeat split; auto.
      destruct c as [|C pkt' c]; auto.
      destruct ccc as [col [[[Ht Hgr] | [Hf Hg]] colpkt]]; simpl in *.
      -- exists col; easy.
      -- easy.
    + destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try exact c_wf; simpl.
      destruct (are_yellow p2 s2) as [[p2y s2y] | ny]; try exact c_wf; simpl.
      pose_destruct (yellow_prefix_concat_green_red p1 p2 p2y) (yellow_prefix_concat p1 p2 p2y) Heqp p1g.
      pose_destruct (yellow_suffix_concat_red_green s2 s2y s1) (yellow_suffix_concat s2 s2y s1) Heqs s1g.
      destruct c_wf as [[p1_wf [p2_wf pkt_wf]] [ccc c_wf]]; simpl.
      repeat split; auto.
      -- exists Red; split; try left; easy.
      -- repeat split; try easy.
        destruct c as [|p3 pkt' s3]; auto; simpl in *.
        exists Green; split.
        ++ apply colored_buffer_split; easy.
        ++ destruct ccc as [col [[[Hf Hgr] | [Hn Hg]] colpkt]]; easy.
Qed.

Lemma ensure_green_green {A} (c : chain A) (c_wf : wf_chain c) :
  colored_chain (ensure_green c) green.
Proof.
  destruct c; simpl; auto.
  destruct p as [|B p1 pkt s1]; simpl.
  - apply (falsity c_wf). unfold wf_chain, wf_packet.
    apply proj1.
  - destruct pkt as [|B p2 pkt s2]; simpl.
    + destruct c as [|B pkt c]; try apply make_small_green.
      destruct pkt as [|B p2 pkt s2]; try easy.
      destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try easy; simpl.
      destruct (are_green p2 s2) as [[p2g s2g] | np2s2g]; simpl.
      -- pose_destruct (green_prefix_concat_green_yellow p1 p2 p2g) (green_prefix_concat p1 p2 p2g) Heqp cbp.
         pose_destruct (green_suffix_concat_yellow_green s2 s2g s1) (green_suffix_concat s2 s2g s1) Heqs cbs.
         exists Green; easy.
      -- destruct c_wf as [p1_wf [[col [[[Hf Hgr] | [Ht Hg]] colpkt]] c_wf]].
         ++ easy.
         ++ unfold green in Hg; subst; easy.
    + destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try easy; simpl.
      destruct (are_yellow p2 s2) as [[p2y s2y] | np2s2y]; try easy; simpl.
      pose_destruct (yellow_prefix_concat_green_red p1 p2 p2y) (yellow_prefix_concat p1 p2 p2y) Heqp p1g.
      pose_destruct (yellow_suffix_concat_red_green s2 s2y s1) (yellow_suffix_concat s2 s2y s1) Heqs s1g.
      easy.
Qed.

Lemma ensure_green_seq {A} (c : chain A) (c_wf : wf_chain c) :
  chain_seq (ensure_green c) = chain_seq c.
Proof.
  destruct c; simpl; auto.
  destruct p as [|B p1 pkt s1]; simpl.
  - apply (falsity c_wf). unfold wf_chain, wf_packet.
    apply proj1.
  - destruct pkt as [|B p2 pkt s2]; simpl.
    + destruct c as [|B pkt c]; try apply make_small_seq.
      destruct pkt as [|B p2 pkt s2]; try easy.
      destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try easy; simpl.
      destruct (are_green p2 s2) as [[p2g s2g] | np2s2g]; try easy; simpl.
      pose_destruct (green_prefix_concat_seq p1 p2 p2g) (green_prefix_concat p1 p2 p2g) Heqp Hseqp.
      pose_destruct (green_suffix_concat_seq s2 s2g s1) (green_suffix_concat s2 s2g s1) Heqs Hseqs.
      repeat rewrite flattenp_app.
      repeat rewrite app_assoc.
      rewrite Hseqp.
      repeat rewrite <-app_assoc.
      do 3 f_equal.
      easy.
    + destruct (are_green p1 s1) as [[p1g s1g] | np1s1g]; try easy; simpl.
      destruct (are_yellow p2 s2) as [[p2y s2y] | np2s2y]; try easy; simpl.
      pose_destruct (yellow_prefix_concat_seq p1 p2 p2y) (yellow_prefix_concat p1 p2 p2y) Heqp Hseqp.
      pose_destruct (yellow_suffix_concat_seq s2 s2y s1) (yellow_suffix_concat s2 s2y s1) Heqs Hseqs.
      repeat rewrite flattenp_app.
      repeat rewrite app_assoc.
      rewrite Hseqp.
      repeat rewrite <-app_assoc.
      do 3 f_equal.
      easy.
Qed.

(* +------------------------------------------------------------------------+ *)
(* |                               Operations                               | *)
(* +------------------------------------------------------------------------+ *)

(* The empty deque. *)
Equations empty {A : Type} : deque A :=
empty := T (Ending B0).

Lemma empty_wf {A} : wf_deque (@empty A).
Proof. easy. Qed.

Lemma empty_seq {A} : deque_seq (@empty A) = [].
Proof. reflexivity. Qed.

(* Pushes on a deque. *)
Equations push {A : Type} (x : A) (d : deque A) : deque A :=
push x (T (Ending b)) := T (buffer_push x b);
push x (T (Chain (Packet p pkt s) c)) with are_green p s => {
  | inl (conj pg _) :=
    T (Chain (Packet (green_push x p pg) pkt s) (ensure_green c));
  | inr _ with are_yellow p s => {
    | inl (conj py _) :=
      T (ensure_green (Chain (Packet (yellow_push x p py) pkt s) c));
    | inr _ := T (Chain (Packet p pkt s) c) } };
push _ d := d.

Lemma chain_green_to_green_or_yellow {A} (c : chain A) :
  colored_chain c green -> colored_chain c green_or_yellow.
Proof. intro ccg. destruct c; easy. Qed.

Lemma regularity_green {A B} (pkt : packet A B) : (regularity pkt) Green.
Proof.
  destruct pkt; try easy; simpl.
  destruct (are_green b b0) as [[bg b0g] | nbb0g]; easy.
Qed.

Lemma chain_green_to_regularity {A B} (c : chain B) (pkt : packet A B) :
  colored_chain c green -> colored_chain c (regularity pkt).
Proof.
  intro ccg.
  destruct c; auto; simpl in *.
  destruct ccg as [col [colg colpkt]].
  exists col; split; auto.
  unfold green in colg; subst.
  apply regularity_green.
Qed.

Lemma push_wf {A} (x : A) (d : deque A) (d_wf : wf_deque d) :
  wf_deque (push x d).
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - split.
    + apply buffer_push_green. easy.
    + apply buffer_push_wf.
  - destruct d_wf as [cc [pkt_wf rest]].
    simpl in pkt_wf. destruct pkt_wf.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      repeat split; auto.
      -- exists Yellow; repeat split; try easy.
         ++ apply green_push_yellow.
         ++ apply green_buffer_to_yellow_buffer. easy.
      -- apply chain_green_to_regularity.
         apply ensure_green_green.
         exact c_wf.
      -- apply ensure_green_wf.
         exact c_wf.
    + destruct (are_yellow p s) as [[py sy] | npsy]; try exact d_wf.
      destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      assert (wf_chain (Chain (Packet (yellow_push x p py) pkt s) c)) as pc_wf.
      -- repeat split; auto.
         destruct c; auto.
         destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']].
         ++ exfalso. apply npsg. exact psg.
         ++ exists Green; split; try easy.
            apply regularity_green.
      -- repeat split; auto.
         ++ apply chain_green_to_green_or_yellow.
            apply ensure_green_green.
            exact pc_wf.
         ++ apply ensure_green_wf.
            exact pc_wf.
Qed.

Lemma push_seq {A} (x : A) (d : deque A) (d_wf : wf_deque d) :
  deque_seq (push x d) = [x] ++ deque_seq d.
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - apply buffer_push_seq.
  - destruct d_wf as [cc [pkt_wf rest]].
    simpl in pkt_wf. destruct pkt_wf.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + rewrite green_push_seq.
      rewrite ensure_green_seq; easy.
    + destruct (are_yellow p s) as [[py sy] | npsy].
      -- unfold push_clause_2_clause_2.
         rewrite deque_seq_equation_1.
         rewrite ensure_green_seq; simpl.
         ++ rewrite yellow_push_seq. easy.
         ++ destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
            repeat split; auto.
            destruct c; auto.
            destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']].
            --- exfalso. apply npsg. exact psg.
            --- exists Green; split; try easy.
                apply regularity_green.
      -- destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
         destruct colgy; subst; easy.
Qed.

(* Injects on a deque. *)
Equations inject {A : Type} (d : deque A) (x : A) : deque A :=
inject (T (Ending b)) x := T (buffer_inject b x);
inject (T (Chain (Packet p pkt s) c)) x with are_green p s => {
  | inl (conj _ sg) :=
    T (Chain (Packet p pkt (green_inject s x sg)) (ensure_green c));
  | inr _ with are_yellow p s => {
    | inl (conj _ sy) :=
      T (ensure_green (Chain (Packet p pkt (yellow_inject s x sy)) c));
    | inr _ := T (Chain (Packet p pkt s) c) } };
inject d _ := d.

Lemma inject_wf {A} (d : deque A) (x : A) (d_wf : wf_deque d) :
  wf_deque (inject d x).
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - split.
    + apply buffer_inject_green. easy.
    + apply buffer_inject_wf.
  - destruct d_wf as [cc [pkt_wf rest]].
    simpl in pkt_wf. destruct pkt_wf.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      repeat split; auto.
      -- exists Yellow; repeat split; try easy.
         ++ apply green_buffer_to_yellow_buffer. easy.
         ++ apply green_inject_yellow.
      -- apply chain_green_to_regularity.
         apply ensure_green_green.
         exact c_wf.
      -- apply ensure_green_wf.
         exact c_wf.
    + destruct (are_yellow p s) as [[py sy] | npsy]; try exact d_wf.
      destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      assert (wf_chain (Chain (Packet p pkt (yellow_inject s x sy)) c)) as pc_wf.
      -- repeat split; auto.
         destruct c; auto.
         destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']].
         ++ exfalso. apply npsg. exact psg.
         ++ exists Green; split; try easy.
            apply regularity_green.
      -- repeat split; auto.
         ++ apply chain_green_to_green_or_yellow.
            apply ensure_green_green.
            exact pc_wf.
         ++ apply ensure_green_wf.
            exact pc_wf.
Qed.

Lemma inject_seq {A} (d : deque A) (x : A) (d_wf : wf_deque d) :
  deque_seq (inject d x) = deque_seq d ++ [x].
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - apply buffer_inject_seq.
  - destruct d_wf as [cc [pkt_wf rest]].
    simpl in pkt_wf. destruct pkt_wf.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + rewrite green_inject_seq.
      rewrite ensure_green_seq; hauto db:rlist.
    + destruct (are_yellow p s) as [[py sy] | npsy].
      -- unfold inject_clause_2_clause_2.
         rewrite deque_seq_equation_1.
         rewrite ensure_green_seq; simpl.
         ++ rewrite yellow_inject_seq; hauto db:rlist.
         ++ destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
            repeat split; auto.
            destruct c; auto.
            destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']].
            --- exfalso. apply npsg. exact psg.
            --- exists Green; split; try easy.
                apply regularity_green.
      -- destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
         destruct colgy; subst; easy.
Qed.

(* Pops off a deque. *)
Equations pop {A : Type} (d : deque A) : option (A * deque A) :=
pop (T (Ending b)) with buffer_pop b => {
  | None := None;
  | Some (x, b') := Some (x, T (Ending b')) };
pop (T (Chain (Packet p pkt s) c)) with are_green p s => {
  | inl (conj pg _) with green_pop p pg => { | (x, p') :=
    Some (x, T (Chain (Packet p' pkt s) (ensure_green c))) };
  | inr _ with are_yellow p s => {
    | inl (conj py _) with yellow_pop p py => { | (x, p') :=
      Some (x, T (ensure_green (Chain (Packet p' pkt s) c))) };
    | inr _ := None } };
pop d := None.

Lemma pop_wf {A} (d : deque A) (d_wf : wf_deque d) :
  match pop d with
  | None => True
  | Some (_, d') => wf_deque d'
  end.
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - destruct (buffer_pop b) as [[x b']|]; easy.
  - easy.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      pose_destruct (green_pop_yellow p pg) (green_pop p pg) Heq Hcol; simpl.
      repeat split; auto.
      -- exists Yellow; repeat split; try easy.
         apply green_buffer_to_yellow_buffer. easy.
      -- apply chain_green_to_regularity.
         apply ensure_green_green.
         exact c_wf.
      -- apply ensure_green_wf.
         exact c_wf.
    + destruct (are_yellow p s) as [[py sy] | npsy]; simpl; auto.
      destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      destruct (yellow_pop p py) as [x p'] eqn: Hyp.
      assert (wf_chain (Chain (Packet p' pkt s) c)) as pc_wf.
      -- repeat split; auto.
         destruct c; auto.
         apply chain_green_to_regularity.
         destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']]; easy.
      -- split.
         ++ apply chain_green_to_green_or_yellow.
            apply ensure_green_green.
            apply pc_wf.
         ++ apply ensure_green_wf.
            apply pc_wf.
Qed.

Lemma pop_seq {A} (d : deque A) (d_wf : wf_deque d) :
  deque_seq d = match pop d with
                | None => []
                | Some (x, d') => [x] ++ deque_seq d'
                end.
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - pose_destruct (buffer_pop_seq b) (buffer_pop b) Heq Hseq; auto.
    destruct p. easy.
  - easy.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      pose_destruct (green_pop_seq p pg) (green_pop p pg) Heq Hseq; simpl.
      rewrite ensure_green_seq; auto.
      hauto db:rlist.
    + destruct (are_yellow p s) as [[py sy] | npsy]; simpl.
      -- unfold pop_clause_2_clause_2_clause_1.
         remember (yellow_pop p py) as tmp.
         pose (yellow_pop_seq p py) as Hseq; rewrite <-Heqtmp in Hseq.
         destruct tmp as [x p'].
         rewrite deque_seq_equation_1.
         rewrite ensure_green_seq.
         ++ hauto db:rlist.
         ++ destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
            repeat split; auto.
            destruct c; auto.
            apply chain_green_to_regularity.
            destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']]; easy.
      -- destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
         destruct col; unfold green_or_yellow in colgy; easy.
Qed.

(* Ejects off a deque. *)
Equations eject {A : Type} (d : deque A) : option (deque A * A) :=
eject (T (Ending b)) with buffer_eject b => {
  | None := None;
  | Some (b', x) := Some (T (Ending b'), x) };
eject (T (Chain (Packet p pkt s) c)) with are_green p s => {
  | inl (conj _ sg) with green_eject s sg => { | (s', x) :=
    Some (T (Chain (Packet p pkt s') (ensure_green c)), x) };
  | inr _ with are_yellow p s => {
    | inl (conj _ sy) with yellow_eject s sy => { | (s', x) :=
      Some (T (ensure_green (Chain (Packet p pkt s') c)), x) };
    | inr _ := None } };
eject d := None.

Lemma eject_wf {A} (d : deque A) (d_wf : wf_deque d) :
  match eject d with
  | None => True
  | Some (d', _) => wf_deque d'
  end.
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - destruct (buffer_eject b) as [[x b']|]; easy.
  - easy.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      pose_destruct (green_eject_yellow s sg) (green_eject s sg) Heq Hcol; simpl.
      repeat split; auto.
      -- exists Yellow; repeat split; try easy.
         apply green_buffer_to_yellow_buffer. easy.
      -- apply chain_green_to_regularity.
         apply ensure_green_green.
         exact c_wf.
      -- apply ensure_green_wf.
         exact c_wf.
    + destruct (are_yellow p s) as [[py sy] | npsy]; simpl; auto.
      destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      destruct (yellow_eject s sy) as [s' x] eqn: Hyp.
      assert (wf_chain (Chain (Packet p pkt s') c)) as pc_wf.
      -- repeat split; auto.
         destruct c; auto.
         apply chain_green_to_regularity.
         destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']]; easy.
      -- split.
         ++ apply chain_green_to_green_or_yellow.
            apply ensure_green_green.
            apply pc_wf.
         ++ apply ensure_green_wf.
            apply pc_wf.
Qed.

Lemma eject_seq {A} (d : deque A) (d_wf : wf_deque d) :
  deque_seq d = match eject d with
                | None => []
                | Some (d', x) => deque_seq d' ++ [x]
                end.
Proof.
  destruct d. destruct c as [|_ [|B p pkt s] c]; simpl.
  - pose_destruct (buffer_eject_seq b) (buffer_eject b) Heq Hseq; auto.
    destruct p. easy.
  - easy.
  - destruct (are_green p s) as [[pg sg] | npsg]; simpl in *.
    + destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
      pose_destruct (green_eject_seq s sg) (green_eject s sg) Heq Hseq; simpl.
      rewrite ensure_green_seq; auto.
      hauto db:rlist.
    + destruct (are_yellow p s) as [[py sy] | npsy]; simpl.
      -- unfold eject_clause_2_clause_2_clause_1.
         remember (yellow_eject s sy) as tmp.
         pose (yellow_eject_seq s sy) as Hseq; rewrite <-Heqtmp in Hseq.
         destruct tmp as [x s'].
         rewrite deque_seq_equation_1.
         rewrite ensure_green_seq.
         ++ hauto db:rlist.
         ++ destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
            repeat split; auto.
            destruct c; auto.
            apply chain_green_to_regularity.
            destruct ccc as [col' [[[psg grcol'] | [_ gcol']] pktcol']]; easy.
      -- destruct d_wf as [[col [colgy [pgy sgy]]] [[psy pkt_wf] [ccc c_wf]]].
         destruct col; unfold green_or_yellow in colgy; easy.
Qed.
