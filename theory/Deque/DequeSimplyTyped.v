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
Definition colored_buffer {A} cc (b : buffer A) : Prop :=
  match b with
  | B0 | B5 _ _ _ _ _ => cc Red
  | B1 _ | B4 _ _ _ _ => cc Red \/ cc Yellow
  | B2 _ _ | B3 _ _ _ => True
  end.

(* A type for packets. *)
Inductive packet (A : Type) : Type -> Type :=
  | Hole : packet A A
  | Packet {B} : buffer A -> packet (A * A) B -> buffer A -> packet A B.
Arguments Hole {A}.
Arguments Packet {A B}.

(* A packet has the same color as its two buffers. *)
Definition colored_packet {A B} cc (p : packet A B) : Prop :=
  match p with
  | Hole => True
  | Packet bp _ bs => colored_buffer cc bp /\ colored_buffer cc bs
  end.

(* Well-formedness of packets.
   The first packet can have any color;
   the following packets must be yellow or empty. *)
Fixpoint wf_packet {A B} (deep : Prop) (p : packet A B) : Prop :=
  match p with
  | Hole => deep
  | Packet _ p' _ =>
    ((deep /\ colored_packet yellow p) \/ ~deep) /\ wf_packet True p'
  end.

(* A type for chains. *)
Inductive chain (A : Type) : Type :=
  | Ending : buffer A -> chain A
  | Chain {B} : packet A B -> chain B -> chain A.
Arguments Ending {A}.
Arguments Chain {A B}.

(* A chain has the same color as its first packet. *)
Definition colored_chain {A} cc (c : chain A) : Prop :=
  match c with
  | Ending _ => True
  | Chain p _ => colored_packet cc p
  end.

(* [regularity p] has type [color_constraint]. It is the color constraint
   imposed by the packet [p] on the chain that follows it. *)
Definition regularity {A B} (p : packet A B) (c : color) : Prop :=
  match p with
  | Hole => True
  | Packet _ _ _ =>
    (* A green packet must be followed with a green or red chain. *)
    (colored_packet green p /\ green_or_red c) \/
    (* A yellow or red packet must be followed with a green chain. *)
    green c
  end.

Fixpoint wf_chain {A} (c : chain A) : Prop :=
  match c with
  | Ending _ => True
  | Chain p c =>
    (* The top packet must be well-formed. *)
    wf_packet False p /\
    (* The packet [p] imposes a color constraint on the subchain [c]. *)
    colored_chain (regularity p) c /\
    (* The subchain must be well-formed. *)
    wf_chain c
  end.

(* A type decomposing buffers according to their number of elements.
   Buffers with 0 or 1 element are decomposed into [Underflow];
   buffers with 2 or 3 elements are decomposed into [Ok];
   buffers with 4 or 5 elements are decomposed into [Overflow]. *)
Inductive decompose (A : Type) : Type :=
  | Underflow : option A -> decompose A
  | Ok : forall b : buffer A, colored_buffer green b -> decompose A
  | Overflow :
      forall b : buffer A, colored_buffer green b -> A * A -> decompose A.
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
  | T c => colored_chain green_or_yellow c /\ wf_chain c
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
  | h: _ /\ _      |- _ => destruct h
  | h: exists x, _ |- _ => destruct h
  end.

Ltac deduce :=
  simpl in *; repeat unpack.

Ltac crunch :=
  lazymatch goal with
  | |- _ /\ _ =>
      split; crunch
  | _ =>
      try tauto;
      try congruence;
      try lia;
      eauto with easy
  end.

Ltac easy :=
  solve [ deduce; crunch ].

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

Definition falsity {A : Type} {p : Prop} (tp : p) : (p -> False) -> A.
Proof. intro Hf. exfalso. apply Hf. exact tp. Qed.

(* Pushes on a green buffer. *)
Equations green_push {A}
  (x : A) (b : buffer A) (cc : colored_buffer green b) : buffer A :=
green_push x (B2 a b)   _  := B3 x a b;
green_push x (B3 a b c) _  := B4 x a b c;
green_push _ _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_push_yellow {A}
  (x : A) (b : buffer A) (cc : colored_buffer green b) :
    colored_buffer yellow (green_push x b cc).
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - reflexivity.
  - simpl. right. reflexivity.
Qed.

Lemma green_push_seq {A}
  (x : A) (b : buffer A) (cc : colored_buffer green b) :
    buffer_seq (green_push x b cc) = [x] ++ buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Injects on a green buffer. *)
Equations green_inject {A}
  (b : buffer A) (x : A) (cc : colored_buffer green b) : buffer A :=
green_inject (B2 a b)   x _  := B3 a b x;
green_inject (B3 a b c) x _  := B4 a b c x;
green_inject _          _ cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_inject_yellow {A}
  (b : buffer A) (x : A) (cc : colored_buffer green b) :
    colored_buffer yellow (green_inject b x cc).
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - reflexivity.
  - simpl. right. reflexivity.
Qed.

Lemma green_inject_seq {A}
  (b : buffer A) (x : A) (cc : colored_buffer green b) :
    buffer_seq (green_inject b x cc) = buffer_seq b ++ [x].
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pops off a green buffer. *)
Equations green_pop {A}
  (b : buffer A) (cc : colored_buffer green b) : A * buffer A :=
green_pop (B2 a b)   _  := (a, B1 b);
green_pop (B3 a b c) _  := (a, B2 b c);
green_pop _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_pop_yellow {A}
  (b : buffer A) (cc : colored_buffer green b) :
    colored_buffer yellow (snd (green_pop b cc)).
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - simpl. right. reflexivity.
  - reflexivity.
Qed.

Lemma green_pop_seq {A} (b : buffer A) (cc : colored_buffer green b) :
    let '(x, b') := green_pop b cc in
    [x] ++ buffer_seq b' = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Ejects off a green buffer. *)
Equations green_eject {A}
  (b : buffer A) (cc : colored_buffer green b) : buffer A * A :=
green_eject (B2 a b)   _  := (B1 a, b);
green_eject (B3 a b c) _  := (B2 a b, c);
green_eject _          cc := falsity cc _.
Next Obligation. easy. Qed.
Next Obligation. easy. Qed.

Lemma green_eject_yellow {A}
  (b : buffer A) (cc : colored_buffer green b) :
    colored_buffer yellow (fst (green_eject b cc)).
Proof.
  destruct b; try (apply (falsity cc); simpl; easy).
  - simpl. right. reflexivity.
  - reflexivity.
Qed.

Lemma green_eject_seq {A} (b : buffer A) (cc : colored_buffer green b) :
    let '(b', x) := green_eject b cc in
    buffer_seq b' ++ [x] = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pushes on a yellow buffer. *)
Equations yellow_push {A}
  (x : A) (b : buffer A) (cc : colored_buffer yellow b) : buffer A :=
yellow_push x (B1 a)       _  := B2 x a;
yellow_push x (B2 a b)     _  := B3 x a b;
yellow_push x (B3 a b c)   _  := B4 x a b c;
yellow_push x (B4 a b c d) _  := B5 x a b c d;
yellow_push x _            cc := falsity cc _.

Lemma yellow_push_seq {A}
  (x : A) (b : buffer A) (cc : colored_buffer yellow b) :
    buffer_seq (yellow_push x b cc) = [x] ++ buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Injects on a yellow buffer. *)
Equations yellow_inject {A}
  (b : buffer A) (x : A) (cc : colored_buffer yellow b) : buffer A :=
yellow_inject (B1 a)       x _  := B2 a x;
yellow_inject (B2 a b)     x _  := B3 a b x;
yellow_inject (B3 a b c)   x _  := B4 a b c x;
yellow_inject (B4 a b c d) x _  := B5 a b c d x;
yellow_inject _            x cc := falsity cc _.

Lemma yellow_inject_seq {A}
  (b : buffer A) (x : A) (cc : colored_buffer yellow b) :
    buffer_seq (yellow_inject b x cc) = buffer_seq b ++ [x].
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Pops off a yellow buffer. *)
Equations yellow_pop {A}
  (b : buffer A) (cc : colored_buffer yellow b) : A * buffer A :=
yellow_pop (B1 a)       _  := (a, B0);
yellow_pop (B2 a b)     _  := (a, B1 b);
yellow_pop (B3 a b c)   _  := (a, B2 b c);
yellow_pop (B4 a b c d) _  := (a, B3 b c d);
yellow_pop _            cc := falsity cc _.

Lemma yellow_pop_seq {A} (b : buffer A) (cc : colored_buffer yellow b) :
    let '(x, b') := yellow_pop b cc in
    [x] ++ buffer_seq b' = buffer_seq b.
Proof.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Ejects off a yellow buffer. *)
Equations yellow_eject {A}
  (b : buffer A) (cc : colored_buffer yellow b) : buffer A * A :=
yellow_eject (B1 a)       _  := (B0, a);
yellow_eject (B2 a b)     _  := (B1 a, b);
yellow_eject (B3 a b c)   _  := (B2 a b, c);
yellow_eject (B4 a b c d) _  := (B3 a b c, d);
yellow_eject _            cc := falsity cc _.

Lemma yellow_eject_seq {A} (b : buffer A) (cc : colored_buffer yellow b) :
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

Lemma wf_buffer_push {A} (x : A) (b : buffer A) : wf_chain (buffer_push x b).
Proof.
  destruct b; simpl; try reflexivity.
  split; split; try reflexivity.
  right. intro. congruence.
Qed.

Lemma buffer_push_green {A} (x : A) (b : buffer A) :
  colored_chain green (buffer_push x b).
Proof.
  destruct b; simpl; try reflexivity.
  split; reflexivity.
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

Lemma wf_buffer_inject {A} (b : buffer A) (x : A) :
  wf_chain (buffer_inject b x).
Proof.
  destruct b; simpl; try reflexivity.
  split; split; try reflexivity.
  right. intro. congruence.
Qed.

Lemma buffer_inject_green {A} (b : buffer A) (x : A) :
  colored_chain green (buffer_inject b x).
Proof.
  destruct b; simpl; try reflexivity.
  split; reflexivity.
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
  colored_buffer green (prefix23 o p).
Proof. destruct o; destruct p; reflexivity. Qed.

Lemma prefix23_seq {A} (o : option A) (p : A * A) :
  buffer_seq (prefix23 o p) = option_seq o ++ pair_seq p.
Proof. destruct o; destruct p; reflexivity. Qed.

(* Merges a pair and an option to create a green buffer. *)
Equations suffix23 {A} (p : A * A) (o : option A) : buffer A :=
suffix23 (a, b)  None    := B2 a b;
suffix23 (a, b) (Some c) := B3 a b c.

Lemma suffix23_green {A} (p : A * A) (o : option A) :
  colored_buffer green (suffix23 p o).
Proof. destruct p; destruct o; reflexivity. Qed.

Lemma suffix23_seq {A} (p : A * A) (o : option A) :
  buffer_seq (suffix23 p o) = pair_seq p ++ option_seq o.
Proof. destruct p; destruct o; reflexivity. Qed.

(* Merges an element and an option to create a yellow buffer. *)
Equations suffix12 {A} (x : A) (o : option A) : buffer A :=
suffix12 x  None    := B1 x;
suffix12 x (Some y) := B2 x y.

Lemma suffix12_yellow {A} (x : A) (o : option A) :
  colored_buffer yellow (suffix12 x o).
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
#[export] Hint Rewrite flattenp_app : rlist.

Lemma green_buffer_to_yellow_buffer {A} (b : buffer A) :
  colored_buffer green b -> colored_buffer yellow b.
Proof.
  intro cc.
  destruct b; try (apply (falsity cc); simpl; easy); reflexivity.
Qed.

(* Takes a buffer of any color and a green buffer of pairs, rearranges elements
   contained in them, and returns a green buffer and a yellow buffer of pairs.
   The order of elements is preserved. *)
Equations green_prefix_concat {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer green b2) :
  buffer A * buffer (A * A) :=
green_prefix_concat b1 b2 cc with prefix_decompose b1 => {
  | Underflow opt with green_pop b2 cc => {
    | (ab, b) with prefix23 opt ab => {
      | prefix := (prefix, b) } };
  | Ok b _ := (b, b2);
  | Overflow b _ ab with green_push ab b2 cc => {
    | suffix := (b, suffix) } }.

Lemma green_prefix_concat_green_yellow {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer green b2) :
    let '(b1', b2') := green_prefix_concat b1 b2 cc in
    colored_buffer green b1' /\ colored_buffer yellow b2'.
Proof.
  unfold green_prefix_concat. remember (prefix_decompose b1) as d.
  destruct d; simpl.
  - remember (green_pop b2 cc) as gp; destruct gp; simpl.
    split.
    + apply prefix23_green.
    + replace (b) with (snd (green_pop b2 cc))
        by (rewrite <-Heqgp; reflexivity).
      apply green_pop_yellow.
  - split.
    + assumption.
    + apply green_buffer_to_yellow_buffer; assumption.
  - split.
    + assumption.
    + apply green_push_yellow.
Qed.

Lemma green_prefix_concat_seq {A}
  (b1 : buffer A) (b2 : buffer (A * A)) (cc : colored_buffer green b2) :
    let '(b1', b2') := green_prefix_concat b1 b2 cc in
    buffer_seq b1  ++ flattenp (buffer_seq b2) =
    buffer_seq b1' ++ flattenp (buffer_seq b2').
Proof.
  unfold green_prefix_concat.
  remember (prefix_decompose b1) as d.
  pose (prefix_decompose_seq b1) as Hpds; rewrite <-Heqd in Hpds; simpl in Hpds.
  destruct d; simpl in *.
  - remember (green_pop b2 cc) as gp; destruct gp; simpl.
    pose (green_pop_seq b2 cc) as Hgps; rewrite <-Heqgp in Hgps.
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
