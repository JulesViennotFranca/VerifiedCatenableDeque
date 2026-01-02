From Coq Require Import Lia.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.

(* This file is a copy of Core.v where we develop an alternate approach. Instead
   of using indexed types [packet c] and [chain c] where [c] is a color, we use
   simple (non-indexed) types [packet] and [chain] and we define several
   well-formedness predicates ([wf_packet], [colored_packet], [regularity],
   [wf_chain], [wf_number]) a posteriori. *)

(* +------------------------------------------------------------------------+ *)
(* |                                Colors                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Colors. *)
Inductive color := Red | Yellow | Green.

(* A color constraint accepts or rejects each color.
   It can also be thought of as a set of colors. *)

Definition color_constraint :=
  color -> Prop.

Implicit Type cc : color_constraint.

Definition green_or_yellow c :=
  c = Green \/ c = Yellow.

Hint Unfold green_or_yellow : easy.
  (* this helps [eauto with easy], which the tactic [easy] uses *)

Lemma yellow_is_not_green_or_red :
  ~ (Yellow = Green \/ Yellow = Red).
Proof. intros [|]; congruence. Qed.

Lemma red_is_not_green_or_yellow :
  green_or_yellow Red -> False.
Proof. intros [|]; congruence. Qed.

#[local] Hint Resolve
  yellow_is_not_green_or_red
  red_is_not_green_or_yellow
: easy.
  (* this helps [eauto with easy], which the tactic [easy] uses *)

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
(* |                                 Types                                  | *)
(* +------------------------------------------------------------------------+ *)

(* A packet is a list of digits, where a digit is G, Y, or R. *)
Inductive packet : Type :=
  | Hole   : packet
  | GDigit : packet -> packet
  | YDigit : packet -> packet
  | RDigit : packet -> packet.

(* Well-formedness of packets.
   The first digit of a packet can be G, Y, R;
   the following digits must be Y;
   a packet cannot be empty. *)
Fixpoint wf_packet (deep : Prop) (p : packet) : Prop :=
  match p with
  | Hole =>
      deep
  | GDigit p
  | RDigit p =>
      ~deep /\ wf_packet True p
  | YDigit p =>
      wf_packet True p
  end.

(* Coloring of packets.
   The color of a packet is the color of its first digit. *)

Definition colored_packet cc (p : packet) : Prop :=
  match p with
  | Hole =>
      True (* irrelevant *)
  | GDigit _ =>
      cc Green
  | RDigit _ =>
      cc Red
  | YDigit _ =>
      cc Yellow
  end.

(* A chain is a list of packets. *)
Inductive chain : Type :=
  | Empty : chain
  | Chain : packet -> chain -> chain.

(* [regularity p] has type [color_constraint]. It is the color constraint
   imposed by the packet [p] on the chain that follows it. *)
Definition regularity (p : packet) (c : color) : Prop :=
  match p with
  | Hole =>
      True (* irrelevant *)
  | GDigit _ =>
      (* A green packet must be followed with a green or red chain. *)
      c = Green \/ c = Red
  | YDigit _ =>
      (* A yellow packet must be followed with a green chain. *)
      c = Green
  | RDigit _ =>
      (* A red packet must be followed with a green chain. *)
      c = Green
  end.

Hint Unfold regularity : easy.
  (* this helps [eauto with easy], which the tactic [easy] uses *)

(* Well-formedness and coloring of chains. *)
Fixpoint wf_chain cc (c : chain) : Prop :=
  match c with
  | Empty =>
      True
  | Chain p c =>
      (* Every packet must be well-formed. *)
      wf_packet False p /\
      (* The color of a chain is the color of its first packet.
         Thus the color constraint [cc] bears on the packet [p]. *)
      colored_packet cc p /\
      (* The packet [p] imposes a color constraint on the subchain [c]. *)
      wf_chain (regularity p) c
  end.

(* A number is a green or yellow chain. *)
Definition wf_number (c : chain) : Prop :=
  wf_chain green_or_yellow c.

(* +------------------------------------------------------------------------+ *)
(* |                                 Models                                 | *)
(* +------------------------------------------------------------------------+ *)

(* Model functions are transparent. *)
Set Equations Transparent.

(* Returns the natural number associated to a packet, provided the natural
   number associated to its hole. *)
Equations packet_nat : packet -> nat -> nat :=
packet_nat Hole n := n;
packet_nat (GDigit pkt) n := 0 + 2 * packet_nat pkt n;
packet_nat (YDigit pkt) n := 1 + 2 * packet_nat pkt n;
packet_nat (RDigit pkt) n := 2 + 2 * packet_nat pkt n.

(* Returns the natural number associated to a chain. *)
Equations chain_nat : chain -> nat :=
chain_nat Empty := 0;
chain_nat (Chain pkt c) := packet_nat pkt (chain_nat c).

(* Returns the natural number associated to a number. *)
Notation number_nat := chain_nat.

Unset Equations Transparent.

(* +------------------------------------------------------------------------+ *)
(* |                                  Core                                  | *)
(* +------------------------------------------------------------------------+ *)

(* Instead of defining [red_of_green] first and [ensure_green] in a second step,
   as in Core.v, it seems easier here to define [ensure_green] directly, because
   we need to cover all cases anyway. *)

Definition ensure_green (c : chain) : chain :=
  match c with
  | Chain (RDigit Hole) Empty =>
      Chain (GDigit (YDigit Hole)) Empty
  | Chain (RDigit Hole) (Chain (GDigit body) c) =>
      Chain (GDigit (YDigit body)) c
  | Chain (RDigit (YDigit body)) c =>
      Chain (GDigit Hole) (Chain (RDigit body) c)
  | _ =>
      c
  end.

(* If [c] is red or green (that is, not yellow) then [ensure_green c] is a
   well-formed green chain. *)

Lemma wf_chain_ensure_green cc cc' c :
  wf_chain cc c ->
  ~ cc Yellow ->
  cc' Green ->
  wf_chain cc' (ensure_green c).
Proof.
  (* The proof is a bit painful, as we must repeat the case analysis
     that exists in the code. *)
  intros Hc Hcc Hcc'.
  destruct c; try easy.
  destruct p; try easy.
  destruct p; try easy.
  destruct c; try easy.
  destruct p; try easy.
Qed.

(* A special case of the previous lemma, where [cc] is "is red". *)

Lemma wf_chain_ensure_green' cc' c :
  wf_chain (fun c => c = Red) c ->
  cc' Green ->
  wf_chain cc' (ensure_green c).
Proof.
  eauto using wf_chain_ensure_green with easy.
Qed.

(* The chain [c] and the chain [ensure_green c] represent the same
   natural number. *)

Lemma ensure_green_correct c :
  chain_nat (ensure_green c) = chain_nat c.
Proof.
  (* The proof is a bit painful, as we must repeat the case analysis
     that exists in the code. *)
  destruct c; try easy.
  destruct p; try easy.
  destruct p; try easy.
  destruct c; try easy.
  destruct p; try easy.
Qed.

(* +------------------------------------------------------------------------+ *)
(* |                               Operation                                | *)
(* +------------------------------------------------------------------------+ *)

(* [succ] increments a natural number. *)

Definition succ (c : chain) : chain :=
  match c with
  | Empty =>
      Chain (YDigit Hole) Empty
  | Chain (GDigit body) c =>
      let c' := ensure_green c in
      Chain (YDigit body) c'
  | Chain (YDigit body) c =>
      ensure_green (Chain (RDigit body) c)
  | Chain Hole _
  | Chain (RDigit _) _ =>
      (* cannot happen if [c] is well-formed *)
      c
  end.

(* [succ] preserves well-formedness. *)

Lemma wf_number_succ c :
  wf_number c ->
  wf_number (succ c).
Proof.
  unfold wf_number. intro Hc.
  destruct c; try easy.
  destruct p; try easy.
  { (* Case 2: green digit. *)
    unfold succ. deduce. crunch. eapply wf_chain_ensure_green; easy. }
  { (* Case 3: yellow digit. *)
    unfold succ. eapply wf_chain_ensure_green'; easy. }
Qed.

(* [succ] is correct; it corresponds to incrementation. *)

Lemma succ_correct c :
  wf_number c ->
  number_nat (succ c) = S (number_nat c).
Proof.
  unfold wf_number.
  intro Hc.
  destruct c; try easy.
  destruct p; try solve [ easy | exfalso; easy ].
  { (* Case 2: green digit. *)
    unfold succ. simpl number_nat.
    rewrite ensure_green_correct. reflexivity. }
  { (* Case 3: yellow digit. *)
    unfold succ. rewrite ensure_green_correct. reflexivity. }
Qed.

(* In the two proofs above, we exploit the properties of [ensure_green],
   either via [eapply] or via [rewrite]. Perhaps, with some work, we could
   automate these steps completely, so that we can just write [easy] and
   avoid the need to think. Even then, we would still need to perform a case
   analysis by hand, mimicking the structure of the code. In comparison, the
   proofs about [succ] in Core.v are fully automated. *)

(* +------------------------------------------------------------------------+ *)
(* |                           Back to ensure_green                         | *)
(* +------------------------------------------------------------------------+ *)

(* Notation for dependent types hiding the property on [x]. *)
Notation "? x" := (@exist _ _ x _) (at level 100).

(* Above, we have defined [ensure_green] as a function of simple type
   [chain -> chain], and we have proved a posteriori that it preserves
   well-formedness and preserves the chain's model. *)

(* Here we try an alternative approach, which is to use Equations and
   to decorate the definition of [ensure_green] with a precondition
   and a postcondition. This removes the need to perform a case analysis
   that mimics the code (Equations does it for us). Furthermore, if we
   provide a well-chosen tactic, then all proof obligations are proved
   automatically. *)

Ltac default_obligation_tactic :=
  (* Equations's default tactic. *)
  simpl in *;
  Tactics.program_simplify;
  CoreTactics.equations_simpl;
  try Tactics.program_solve_wf.

Obligation Tactic :=
  try solve [ default_obligation_tactic |
    (* Our own tactic. *)
    unfold wf_number in *;
    repeat intros; simpl in *; repeat unpack; crunch
  ].

(* Here is [ensure_green] with a precondition and a postcondition. *)

Equations ensure_green' (c : chain)
  (* Precondition: the chain [c] is well-formed and not yellow. *)
  (_ : exists cc, wf_chain cc c /\ ~ cc Yellow)
: { c' : chain |
  (* Postcondition: the chain [c'] is well-formed, green,
     and represents the same number as the chain [c]. *)
    (forall cc', cc' Green -> wf_chain cc' c') /\
    chain_nat c' = chain_nat c
  } :=
ensure_green' (Chain (RDigit Hole) Empty) _ :=
  ? Chain (GDigit (YDigit Hole)) Empty ;
ensure_green' (Chain (RDigit Hole) (Chain (GDigit body) c)) _ :=
  ? Chain (GDigit (YDigit body)) c ;
ensure_green' (Chain (RDigit (YDigit body)) c) _ :=
  ? Chain (GDigit Hole) (Chain (RDigit body) c) ;
ensure_green' (c) _ :=
  ? c.

(* Here is [succ] with a precondition and a postcondition. *)

Equations succ' (c : chain)
  (* Precondition: [c] is well-formed. *)
  (_ : wf_number c)
: { c' : chain |
  (* Postcondition: [c'] is well-formed
     and represents the successor of [c]. *)
    wf_number c' /\
    number_nat c' = S (number_nat c)
  } :=
succ' Empty _ :=
  ? Chain (YDigit Hole) Empty ;
succ' (Chain (GDigit body) c) _
  with ensure_green' c _ => { | ? c' :=
  ? Chain (YDigit body) c' } ;
succ' (Chain (YDigit body) c) _
  with ensure_green' (Chain (RDigit body) c) _ => { | ? c' :=
  ? c' } ;
succ' _ _ :=
  (* We shall prove that this default branch is dead. *)
  (* Equations does not allow us to just remove it,
     although it could. *)
  _.

Next Obligation.
  (* Case 3, obligation: prove that [Chain (RDigit body) c]
     is well-formed and not yellow. This is the precondition
     of [ensure_green']. *)
  unfold wf_number in *.
  repeat intros.
  exists (fun c => c = Red).
  easy.
Qed.

Next Obligation.
  (* Case 4, obligation: prove that this branch is dead. *)
  unfold wf_number in *.
  repeat intros.
  exfalso.
  easy.
Qed.

(* With some more work on our tactics, we could automate also
   the above two proof obligations. *)

(* We conclude that: 1- it is possible to work with simple (non-indexed)
   types; 2- regardless of whether indexed types or simple types are used, it
   is helpful to use Equations so as to avoid manually writing a case analysis
   that mimics the structure of the code; 3- regardless of whether indexed
   types or simple types are used, it appears feasible to automate all proof
   obligations, provided sufficient effort is put into fine-tuning tactics. *)
