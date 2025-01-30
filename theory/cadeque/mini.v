From Coq Require Import List.
Import ListNotations.
From Equations Require Import Equations.
Require Import Coq.Program.Equality.
From Hammer Require Import Tactics.
From AAC_tactics Require Import AAC.
From AAC_tactics Require Import Instances.
Import Instances.Lists.

From Theory.color Require Import GYOR.
From Theory.cadeque Require Import buffer types models.

Axiom cheat : forall {A}, A.

Notation "? x" := (@exist _ _ x _) (at level 100).

Axiom ensure_green_prefix : forall {A lvl q ck}
  (p : prefix A lvl (5 + q))
  (child : chain A (S lvl) (S ck) only green green),
{ '(pgreen, child') : green_buffer A lvl * semi_cadeque A (S lvl) |
    prefix_seq p ++ chain_seq child =
    green_buffer_seq pgreen ++ semi_cadeque_seq child' }.

Equations green_of_red_left {A hlvl tlvl hk ck}
  (bd : body A hlvl tlvl hk left)
  (red : node A tlvl (S ck) left red)
  (child : chain A (S tlvl) (S ck) only green green) :
  { c : chain A hlvl single hk green green |
    chain_seq c = body_seq bd (node_seq red (chain_seq child)) } :=
green_of_red_left bd (Left Rc p s) child
  with ensure_green_prefix p child => {
    | ? (Gbuf p1, Semi Empty) :=
      ? Single G (Packet bd (Left Ec p1 s)) Empty;
    | ? (Gbuf p1, Semi child1) := cheat }.
Next Obligation. exact cheat. Qed.
Next Obligation.
  rewrite app_nil_r in y.
  rewrite <-y.
  rewrite <-app_assoc.
  reflexivity.
Qed.
