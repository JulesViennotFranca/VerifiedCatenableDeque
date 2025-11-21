From Stdlib Require Import List.
Import ListNotations.
From Deques.Cadeque Require Import Types Models Core Operations.
From Deques Require Import Signatures.

(* We check that the types and operations defined in this directory
   satisfy (the two variants of) the signature of catenable deques,
   as defined (one level up) in Signatures.v. *)

(* The intrinsic signature. *)

Module I : CADEQUE_INTRINSIC.
  Definition cadeque := Types.cadeque.
  Definition model   := @Models.cadeque_seq.
  Definition empty   := @Operations.D.empty.
  Definition push    := @Operations.D.push.
  Definition inject  := @Operations.D.inject.
  Definition pop     := @Operations.D.pop.
  Definition eject   := @Operations.D.eject.
  Definition concat  := @Operations.D.concat.
End I.

(* The extrinsic signature is satisfied by splitting up each operation
   in two components: the operation itself and its proof of correctness.
   The two pair projections are [proj1_sig] and [proj2_sig]. *)

Module E : CADEQUE_EXTRINSIC.

  Definition cadeque   :=
    Types.cadeque.

  Definition model {A} :=
    (@Models.cadeque_seq A).

  Definition empty {A} :=
    proj1_sig (@Operations.D.empty A).
  Definition empty_correct {A} :=
    proj2_sig (@Operations.D.empty A).

  Definition push {A} (x : A) (d : cadeque A) :=
    proj1_sig (Operations.D.push x d).
  Definition push_correct {A : Type} (x : A) (d : cadeque A) :=
    proj2_sig (Operations.D.push x d).

  Definition inject {A} (d : cadeque A) (x : A) :=
    proj1_sig (Operations.D.inject d x).
  Definition inject_correct {A} (d : cadeque A) (x : A) :=
    proj2_sig (Operations.D.inject d x).

  Definition pop {A} (d : cadeque A) :=
    proj1_sig (Operations.D.pop d).
  Definition pop_correct {A} (d : cadeque A) :=
    proj2_sig (Operations.D.pop d).

  Definition eject {A} (d : cadeque A) :=
    proj1_sig (Operations.D.eject d).
  Definition eject_correct {A} (d : cadeque A) :=
    proj2_sig (Operations.D.eject d).

  Definition concat {A} (d1 d2 : cadeque A) :=
    proj1_sig (Operations.D.concat d1 d2).
  Definition concat_correct {A : Type} (d1 d2 : cadeque A) :=
    proj2_sig (Operations.D.concat d1 d2).

End E.

(* Finally, check that no axioms were used, no proofs were admitted, etc. *)

(* I don't know how to apply Print Assumptions to a module,
   so I apply it to a tuple which contains every identifier
   in the module I and E. *)

(* This should print [Closed under the global context]. *)

Definition everything := (
  @I.cadeque,
  @I.model,
  @I.empty,
  @I.push,
  @I.pop,
  @I.inject,
  @I.eject,
  @I.concat,
  @E.cadeque,
  @E.model,
  @E.empty,
  @E.empty_correct,
  @E.push,
  @E.push_correct,
  @E.pop,
  @E.pop_correct,
  @E.inject,
  @E.inject_correct,
  @E.eject,
  @E.eject_correct,
  @E.concat,
  @E.concat_correct
).

Print Assumptions everything.
