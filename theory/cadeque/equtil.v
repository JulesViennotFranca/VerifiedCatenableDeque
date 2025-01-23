From Coq Require Import Arith.

(* Helper definition that we use to ensure that our functions fully reduce when
   evaluated in Coq.

   [comp_eq eq] converts an equality proof [eq] that may contain opaque terms
   (e.g. defined with Qed) into an equality proof that reduces for sure (it
   directly computes whether the two integers are equal). (We thank Guillaume
   Melquiond for this trick.) *)
Definition comp_eq {n1 n2: nat} (eq: n1 = n2): n1 = n2.
Proof.
  destruct (Nat.eq_dec n1 n2) as [e|ne].
  - clear eq. destruct e. exact eq_refl.
  - exfalso. apply ne, eq.
Defined.
(* [Print comp_eq] can be used to check that [comp_eq] does not uses its [eq]
   argument in the first case. *)
