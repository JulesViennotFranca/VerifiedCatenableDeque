From Coq Require Import Arith.
From Equations Require Import Equations.

(* Helper definition that we use to ensure that our functions fully reduce when
   evaluated in Coq.

   [comp_eq eq] converts an equality proof [eq] that may contain opaque terms
   (e.g. defined with Qed) into an equality proof that reduces for sure (it
   directly computes whether the two integers are equal). (We thank Guillaume
   Melquiond for this trick.) *)
Equations comp_eq {n1 n2 : nat} (eq : n1 = n2) : n1 = n2 :=
comp_eq eq with Nat.eq_dec n1 n2 => {
  | left e => e;
  | right ne => False_rect _ _ }.
