From Equations Require Import Equations.

(* Helper definition that we use to ensure that our functions fully reduce when
   evaluated in Coq.

   [comp_eq eq] converts an equality proof [eq] that may contain opaque terms
   (e.g. defined with Qed) into an equality proof that reduces for sure (it
   directly computes whether the two integers are equal). (We thank Guillaume
   Melquiond for this trick.) *)
Equations comp_eq {A} {eq_dec: EqDec A} {x y : A} (eq : x = y) : x = y :=
comp_eq eq with eq_dec x y => {
  | left e => e;
  | right ne => _ }.
