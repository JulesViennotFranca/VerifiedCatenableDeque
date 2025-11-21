From Equations Require Import Equations.
Require Import Stdlib.Program.Equality.

(* Defining some hues. *)
Inductive green_hue  := SomeGreen  | NoGreen.
Inductive yellow_hue := SomeYellow | NoYellow.
Inductive orange_hue := SomeOrange | NoOrange.
Inductive red_hue    := SomeRed    | NoRed.

(* Defining colors type. *)
Inductive color :=
  Mix : green_hue -> yellow_hue -> orange_hue -> red_hue -> color.

(* Deriving the [NoConfusion] and [EqDec] typeclasses used by [Equations]. *)
Derive NoConfusion EqDec for green_hue.
Derive NoConfusion EqDec for yellow_hue.
Derive NoConfusion EqDec for orange_hue.
Derive NoConfusion EqDec for red_hue.
Derive NoConfusion for color.

(* [Derive EqDec for color] raises a warning, so we prove it manually... *)
Instance color_eqdec : EqDec color.
Proof.
  intros [g y o r] [g' y' o' r'].
  destruct (green_hue_eqdec g g') as [->|]; [| right; congruence].
  destruct (yellow_hue_eqdec y y') as [->|]; [| right; congruence].
  destruct (orange_hue_eqdec o o') as [->|]; [| right; congruence].
  destruct (red_hue_eqdec r r') as [->|]; [| right; congruence].
  left; reflexivity.
Defined.

(* Defining colors. *)
Notation green := (Mix SomeGreen NoYellow NoOrange NoRed).
Notation yellow := (Mix NoGreen SomeYellow NoOrange NoRed).
Notation orange := (Mix NoGreen NoYellow SomeOrange NoRed).
Notation red := (Mix NoGreen NoYellow NoOrange SomeRed).
Notation uncolored := (Mix NoGreen NoYellow NoOrange NoRed).
