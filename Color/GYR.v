From Equations Require Import Equations.

(* Defining some hues. *)
Inductive green_hue  := SomeGreen  | NoGreen.
Inductive yellow_hue := SomeYellow | NoYellow.
Inductive red_hue    := SomeRed    | NoRed.

(* Defining colors type. *)
Inductive color := Mix : green_hue -> yellow_hue -> red_hue -> color.

(* Deriving the [NoConfusion] trait needed by [Equations]. *)
Derive NoConfusion for green_hue.
Derive NoConfusion for yellow_hue.
Derive NoConfusion for red_hue.
Derive NoConfusion for color.

(* Defining colors. *)
Notation green := (Mix SomeGreen NoYellow NoRed).
Notation yellow := (Mix NoGreen SomeYellow NoRed).
Notation red := (Mix NoGreen NoYellow SomeRed).
Notation uncolored := (Mix NoGreen NoYellow NoRed).