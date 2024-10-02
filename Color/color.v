From Equations Require Import Equations.
Require Import Coq.Program.Equality.

Module GYR.
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
End GYR.

Module GYOR.
  (* Defining some hues. *)
  Inductive green_hue  := SomeGreen  | NoGreen.
  Inductive yellow_hue := SomeYellow | NoYellow.
  Inductive orange_hue := SomeOrange | NoOrange.
  Inductive red_hue    := SomeRed    | NoRed.

  (* Defining colors type. *)
  Inductive color :=
    Mix : green_hue -> yellow_hue -> orange_hue -> red_hue -> color.

  (* Deriving the [NoConfusion] trait needed by [Equations]. *)
  Derive NoConfusion for green_hue.
  Derive NoConfusion for yellow_hue.
  Derive NoConfusion for orange_hue.
  Derive NoConfusion for red_hue.
  Derive NoConfusion for color.

  (* Defining colors. *)
  Notation green := (Mix SomeGreen NoYellow NoOrange NoRed).
  Notation yellow := (Mix NoGreen SomeYellow NoOrange NoRed).
  Notation orange := (Mix NoGreen NoYellow SomeOrange NoRed).
  Notation red := (Mix NoGreen NoYellow NoOrange SomeRed).
  Notation uncolored := (Mix NoGreen NoYellow NoOrange NoRed).

  (* An instance of [UIP] for colors is needed in Cdeque/core.v. *)
  Instance UIP_color : UIP color.
  Proof.
    intros C1 C2 p q.
    destruct C1 as [G1 Y1 O1 R1].
    destruct C2 as [G2 Y2 O2 R2].
    pose (projg := fun '(Mix g _ _ _) => g).
    pose (projy := fun '(Mix _ y _ _) => y).
    pose (projo := fun '(Mix _ _ o _) => o).
    pose (projr := fun '(Mix _ _ _ r) => r).
    assert (G1 = G2) as Hg. { apply (f_equal projg p). }
    assert (Y1 = Y2) as Hy. { apply (f_equal projy p). }
    assert (O1 = O2) as Ho. { apply (f_equal projo p). }
    assert (R1 = R2) as Hr. { apply (f_equal projr p). }
    destruct Hg. destruct Hy. destruct Ho. destruct Hr.
    dependent destruction p.
    dependent destruction q.
    reflexivity.
  Qed.
End GYOR.