From Coq Require Import List.
Import ListNotations.
From Deques.Cadeque Require Import Types Models Cadeque.

Definition test3 : cadeque nat :=
  let d := proj1_sig empty in
  let d := proj1_sig (push 0 d) in
  let d := proj1_sig (push 1 d) in
  let d := proj1_sig (push 2 d) in
  d.

Eval vm_compute in test3.

Goal cadeque_seq test3 = [2; 1; 0].
Proof. reflexivity. Qed.

Definition test6 : cadeque nat :=
  proj1_sig (concat test3 test3).

Eval vm_compute in test6.

Goal cadeque_seq test6 = [2; 1; 0; 2; 1; 0].
Proof. reflexivity. Qed.

Definition test12 : cadeque nat :=
  proj1_sig (concat test6 test6).

Eval vm_compute in test12.

Goal cadeque_seq test12 = [2; 1; 0; 2; 1; 0; 2; 1; 0; 2; 1; 0].
Proof. reflexivity. Qed.

Definition two : nat :=
  match proj1_sig (pop test12) with
  | Some (x, _) => x
  | None        => 42 (* cannot happen *)
  end.

Goal two = 2.
Proof. reflexivity. Qed.

Definition zero : nat :=
  match proj1_sig (eject test12) with
  | Some (_, x) => x
  | None        => 42 (* cannot happen *)
  end.

Goal zero = 0.
Proof. reflexivity. Qed.
