From Cadeque.deque Require Import deque.

Definition test : option (deque nat * nat) :=
  let d := proj1_sig empty in
  let d := proj1_sig (push 2 d) in
  let d := proj1_sig (push 1 d) in
  let d := proj1_sig (push 0 d) in
  let d := proj1_sig (inject d 3) in
  let d := proj1_sig (inject d 4) in
  let d := proj1_sig (inject d 5) in
  match proj1_sig (pop d) with
  | Some (_, d) => proj1_sig (eject d)
  | None => None
  end.

Eval vm_compute in test.

From Cadeque.deque Require Import deque_lvl.

Definition test_lvl : option (deque nat * nat) :=
  let d := proj1_sig empty in
  let d := proj1_sig (push 2 d) in
  let d := proj1_sig (push 1 d) in
  let d := proj1_sig (push 0 d) in
  let d := proj1_sig (inject d 3) in
  let d := proj1_sig (inject d 4) in
  let d := proj1_sig (inject d 5) in
  match proj1_sig (pop d) with
  | Some (_, d) => proj1_sig (eject d)
  | None => None
  end.

Eval vm_compute in test.

From Cadeque.deque Require Import deque_size.

Definition test_size : deque nat 4 * nat :=
  let d := proj1_sig empty in
  let d := proj1_sig (push 2 d) in
  let d := proj1_sig (push 1 d) in
  let d := proj1_sig (push 0 d) in
  let d := proj1_sig (inject d 3) in
  let d := proj1_sig (inject d 4) in
  let d := proj1_sig (inject d 5) in
  let '(_, d) := proj1_sig (pop d) in
  proj1_sig (eject d).

Eval vm_compute in test.
