From Deques.Steque Require Import Steque.

Definition test : option (nat * steque nat) :=
  let d := proj1_sig empty in
  let d := proj1_sig (push 2 d) in
  let d := proj1_sig (push 1 d) in
  let d := proj1_sig (push 0 d) in
  let d := proj1_sig (inject d 3) in
  let d := proj1_sig (inject d 4) in
  let d := proj1_sig (inject d 5) in
  let d := proj1_sig (concat d d) in
  let d := proj1_sig (concat d d) in
  proj1_sig (pop d).

Eval vm_compute in test.
