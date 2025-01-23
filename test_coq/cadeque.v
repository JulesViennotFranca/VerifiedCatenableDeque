From Cadeque.cadeque Require Import types operations.

Definition test3 : cadeque nat :=
  let d := proj1_sig D.empty in
  let d := proj1_sig (D.push 0 d) in
  let d := proj1_sig (D.push 1 d) in
  let d := proj1_sig (D.push 2 d) in
  d.

Eval vm_compute in test3.

Definition test6 : cadeque nat :=
  proj1_sig (D.concat test3 test3).

Eval vm_compute in test6.

Definition test12 : cadeque nat :=
  proj1_sig (D.concat test6 test6).

Eval vm_compute in test12.
