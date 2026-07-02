From Coq Require Extraction.
From Deques Require Deque Deque_lvl Deque_plain.
From Deques Require Steque.
From Deques Require Cadeque.

From Coq Require ExtrOcamlBasic.

Separate Extraction
  Deque.empty
  Deque.push
  Deque.inject
  Deque.pop
  Deque.eject

  Deque_lvl.empty
  Deque_lvl.push
  Deque_lvl.inject
  Deque_lvl.pop
  Deque_lvl.eject

  Deque_plain.empty
  Deque_plain.push
  Deque_plain.inject
  Deque_plain.pop
  Deque_plain.eject

  Steque.empty
  Steque.push
  Steque.inject
  Steque.pop
  Steque.concat

  Cadeque.empty
  Cadeque.push
  Cadeque.inject
  Cadeque.pop
  Cadeque.eject
  Cadeque.concat
.
