From Coq Require Extraction.
From Deques Require deque deque_lvl deque_plain.
From Deques Require steque.
From Deques Require cadeque.operations.

Definition library := (
  @deque.empty,
  @deque.push,
  @deque.inject,
  @deque.pop,
  @deque.eject,


  @deque_lvl.empty,
  @deque_lvl.push,
  @deque_lvl.inject,
  @deque_lvl.pop,
  @deque_lvl.eject,

  @deque_plain.empty,
  @deque_plain.push,
  @deque_plain.inject,
  @deque_plain.pop,
  @deque_plain.eject,

  @steque.empty,
  @steque.push,
  @steque.inject,
  @steque.pop,
  @steque.concat,

  @cadeque.operations.D.empty,
  @cadeque.operations.D.push,
  @cadeque.operations.D.inject,
  @cadeque.operations.D.pop,
  @cadeque.operations.D.eject,
  @cadeque.operations.D.concat
).

Print Assumptions library.
