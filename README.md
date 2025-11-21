# Verified Purely Functional Catenable Real-Time Deques

This repository serves as a companion to the paper
*Verified Purely Functional Catenable Real-Time Deques*
by
Jules Viennot,
Arthur Wendling,
Armaël Guéneau,
and
François Pottier.

This paper presents OCaml and Rocq implementations of the data structures
presented by
Haim Kaplan
and
Robert E. Tarjan
in the paper
[Purely Functional, Real-Time Deques with Catenation](https://doi.org/10.1145/324133.324139).

At this time,
our code is accepted by Rocq 8.19
but is **rejected by Rocq 8.20 and by Rocq 9**
with a "universe inconsistency" error
that we do not understand.
[We have reported this issue](https://github.com/mattam82/Coq-Equations/issues/635).

## Data Structures

A double-ended queue, also known as a *deque*, is a queue that supports the
four operations *push*, *inject*, *pop*, *eject*.

+ *push* and *pop*
  insert and extract an element at the front end of the queue.
+ *inject* and *eject*
  insert and extract an element at the rear end of the queue.

In addition to these four operations,
*catenable deques* support a concatenation operation,
which expects two deques and produces a deque.

Following Kaplan and Tarjan's paper,
we implement four data structures,
each of which supports a subset of the above five operations.
The following table shows which operations are supported
and indicates their worst-case time complexity:

|         | push | inject |  pop | eject | concat |  rev |         nth         |
| :-----: | :--: | :----: | :--: | :---: | :----: | :--: | :-----------------: |
|  List   | O(1) |   n/a  | O(1) |  n/a  |   n/a  |  n/a |         n/a         |
|  Deque  | O(1) |   O(1) | O(1) |  O(1) |   n/a  | O(1) | O(log(min(i, n-i))) |
| Steque  | O(1) |   O(1) | O(1) |  n/a  |  O(1)  |  n/a |         n/a         |
| Cadeque | O(1) |   O(1) | O(1) |  O(1) |  O(1)  |  n/a |         n/a         |

Each data structure is implemented both in OCaml and in Rocq.
The Rocq implementations are verified.

## Organization

The main directories are as follows:

+ [lib](/lib/) contains OCaml code for each data structure.

  This OCaml code can be compiled using the command `make lib`.

+ [theory](/theory/) contains Rocq code and proofs of correctness
  for each data structure.

  This Coq code can be compiled using the command `make theory`,
  which requires 10 to 30 minutes.

+ [extraction](/extraction/) defines build rules to extract the
  Rocq implementation of each data structure into OCaml.

  Extracting the Rocq code into OCaml and compiling the result
  as an OCaml library is done by the command `make extraction`,
  which takes a few seconds.

  Note: although the extracted code is a useful witness that our Rocq
  implementation can be executed, it is currently expected that it is not
  efficient, and in particular not constant time. This is because the extracted
  code contains superfluous computations on natural numbers (size and level
  indices) that we have no good way of erasing at extraction.
  Our handwritten OCaml implementation is the one that should be used for
  all practical purposes.

+ [bench](/bench/) contains benchmarks of the OCaml code. The benchmarks can be
  run with the command `cd bench && make run`, which takes several minutes.

  More details on the benchmarks are available in [bench](/bench/).

+ [test_coq](/test_coq/) TODO

+ [test_monolith](/test_monolith/) contains a harness for model-based testing of
  the OCaml library against a reference implementation (lists). To run this
  test, you first need to additionally install the
  [Monolith](https://gitlab.inria.fr/fpottier/monolith) OCaml library, and the
  `parallel` tool, then run `cd test_monolith && make run`.
