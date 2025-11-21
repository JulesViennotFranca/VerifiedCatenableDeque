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

## Requirements

Our [OCaml code](lib/) requires OCaml 4.08 or newer.

Our [Rocq code](theory/) is accepted by Rocq 8.19
but is **rejected by Rocq 8.20 and by Rocq 9**
with a "universe inconsistency" error
that we do not understand.
[We have reported this issue](https://github.com/mattam82/Coq-Equations/issues/635).

## Data Structures

A double-ended queue, also known as a *deque*,
is an abstract data structure that represents a sequence
and that offers the constant *empty*
as well as the four operations *push*, *inject*, *pop*, *eject*.

+ *push* and *pop*
  insert and extract an element at the front end of the queue.
+ *inject* and *eject*
  insert and extract an element at the rear end of the queue.

In addition to these four operations,
*catenable deques* support a concatenation operation, *concat*,
which expects two deques and produces a deque.
We say *cadeque* as a short-hand for *catenable deque*.

Following Kaplan and Tarjan's paper,
we implement four data structures,
each of which supports a subset of the above operations.
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

## Summary of Verified Results

A [signature](theory/Signatures.v) lists the types and operations
that must be offered by an implementation of catenable deques,
as well as the correctness properties that these operations must satisfy.
We offer two variants of this signature, in *intrinsic style*
and *extrinsic style*.

A [summary](theory/Cadeque/Summary.v) file
asks Rocq to verify that our implementation of catenable deques
satisfies (the two variants of) this signature,
that no axiom has been used,
and that no proof has been left unfinished.

Typing `make axioms` applies a similar check to all data structures,
as opposed to just catenable deques.

## Organization

To compile everything, type `make`.

The main directories are as follows:

+ [lib](/lib/) contains OCaml code for each data structure.

  This OCaml code is compiled by `make lib`.

+ [theory](/theory/) contains Rocq code and proofs of correctness
  for each data structure.

  This Coq code is compiled by `make theory`,
  which requires 10 to 30 minutes.

+ [extraction](/extraction/) contains commands to extract the
  Rocq implementation of each data structure down to OCaml.

  Extracting and compiling the resulting OCaml code
  as an OCaml library is done by `make extraction`,
  which takes a few seconds.

  Although the extracted code is a useful witness that our Rocq
  implementation can be executed, it is not
  efficient: in particular, the complexity of each operation is *not* O(1).
  Indeed, the extracted code contains superfluous computations on natural numbers
  (such as size and level indices) which currently cannot be erased during extraction.
  For all practical purposes,
  our hand-written OCaml implementation should be preferred.

+ [bench](/bench/) contains benchmarks of the OCaml code. The benchmarks can be
  run with the command `make bench`, which takes several minutes.

  More details on the benchmarks are available in [bench/README.md](bench/README.md).

+ [test_coq](/test_coq/) contains the check that is executed by `make axioms`.
  It also contains a few files where we check that reduction inside Rocq works.
  These checks are performed by Rocq when everything is compiled via `make`.

+ [test_monolith](/test_monolith/) contains a harness for model-based testing of
  the OCaml library against a reference implementation (lists). To run this
  test,
  [Monolith](https://gitlab.inria.fr/fpottier/monolith) and GNU
  `parallel` are needed. (If necessary, type `opam install monolith`.)
  Run the tests with `make test`.
  By design, these tests run forever.
  If a discrepancy between the reference implementation
  and the candidate implementation is found
  then the scenario that gives rise to the problem is printed.
