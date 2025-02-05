* The OCaml code currently does not build.
  `Unbound module Lib.ArtWend`

* `make` currently fails with Coq 8.20.

* In `theory/cadeque`, merge `buffer.v` into `deque.v`.
  Or keep `buffer.v` but make the type `buffer` a synonym for `deque` (no constructor).

* In `theory/deque`, rename `deque_size.v` into `deque.v`. (Thus the module alias can be removed.)
  `deque.v` (unindexed) should be `deque_plain.v` or something similar.
  Each of the three files `deque*.v` should begin with a comment
  explaining its role (and pointing to the other two).

* Clean up `README.md`.

  + Add a link to our paper.
  + Clarify which implementations are presented in our paper (deque and cadeque?)
    and which implementations are not presented (list and steque?).
  + Do we want to mention `rev` and `nth` in the main table?
    Why are these operations supported by deques only?
  + Clarify how much time `make test` is expected to require.
    It seems to take a long time.

* Add a `README.md` in each subdirectory of `theory`
  so as to describe the role of each file.

* Extend the `Makefile` with more entries (if useful).

* Set up an OCaml package and a Coq package.
  Declare the dependencies of the Coq package
  on `coq-aac-tactics`, `coq-hammer`, `coq-equations`.
  Document which versions of OCaml/Coq are supported.
  (Add constraints in `dune-project`.)

* Review (together) the public APIs of the OCaml and Coq libraries,
  and make sure that we are happy with the naming conventions.

* Should `lib` move inside `src`?

* Clean up `theory/cadeque/abstraction.v`.

* Remove `theory/extraction`,
  add `make extract` to produce the extracted code,
  document it.
  Make sure that `dune` can compile it
  (disable certain errors).
