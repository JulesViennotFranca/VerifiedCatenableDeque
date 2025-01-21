* The OCaml code currently does not build.
  `Unbound module Lib.ArtWend`

* `make` currently fails with Coq 8.20.

* Clean up `README.md`.

  + Add a link to our paper.
  + Clarify which implementations are presented in our paper (deque and cadeque?)
    and which implementations are not presented (list and steque?).
  + Do we want to mention `rev` and `nth` in the main table?
    Why are these operations supported by deques only?
  + Clarify how much time `make test` is expected to require.
    It seems to take a long time.

* Add `AUTHORS.md`.

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

* Why does the code in `theory/extraction` contain manual modifications?
  This cannot be trusted / maintained.
  -> the code didn't type checked because some importation of modules were not used,
     so I only commented out the modules that weren't used,
     maybe there exist a dune flag to not raise errors for unused modules.
