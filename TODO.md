* `make` currently fails with Coq 8.20.

* Let every file name begin with a capital letter,
  so that module names begin with a capital letter,
  as in OCaml? (The paper assumes `Deque` is a module name.)

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

* Review (together) the public APIs of the OCaml and Coq libraries,
  and make sure that we are happy with the naming conventions.

* Clean up `theory/cadeque/abstraction.v`.

* fix `make test`: the code in `test/` needs updating

* Perform Monolith tests.
  Apply them to all of the libraries that we benchmark.
