* The OCaml code currently does not build.
  `Unbound module Lib.ArtWend`

* `make` currently fails with Coq 8.20.

* `make` used to work with Coq 8.19, but is broken now. Here is the message:
   ```
File "./theory/cadeque/core.v", line 645, characters 14-66:
Error:
In environment
A : Type
lvl : arity
C : color
a1 : arity
a2 : stored A lvl
a3 : stored A lvl
a4 : stored A lvl
a5 : stored A lvl
a6 : stored A lvl
a0 : arity
rC1 : color
qp1 : arity
qs1 : arity
col : coloring qp1 qs1 (S a0) yellow
p : prefix' (stored A lvl) 2
p1 : t (stored A lvl) (6 + 2)
e : seq p1 = [a0] ++ [a2] ++ [a3] ++ [a4] ++ [a5] ++ [a6] ++ seq p
s : suffix' (stored A lvl) (5 + qs1)
child : chain A (S lvl) (S a0) only C rC1
reg := Y : regularity yellow C (S a0) C rC1
The term "? Triple reg (Only (adapt_to_prefix col) p1 s) child" has type
 "{x : triple A lvl only C | ?P x}" while it is expected to have type
 "{t : triple A lvl only C
  | triple_seq t =
    (stored_seq a0 ++
     stored_seq a2 ++
     stored_seq a3 ++ stored_seq a4 ++ stored_seq a5 ++ stored_seq a6) ++
    concat_map_seq (@stored_seq) p ++
    chain_seq child ++ concat_map_seq (@stored_seq) s}"
(unable to find a well-typed instantiation for "?P": cannot ensure that
"arity" is a subtype of "stored A lvl").
```
  Something is weird is going on with `a0` and/or `a1`.
  This could be caused by my renaming of `ckind` to `arity` and `ck` to `a`
  in `types.v`, although I do not clearly see why this would create a problem here.

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

* Why do we have both `core.ml` and `makeCore.ml`?
  Can we avoid this duplication?

* Why does the code in `theory/extraction` contain manual modifications?
  This cannot be trusted / maintained.

* In `theory/cadeque`, I have renamed the type `ckind` to `arity`,
  but there are still many variables named `ck`. Rename?
