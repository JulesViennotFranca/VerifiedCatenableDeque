.PHONY: all
all:
	@ dune build

.PHONY: lib
lib:
	@ dune build lib

.PHONY: theory
theory:
	@ dune build theory

.PHONY: extraction
extraction:
	@ dune build extraction

.PHONY: test
test:
	@ make -C test_monolith parallel

.PHONY: bench
bench:
	@ make -C bench run

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: axioms
axioms: all
	@ echo "This command should print 'Closed under the global context':"
	coqc -R ./_build/default/theory Deques test_coq/check_axioms.v

# ------------------------------------------------------------------------------

# [make versions] compiles the OCaml package under several versions of OCaml,
# whose list is specified below.

# This requires appropriate opam switches to exist. A missing switch
# can be created like this:
#   opam switch create 4.03.0

VERSIONS := \
  4.12.0 \
  4.13.1 \
  4.14.2 \
  5.0.0 \
  5.1.0 \
  5.2.0 \

.PHONY: versions
versions:
	@(echo "(lang dune 2.0)" && \
	  for v in $(VERSIONS) ; do \
	    echo "(context (opam (switch $$v)))" ; \
	  done) > dune-workspace.versions
	@ dune build --workspace dune-workspace.versions lib
