# The switch that is used for testing is the current switch.
SWITCH := $(shell opam switch show)

# By default, [make] compiles the test.
.PHONY: build
build:
	@ dune build

SUBDIRS := \

.PHONY: parallel
parallel: build
	@ parallel --tag --line-buffer make random_nodep EXE=test_{}.exe \
	::: \
	deque \
	steque \
	cadeque \

.PHONY: run_deque
run_deque: build
	@ make random_nodep EXE=test_deque.exe

.PHONY: run_steque
run_steque: build
	@ make random_nodep EXE=test_steque.exe

.PHONY: run_cadeque
run_cadeque: build
	@ make random_nodep EXE=test_cadeque.exe

# This line is somewhat problematic, as it assumes that ocamlfind and
# monolith are installed in the current switch, whatever it is.
-include $(shell ocamlfind query monolith)/Makefile.monolith
