.PHONY: all
all:
	@ dune build

.PHONY: test
test:
	@ dune test

.PHONY: clean
clean:
	@ git clean -fdX
