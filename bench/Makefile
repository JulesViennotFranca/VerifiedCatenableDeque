.PHONY: all
all:
	@ dune build ./benchmark.exe ./listLike.exe

.PHONY: run
run: all
#	@ dune exec ./listLike.exe
	@ dune exec ./benchmark.exe

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: view
view: all
	@ open graphics.ipynb
