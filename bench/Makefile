.PHONY: all
all:
	@ dune build ./benchmark.exe ./listLike.exe

.PHONY: run
run: all
#	@ dune exec ./listLike.exe
	@ dune exec ./benchmark.exe -- --exclude List --bins 41 --inhabitants 50
	@ dune exec ./benchmark.exe -- --exclude Sek --exclude Cadeque --exclude KOT --bins 17 --inhabitants 50

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: view
view: all
	@ open graphics.ipynb
