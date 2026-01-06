.PHONY: all
all:
	@ dune build ./benchmark.exe ./listLike.exe

.PHONY: run
run: all
#	@ dune exec ./listLike.exe
	@ dune exec ./benchmark.exe -- Sek Cadeque KOT --bins 41 --inhabitants 50
	@ dune exec ./benchmark.exe -- List Deque --bins 17 --inhabitants 50

.PHONY: clean
clean:
	@ git clean -fdX

.PHONY: view
view: all
	@ open graphics.ipynb
