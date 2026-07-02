BUILD := ../_build/default/minibenchmark

.PHONY: all
all:
	@ dune build .
	@ echo "Benchmark 1 involves 100 repetitions at n = 5000."
	@ echo "Benchmark 2 involves ONE repetition  at n = 5000."
	@ hyperfine --warmup 1 \
	    '$(BUILD)/deques/main.exe' \
	    '$(BUILD)/deques_extracted/main.exe'
