BENCHMARKS=csv json pgn ppm sexp intexp
QUOTA?=5
BENCHARGS=-quota $(QUOTA) -ci-absolute -ascii -display blank -clear-columns -all-values +time

all:
	dune build -p flap

clean:
	dune clean
	rm -fr benchmark-scratch

test:
	dune runtest

bench: $(foreach b,$(BENCHMARKS),$(b)-bench)

%-bench:
        # Some generated code is currently not tail-recursive,
        # so we need a bigger stack.
	dune build benchmarks/$*/$*_benchmark.exe
	ulimit -s unlimited
	mkdir -p benchmark-scratch
	cp ./_build/default/benchmarks/$*/.$*_benchmark.eobjs/byte/*.cm* benchmark-scratch || true
	cp `ocamlfind query asp`/*.cm* benchmark-scratch
	cd benchmark-scratch && dune exec -- ../benchmarks/$*/$*_benchmark.exe $(BENCHARGS) | tee ../benchmark-$*.out
	rm -fr benchmark-scratch
	mkdir -p paper/csv && ./benchmarks/munge.py < benchmark-$*.out > paper/csv/$*.csv

.PHONY: all clean test bench
