DUNE ?= dune

all:
	$(DUNE) build

check: tests

test:
	$(DUNE) runtest --no-buffer

clean:
	rm -rf _build

.PHONY: pin test all clean check
