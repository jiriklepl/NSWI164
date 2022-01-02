.PHONY: all run test

all:
	@cabal build

run:
	@cabal run -v0

test:
	@bash -c 'diff <(cabal run -v0 < sample.dsl) sample.lilypond && echo "SUCCESS"'