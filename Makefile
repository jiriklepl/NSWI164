.PHONY: all run test Translator Graphs generateGraphs clean clear

all:
	@cabal build all

run:
	@cabal run -v0 Translator

Translator:
	@cabal build Translator

Graphs:
	@cabal build Graphs

generateGraphs:
	@cabal run -v0 Graphs

test:
	@bash -c 'diff <(cabal run -v0 Translator < sample.dsl) sample.lilypond && echo "SUCCESS"'

clear: clean
clean:
	@cabal clean