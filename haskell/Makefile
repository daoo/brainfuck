build: prof

prof:
	@cabal-dev build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

test:
	@./dist/build/brainfuck-tests/brainfuck-tests

clean:
	@cabal-dev clean --save-configure

ctags:
	@echo ":ctags" | ghci -isrc/ -v0 src/TestProperties.hs

lint:
	@hlint src -xc

.PHONY: build clean lint
