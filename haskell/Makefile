build:
	cabal build

ctags:
	echo ":ctags" | ghci -isrc/ -v0 src/TestProperties.hs

lint:
	hlint src -xc

.PHONY: build clean lint
