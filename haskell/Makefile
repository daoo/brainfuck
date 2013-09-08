build:
	@cabal build --ghc-options="-H64m -rtsopts"

prof:
	@cabal build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal build --ghc-options="-fllvm -H64m -O2"

deps:
	@cabal install --only-dependencies

conf:
	@cabal configure

test:
	@cabal test

repl:
	@cabal repl

clean:
	@cabal clean --save-configure

lint:
	@hlint src
