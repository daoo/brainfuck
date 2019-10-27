build:
	@cabal v2-build --ghc-options="-Wall -O -fno-ignore-asserts"

release:
	@cabal v2-clean
	@cabal v2-build --ghc-options="-Wall -O2 -fignore-asserts"

clean:
	@cabal v2-clean

lint:
	@hlint src
