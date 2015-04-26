build:
	@cabal build --ghc-options="-Wall -O -fno-ignore-asserts"

release:
	@cabal clean
	@cabal build  --ghc-options="-Wall -O2 -fignore-asserts"

clean:
	@cabal clean

lint:
	@hlint src
