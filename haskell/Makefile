warnings = -Wall \
	   -fno-warn-unused-do-bind \
	   -fwarn-duplicate-exports \
	   -fwarn-hi-shadowing \
	   -fwarn-incomplete-patterns \
	   -fwarn-lazy-unlifted-bindings \
	   -fwarn-missing-fields \
	   -fwarn-missing-methods \
	   -fwarn-missing-signatures \
	   -fwarn-monomorphism-restriction \
	   -fwarn-name-shadowing \
	   -fwarn-orphans \
	   -fwarn-overlapping-patterns \
	   -fwarn-tabs \
	   -fwarn-type-defaults \
	   -fwarn-unused-binds \
	   -fwarn-unused-imports \
	   -fwarn-unused-matches \
	   -fwarn-wrong-do-bind \

flags = -isrc -o build/brainfuck -odir build -hidir build/

all:
	ghc --make $(warnings) $(flags) src/Main.hs

ghci:
	ghc --interactive $(warnings) $(flags) src/Main.hs

clean:
	rm -r build/
	mkdir build/

hlint:
	$(HOME)/.cabal/bin/hlint . -p src
