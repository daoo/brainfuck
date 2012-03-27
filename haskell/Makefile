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

flags = -isrc:build -o build/brainfuck -odir build -hidir build/

build: parser
	ghc --make $(warnings) $(flags) src/Main.hs

ghci: parser
	ghc --interactive $(warnings) $(flags) src/Main.hs

parser: src/Brainfuck/Parser/Parser.y
	happy src/Brainfuck/Parser/Parser.y -o build/Brainfuck/Parser/Parser.hs

clean:
	rm -r build/
	mkdir -p build/Brainfuck/Parser/

hlint:
	$(HOME)/.cabal/bin/hlint . -c -p src
