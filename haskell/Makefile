exename = brainfuck

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

flags = -isrc:build -odir build -hidir build

make_main = --make $(flags) $(warnings) -o build/$(exename) src/Main.hs

build:
	ghc -rtsopts $(make_main)

release:
	ghc -O3 $(make_main)

test:
	ghc -O3 --make $(warnings) $(flags) -o build/test src/TestPrograms.hs
	./build/test

bench:
	ghc -O3 --make $(warnings) $(flags) -o build/bench src/Benchmark.hs

ghci:
	ghci $(warnings) $(flags) src/TestProperties.hs

clean:
	rm -r build/*

ctags:
	echo ":ctags" | ghci -isrc/:build/ -v0 src/TestProperties.hs

lint:
	hlint src -c

.PHONY: build clean lint
