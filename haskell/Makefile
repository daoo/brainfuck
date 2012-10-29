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

flags = -isrc:build -odir build -hidir build -XTupleSections

make_main = --make $(flags) $(warnings) -o build/$(exename) src/Main.hs

build:
	ghc -rtsopts $(make_main)

release:
	ghc -O3 $(make_main)

test:
	ghc -O3 --make $(warnings) $(flags) -o build/test src/Programs.hs
	./build/test

ghci:
	ghci $(warnings) $(flags) tests/Properties.hs

clean:
	rm -r build/*

ctags:
	echo ":ctags" | ghci -isrc/ -v0 src/TestProperties.hs

lint:
	hlint src -xc

.PHONY: build clean lint
