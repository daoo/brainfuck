outdir  = build
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

flags = -isrc:$(outdir) -odir $(outdir) -hidir $(outdir)

build: parser
	ghc -O3 --make $(warnings) $(flags) -o $(outdir)/$(exename) src/Main.hs

test: build
	ghc -O3 --make $(warnings) $(flags) -o $(outdir)/test src/TestPrograms.hs
	./build/test

ghci: parser
	ghci $(warnings) $(flags) src/TestProperties.hs

parser: src/Brainfuck/Parser/Parser.y
	happy -gc src/Brainfuck/Parser/Parser.y -o $(outdir)/Brainfuck/Parser/Parser.hs
	ghc -O3 --make $(flags) $(outdir)/Brainfuck/Parser/Parser.hs

clean:
	rm -r $(outdir)/*
	mkdir -p $(outdir)/Brainfuck/Parser/

ctags:
	echo ":ctags" | ghci -isrc/:build/ -v0 src/TestProperties.hs

hlint:
	hlint src -c

.PHONY: clean hlint
