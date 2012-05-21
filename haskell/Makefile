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

build:
	ghc -O3 --make $(warnings) $(flags) -o $(outdir)/$(exename) src/Main.hs

test: build
	ghc -O3 --make $(warnings) $(flags) -o $(outdir)/test src/TestPrograms.hs
	./$(outdir)/test

ghci:
	ghci $(warnings) $(flags) src/TestProperties.hs

clean:
	rm -r $(outdir)/*

ctags:
	echo ":ctags" | ghci -isrc/:$(outdir)/ -v0 src/TestProperties.hs

lint:
	hlint src -c

.PHONY: build clean lint
