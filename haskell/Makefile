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

sources = $(shell find src -type f -name '*.hs')
flags   = -isrc:$(outdir) -odir $(outdir) -hidir $(outdir)

build: parser.hs
	ghc --make $(warnings) $(flags) -o $(outdir)/$(exename) src/Main.hs

ghci: parser.hs
	ghci $(warnings) $(flags) $(sources)

parser.hs: src/Brainfuck/Parser/Parser.y
	happy -gc src/Brainfuck/Parser/Parser.y -o $(outdir)/Brainfuck/Parser/Parser.hs

clean:
	rm -r $(outdir)/*
	mkdir -p $(outdir)/Brainfuck/Parser/

hlint:
	hlint src -c

.PHONY: clean hlint
