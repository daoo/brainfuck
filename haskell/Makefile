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

sources = $(wildcard src/**/*.hs)
flags   = -isrc:$(outdir) -o $(outdir)/$(exename) -odir $(outdir) -hidir $(outdir)

build: parser
	ghc --make $(warnings) $(flags) src/Main.hs

ghci: parser
	ghc --interactive $(warnings) $(flags) $(sources)

parser: src/Brainfuck/Parser/Parser.y
	@mkdir -p $(outdir)/Brainfuck/Parser/
	happy src/Brainfuck/Parser/Parser.y -o $(outdir)/Brainfuck/Parser/Parser.hs

clean:
	rm -r $(outdir)/
	mkdir -p $(outdir)/Brainfuck/Parser/

hlint:
	hlint . -c -p src
