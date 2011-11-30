BUILD_DIR := ../build
OUTDIRS := -odir $(BUILD_DIR) -hidir $(BUILD_DIR)
GHC_WARNINGS := -fno-warn-unused-do-bind \
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
		-fwarn-wrong-do-bind

all: Parser.o Brainfuck/CommandLine/Brainfuck.hs
	ghc -o $(BUILD_DIR)/brainfuck $(OUTDIRS) \
            -Wall $(GHC_WARNINGS) \
	    --make Brainfuck/CommandLine/Brainfuck.hs

Parser.o: Parser
	ghc $(OUTDIRS) Brainfuck/Parser/Parser.hs

Parser: Brainfuck/Parser/Parser.y
	happy Brainfuck/Parser/Parser.y

