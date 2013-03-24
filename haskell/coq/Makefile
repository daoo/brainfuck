coqc = /usr/bin/coqc

all: Aux.vo Expr.vo Theorems.vo

extraction: Expr.hs

Expr.hs: Extraction.v Expr.vo
	$(coqc) Extraction.v

AST.vo: Expr.vo AST.v
	$(coqc) AST.v

Aux.vo: Aux.v
	$(coqc) Aux.v

Expr.vo: Aux.vo Expr.v
	$(coqc) Expr.v

Theorems.vo: Expr.vo AST.vo Theorems.v
	$(coqc) Theorems.v

clean:
	$(RM) *.vo *.glob *.*~ *.hs

.PHONY: clean
