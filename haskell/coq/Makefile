opts = -opt
coqc = coqc $(opts)

all: Expr.vo OptExpr.vo Theorems.vo

extraction: Expr.hs

Expr.hs: Extraction.v Expr.vo
	$(coqc) Extraction.v

Expr.vo: Expr.v
	$(coqc) Expr.v

OptExpr.vo: OptExpr.v
	$(coqc) OptExpr.v

Theorems.vo: Expr.vo OptExpr.vo Theorems.v
	$(coqc) Theorems.v

clean:
	$(RM) *.vo *.glob *.*~ *.hs

.PHONY: clean
