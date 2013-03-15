opts = -opt
coqc = coqc $(opts)

all: Aux.vo Expr.vo OptExpr.vo Theorems.vo

extraction: Expr.hs

Expr.hs: Extraction.v Expr.vo
	$(coqc) Extraction.v

Aux.vo: Aux.v
	$(coqc) Aux.v

Expr.vo: Aux.vo Expr.v
	$(coqc) Expr.v

OptExpr.vo: Expr.vo OptExpr.v
	$(coqc) OptExpr.v

Theorems.vo: Expr.vo OptExpr.vo Theorems.v
	$(coqc) Theorems.v

clean:
	$(RM) *.vo *.glob *.*~ *.hs

.PHONY: clean
