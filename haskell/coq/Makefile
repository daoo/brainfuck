opts = -opt
coqc = coqc $(opts)

all: Expr.vo OptExpr.vo Theorems.vo

Expr.vo: Expr.v
	$(coqc) Expr.v

OptExpr.vo: OptExpr.v
	$(coqc) OptExpr.v

Theorems.vo: Expr.vo OptExpr.vo Theorems.v
	$(coqc) Theorems.v

clean:
	$(RM) *.vo *.glob *.*~

.PHONY: clean
