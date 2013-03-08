Add LoadPath "/home/daniel/code/brainfuck/haskell/coq".
Require Import Data.Expr.
Set Implicit Arguments.

Section Expr_Optimize_defs.
  Definition addConsts (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (Return (Const a)) (Return (Const b)) => Return (Const (a + b))
    | _                                                       => e
    end.

  Theorem optAddConsts: forall (f: Z -> Z) (e: Expr),
    eval f (addConsts e) = eval f e.
  Proof.
  Qed.
End Expr_Optimize_defs.
