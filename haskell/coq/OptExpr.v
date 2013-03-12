Require Import Expr ssreflect ssrnat ssrint.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Expr_Optimize_defs.
  Definition addConsts (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (Return (Const a)) (Return (Const b)) => Return (Const (addz a b))
    | _                                                       => e
    end.

  Definition addZeroL (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (Return (Const 0)) b => b
    | _                                      => e
    end.

  Definition addZeroR (e: Expr) : Expr :=
    match e with
    | OperateBinary Add a (Return (Const 0)) => a
    | _                                      => e
    end.

  Definition mulConsts (e: Expr) : Expr :=
    match e with
    | OperateBinary Mul (Return (Const a)) (Return (Const b)) => Return (Const (mulz a b))
    | _                                                       => e
    end.

  Definition mulOneL (e: Expr) : Expr :=
    match e with
    | OperateBinary Mul (Return (Const 1)) b => b
    | _                                      => e
    end.

  Definition mulOneR (e: Expr) : Expr :=
    match e with
    | OperateBinary Mul a (Return (Const 1)) => a
    | _                                      => e
    end.

  Definition mulNegOneL (e: Expr) : Expr :=
    match e with
    | OperateBinary Mul (Return (Const (Negz 0))) b => OperateUnary Negate b
    | _                                             => e
    end.

  Definition mulNegOneR (e: Expr) : Expr :=
    match e with
    | OperateBinary Mul a (Return (Const (Negz 0))) => OperateUnary Negate a
    | _                                             => e
    end.

  Definition negConstant (e: Expr) : Expr :=
    match e with
    | OperateUnary Negate (Return (Const a)) => Return (Const (oppz a))
    | _                                      => e
    end.

  Definition negCollaps (e: Expr) : Expr :=
    match e with
    | OperateUnary Negate (OperateUnary Negate a) => a
    | _                                           => e
    end.

  Definition idAny (e: Expr) : Expr :=
    match e with
    | OperateUnary Id a => a
    | _                 => e
    end.

  Definition swapConstGet (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (Return (Const _) as a) (Return (Get _) as b) => OperateBinary Add b a
    | OperateBinary Mul (Return (Const _) as a) (Return (Get _) as b) => OperateBinary Mul b a
    | _                                                               => e
    end.

  Definition swapConstDown (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (Return (Const _) as a) (OperateBinary Add (Return (Get _) as b) c) => OperateBinary Add b (OperateBinary Add a c)
    | OperateBinary Mul (Return (Const _) as a) (OperateBinary Mul (Return (Get _) as b) c) => OperateBinary Mul b (OperateBinary Mul a c)
    | _                                                                                     => e
    end.

  Definition rotateBinary (e: Expr) : Expr :=
    match e with
    | OperateBinary Add (OperateBinary Add a b) c => OperateBinary Add a (OperateBinary Add b c)
    | OperateBinary Mul (OperateBinary Mul a b) c => OperateBinary Mul a (OperateBinary Mul b c)
    | _                                           => e
    end.
End Expr_Optimize_defs.
