Require Import Expr AST.
Require Import ssreflect ssrnat ssrint ssralg ssrfun.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Theorems.
  Theorem evals: forall (f: int -> int) (e: Expr),
    eval f e = eval' f e.
  Proof.
    move=> f.
    elim=> //= [op e IH | op a IHa b IHb].
      by rewrite IH.
      by rewrite IHa IHb.
  Qed.

  Theorem unfoldId: forall (e: Expr),
    unfold OperateUnary OperateBinary Return e = e.
  Proof.
    elim=> //= [op e IH | op a IHa b IHb].
      by rewrite IH.
      by rewrite IHa IHb.
  Qed.

  Theorem addC: forall (f: int -> int) (a b: Expr),
    eval f (OperateBinary Add a b) = eval f (OperateBinary Add b a).
  Proof. move=> ? ? ?. by rewrite /eval addzC. Qed.

  Theorem addA: forall (f: int -> int) (a b c: Expr),
    eval f (OperateBinary Add a (OperateBinary Add b c)) =
      eval f (OperateBinary Add (OperateBinary Add a b) c).
  Proof. move=> ? ? ? ?. by rewrite /eval addzA. Qed.

  Theorem add0e: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Add (Return (Const 0)) e) = eval f e.
  Proof. move=> ? ?. by rewrite /eval add0z. Qed.

  Theorem adde0: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Add e (Return (Const 0))) = eval f e.
  Proof. move=> ? ?. by rewrite /eval addzC add0z. Qed.

  Theorem addnn: forall (f: int -> int) (a b: int),
    eval f (OperateBinary Add (Return (Const a)) (Return (Const b))) =
      eval f (Return (Const (addz a b))).
  Proof. by []. Qed.

  Theorem mulC: forall (f: int -> int) (a b: Expr),
    eval f (OperateBinary Mul a b) = eval f (OperateBinary Mul b a).
  Proof. move=> ? ? ?. by rewrite /eval mulzC. Qed.

  Theorem mulA: forall (f: int -> int) (a b c: Expr),
    eval f (OperateBinary Mul a (OperateBinary Mul b c)) =
      eval f (OperateBinary Mul (OperateBinary Mul a b) c).
  Proof. move=> ? ? ? ?. by rewrite /eval mulzA. Qed.

  Theorem mul0e: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Mul (Return (Const 0)) e) = 0.
  Proof. move=> ? ?. by rewrite /eval mul0z. Qed.

  Theorem mule0: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Mul e (Return (Const 0))) = 0.
  Proof. move=> ? ?. by rewrite /eval mulz0. Qed.

  Theorem mul1e: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Mul (Return (Const 1)) e) = eval f e.
  Proof. move=> ? ?. by rewrite /eval mul1z. Qed.

  Theorem mule1: forall (f: int -> int) (e: Expr),
    eval f (OperateBinary Mul e (Return (Const 1))) = eval f e.
  Proof. move=> ? ?. by rewrite /eval mulzC mul1z. Qed.
End Theorems.
