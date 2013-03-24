Require Import Expr OptExpr AST.
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
    elim=> //= [a IHa b IHb | a b IH].
      by rewrite IHa IHb.
      by rewrite IH.
  Qed.

  Theorem unfoldId: forall (e: Expr),
    unfold Add Mul Const Var e = e.
  Proof.
    elim=> //= [a IHa b IHb | a b IH].
      by rewrite IHa IHb.
      by rewrite IH.
  Qed.

  Theorem addeC: forall (f: int -> int) (a b: Expr),
    eval f (Add a b) = eval f (Add b a).
  Proof. move=> f a b. by rewrite /eval addzC. Qed.

  Theorem addeA: forall (f: int -> int) (a b c: Expr),
    eval f (Add a (Add b c)) = eval f (Add (Add a b) c).
  Proof. move=> ? ? ? ?. by rewrite /eval addzA. Qed.

  Theorem add0e: forall (f: int -> int) (e: Expr),
    eval f (Add (Const 0) e) = eval f e.
  Proof. move=> ? ?. by rewrite /eval add0z. Qed.

  Theorem adde0: forall (f: int -> int) (e: Expr),
    eval f (Add e (Const 0)) = eval f e.
  Proof. move=> ? ?. by rewrite /eval addzC add0z. Qed.

  Theorem addnn: forall (f: int -> int) (a b: int),
    eval f (Add (Const a) (Const b)) = eval f (Const (addz a b)).
  Proof. by []. Qed.

  Theorem mulzze: forall (f: int -> int) (a b: int) (c: Expr),
    eval f (Mul a (Mul b c)) = eval f (Mul (mulz a b) c).
  Proof. move=> ? ? ? ?. by rewrite /eval mulzA. Qed.

  Theorem mul0e: forall (f: int -> int) (e: Expr),
    eval f (Mul 0 e) = 0.
  Proof. move=> ? ?. by rewrite /eval mul0z. Qed.

  Theorem mul1e: forall (f: int -> int) (e: Expr),
    eval f (Mul 1 e) = eval f e.
  Proof. move=> ? ?. by rewrite /eval mul1z. Qed.

  Theorem evalSimplify: forall (f: int -> int) (e: Expr),
    eval f e = eval f (simplify e).
  Proof.
    move=> f.
    elim=> //= [ a | a | a IHa b IHb | a b IH ].
  Qed.
End Theorems.
