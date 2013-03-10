Require Import Expr OptExpr ssreflect ssrnat ssrint ssralg ssrfun.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Theorems.
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
  Proof. done. Qed.

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

  Theorem optAddConsts: forall (f: int -> int) (e: Expr),
    eval f (addConsts e) = eval f e.
  Proof. move=> ?. do 4?case=> //=. move=> ?. by do 2?case=> //=. Qed.

  Theorem optAddZeroL: forall (f: int -> int) (e: Expr),
    eval f (addZeroL e) = eval f e.
  Proof.
    move=> f.
    do 5?case=> //=.
    case=> e //=.
    case: eval => n //=.
    by rewrite subn0.
  Qed.

  Theorem optAddZeroR: forall (f: int -> int) (e: Expr),
    eval f (addZeroR e) = eval f e.
  Proof.
    move=> f.
    case=> //=.
    case=> e //=.
    do 4?case=> //=.
    by rewrite addzC add0z.
  Qed.

  Theorem optMulConsts: forall (f: int -> int) (e: Expr),
    eval f (mulConsts e) = eval f e.
  Proof.
    move=> f.
    do 4?case=> //=.
    move=> z.
    by do 2?case=> //=.
  Qed.

  Theorem optMulOneL: forall (f: int -> int) (e: Expr),
    eval f (mulOneL e) = eval f e.
  Proof.
    move=> f.
    do 7?case=> //=.
    move=> e.
    case: eval => n //=.
    by rewrite mul1n.
    by rewrite mul1n.
  Qed.

  Theorem optMulOneR: forall (f: int -> int) (e: Expr),
    eval f (mulOneR e) = eval f e.
  Proof.
    move=> f.
    case=> //=.
    case=> e //=.
    do 5?case=> //=.
    by rewrite mulzC mul1z.
  Qed.

  Theorem optMulNegOneL: forall (f: int -> int) (e: Expr),
    eval f (mulNegOneL e) = eval f e.
  Proof.
    move=> f.
    do 7?case=> //=.
    move=> e.
    case: eval => //=.
  Admitted.

  Theorem optMulNegOneR: forall (f: int -> int) (e: Expr),
    eval f (mulNegOneR e) = eval f e.
  Proof.
    move=> f.
    case=> //=.
    case=> e //=.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> //=.
  Admitted.

  Theorem optNegConstant: forall (f: int -> int) (e: Expr),
    eval f (negConstant e) = eval f e.
  Proof. move=> f. by do 4?case=> //=. Qed.

  Theorem optNegCollaps: forall (f: int -> int) (e: Expr),
    eval f (negCollaps e) = eval f e.
  Proof. move=> f. do 4?case=> //=. move=> e. by rewrite oppzK. Qed.

  Theorem optIdAny: forall (f: int -> int) (e: Expr),
    eval f (idAny e) = eval f e.
  Proof. move=> f. by do 4?case=> //=. Qed.

  Theorem optSwapConstGet: forall (f: int -> int) (e: Expr),
    eval f (swapConstGet e) = eval f e.
  Proof.
    move=> f.
    do 3?case=> //=.
    case=> i //=.
    case=> //=.
    case=> i' //=.
    by rewrite addzC.
    case=> //=.
    case=> i //=.
    case=> //=.
    case=> i' //=.
    by rewrite mulzC.
    case=> //=.
    case=> i' //=.
    case f => n //=.
    by rewrite mulnC.
  Qed.

  Theorem optSwapConstDown: forall (f: int -> int) (e: Expr),
    eval f (swapConstDown e) = eval f e.
  Proof.
    move=> f.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> i //=.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> //=.
    move=> i' e.
    by rewrite addzA [X in addz X _]addzC addzA.
    case=> //=.
    case=> i //=.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> //=.
    move=> i' e.
    by rewrite mulzA [X in mulz X _]mulzC mulzA.
  Qed.

  Theorem optRotateBinary: forall (f: int -> int) (e: Expr),
    eval f (rotateBinary e) = eval f e.
  Proof.
    move=> f.
    case=> //=.
    case=> //=.
    case=> //=.
    case=> a b c //=.
    by rewrite addzA.
    case=> //=.
    case=> a b c //=.
    by rewrite mulzA.
  Qed.
End Theorems.
