Require Import Aux Expr.
Require Import Basics ssreflect ssrnat ssrint ssrbool eqtype ssralg ssrfun.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section OptExpr_defs.
  Axiom IntMap : Set -> Set.
  Axiom empty : forall a, IntMap a.
  Axiom foldrWithKey' : forall (a b: Set), (int -> a -> b -> b) -> b -> IntMap a -> b.
  Axiom insertWith : forall (a: Set), (a -> a -> a) -> int -> a -> IntMap a -> IntMap a.

  Inductive Analysis: Type := Data: int -> IntMap int -> Analysis.

  Fixpoint clean (expr: Expr) : Expr :=
    match expr with
    | Add a (Const (Posz 0)) => a
    | Add a b                => Add a (clean b)
    | e                      => e
    end.

  Definition toExpr (analysis: Analysis) : Expr :=
    let f := fun d n expr =>
      match d, n with
      | _, (Posz 0) => expr
      | d, 1        => Add (Var d) expr
      | d, n        => Add (Mul n (Var d)) expr
      end in
    match analysis with
    | Data (Posz 0) vars => clean (foldrWithKey' f (Const (Posz 0)) vars)
    | Data num vars      => foldrWithKey' f (Const num) vars
    end.

  Fixpoint analyse' (factor: int) (expr: Expr) (num: int) (vars: IntMap int): Analysis :=
    match expr with
    | Const c => Data (addz num (mulz factor c)) vars
    | Var d   => Data num (insertWith addz d factor vars)

    | Add a b => match analyse' factor a num vars with Data num' vars' => analyse' factor b num' vars' end
    | Mul a b => analyse' (mulz factor a) b num vars
    end.

  Fixpoint analyse (expr: Expr) : Analysis :=
    analyse' (Posz 1) expr (Posz 0) (empty int).

  Definition simplify (expr: Expr) : Expr :=
    toExpr (analyse expr).
End OptExpr_defs.
