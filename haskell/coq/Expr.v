Require Import Aux.
Require Import Basics ssreflect ssrnat ssrint ssrbool eqtype ssralg ssrfun.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Expr_defs.
  Inductive Expr: Type :=
    | Const: int -> Expr
    | Var: int -> Expr
    | Add: Expr -> Expr -> Expr
    | Mul: int -> Expr -> Expr.

  Fixpoint unfold (A: Type)
    (add: A -> A -> A)
    (mul: int -> A -> A)
    (cst: int -> A)
    (var: int -> A)
    (expr: Expr) : A :=
    match expr with
    | Const a => cst a
    | Var a   => var a
    | Add a b => add (unfold add mul cst var a) (unfold add mul cst var b)
    | Mul a b => mul a (unfold add mul cst var b)
    end.

  Definition inlineExpr (d1: int) (this into: Expr) : Expr :=
    unfold Add Mul Const (fun d2 => if eqz d1 d2 then this else Var d2) into.

  Definition modifyVars (f: int -> Expr) (expr: Expr) : Expr :=
    unfold Add Mul Const f expr.

  Fixpoint eval (f: int -> int) (expr: Expr) : int :=
    match expr with
    | Const a => a
    | Var a   => f a
    | Add a b => addz (eval f a) (eval f b)
    | Mul a b => mulz a (eval f b)
    end.

  Definition eval' (f: int -> int) (expr: Expr) : int :=
    unfold addz mulz id f expr.
End Expr_defs.
