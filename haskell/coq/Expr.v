Require Import ssreflect ssrnat ssrint ssrbool eqtype.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Expr_defs.
  Inductive Value: Type :=
    | Get: int -> Value
    | Const: int -> Value.

  Inductive UnaryOperator: Type :=
    | Id: UnaryOperator
    | Negate: UnaryOperator.

  Inductive BinaryOperator: Type :=
    | Add: BinaryOperator
    | Mul: BinaryOperator.

  Inductive Expr: Type :=
    | Return: Value -> Expr
    | OperateUnary: UnaryOperator -> Expr -> Expr
    | OperateBinary: BinaryOperator -> Expr -> Expr -> Expr.

  (*Definition int := compose Return Const.
  Definition get := compose Return Get.
  Definition add := OperateBinary Add.
  Definition mul := OperateBinary Mul.*)

  Fixpoint unfold (A: Type)
    (unary: UnaryOperator -> A -> A)
    (binary: BinaryOperator -> A -> A -> A)
    (value: Value -> A)
    (expr: Expr) : A :=
    match expr with
    | Return v             => value v
    | OperateUnary op a    => unary op (unfold unary binary value a)
    | OperateBinary op a b => binary op (unfold unary binary value a) (unfold unary binary value b)
    end.

  Definition inlineExpr (d1: int) (this into: Expr) : Expr :=
    unfold OperateUnary OperateBinary (fun v => match v with
      | Get d2 => if d1 == d2 then this else Return (Get d2)
      | v      => Return v
      end) into.

  Definition modifyValues (f: Value -> Expr) (e: Expr) : Expr :=
    unfold OperateUnary OperateBinary f e.

  Fixpoint eval (f: int -> int) (expr: Expr) : int :=
    match expr with
    | Return (Const i)      => i
    | Return (Get i)        => f i
    | OperateUnary Id a     => eval f a
    | OperateUnary Negate a => oppz (eval f a)
    | OperateBinary Add a b => addz (eval f a) (eval f b)
    | OperateBinary Mul a b => mulz (eval f a) (eval f b)
    end.

  Definition eval' (f: int -> int) (expr: Expr) : int :=
    unfold
      (fun op a => match op with
        | Id     => a
        | Negate => oppz a
      end)
      (fun op a b => match op with
        | Add => addz a b
        | Mul => mulz a b
      end)
      (fun v => match v with
        | Const i => i
        | Get i   => f i
      end)
      expr.
End Expr_defs.
