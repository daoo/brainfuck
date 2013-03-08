Require Import Basics ZArith Omega.
Set Implicit Arguments.

Section Expr_defs.
  Open Scope Z_scope.

  Inductive Value: Type :=
    | Get: Z -> Value
    | Const: Z -> Value.

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

  Definition int := compose Return Const.
  Definition get := compose Return Get.
  Definition add := OperateBinary Add.
  Definition mul := OperateBinary Mul.

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

  Definition inlineExpr (d1: Z) (this into: Expr) : Expr :=
    unfold OperateUnary OperateBinary (fun v => match v with
      | Get d2 => if Z.eqb d1 d2 then this else Return (Get d2)
      | v      => Return v
    end) into.

  Definition eval (f: Z -> Z) (expr: Expr) : Z :=
    unfold
      (fun op a => match op with
        | Id     => a
        | Negate => Z.opp a
      end)
      (fun op a b => match op with
        | Add => a + b
        | Mul => a * b
      end)
      (fun v => match v with
        | Const i => i
        | Get i   => f i
      end)
      expr.

  Theorem add0e: forall (f: Z -> Z) (e: Expr),
    eval f (OperateBinary Add (Return (Const 0)) e) = eval f e.
  Proof. unfold eval; unfold unfold; reflexivity. Qed.

  Theorem adde0: forall (f: Z -> Z) (e: Expr),
    eval f (OperateBinary Add e (Return (Const 0))) = eval f e.
  Proof. intros f e; unfold eval; unfold unfold; rewrite Z.add_comm; reflexivity. Qed.
End Expr_defs.
