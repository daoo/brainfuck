Require Import Expr.

Set Implicit Arguments.

Section AST_defs.
  Variable Int: Set.

  Inductive Function: Type :=
    | Assign: Int -> Expr -> Function
    | Shift: Int -> Function
    | PutChar: Expr -> Function
    | GetChar: Int -> Function.

  Inductive Control: Type :=
    | Forever: Control
    | Once: Control
    | Never: Control
    | If: Expr -> Control
    | While: Expr -> Control.

  Inductive AST: Type :=
    | Nop: AST
    | Instruction: Function -> AST -> AST
    | Flow: Control -> AST -> AST -> AST.
End AST_defs.
