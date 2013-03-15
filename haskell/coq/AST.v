Require Import Expr.
Require Import Basics ssreflect ssrnat ssrint ssrbool eqtype ssralg ssrfun.
Import intZmod intRing.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section AST_defs.
  Inductive Function: Type :=
    | Assign: int -> Expr -> Function
    | Shift: int -> Function
    | PutChar: Expr -> Function
    | GetChar: int -> Function.

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

  Fixpoint initAST (ast: AST) : AST :=
    match ast with
    | Nop                  => Nop
    | Instruction _ Nop    => Nop
    | Flow _ Nop _         => Nop
    | Flow _ _ Nop         => Nop
    | Instruction f next   => Instruction f (initAST next)
    | Flow ctrl inner next => Flow ctrl (initAST inner) (initAST next)
    end.
End AST_defs.
