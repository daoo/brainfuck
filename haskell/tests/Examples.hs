module Examples where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr

exCopyLoop1 :: AST
exCopyLoop1 =
  Flow (While $ Get 0)
    (Instruction (Set 0 (Get 0 `Add` Const (-1))) Nop)
  Nop

exCopyLoop2 :: AST
exCopyLoop2 =
  Flow (While (Get 5))
    (Instruction (Set 5 $ Get 5 `Add` Const (-1))
    (Instruction (Set 1 $ Get 1 `Add` Const 1)
    (Instruction (Set 2 $ Get 2 `Add` Const 5)
    (Instruction (Set 3 $ Get 3 `Add` Const 10) Nop))))
  Nop

exNotCopyLoop1 :: AST
exNotCopyLoop1 =
  Flow (While $ Get 5)
    (Instruction (Set 5 $ Get 5 `Add` Const (-1))
    (Instruction (Set 6 $ Get 5 `Add` Const 10)
    Nop))
  Nop

exShiftLoop1 :: AST
exShiftLoop1 = Instruction (Set 0 $ Get 0 `Add` Const 10)
  (Instruction (Set 1 $ Const 0)
  (Instruction (Set 2 $ Get 2 `Add` Const 4 `Add` Get 0)
  (Instruction (Set 3 $ Get 3 `Add` Const 5)
  (Flow (While $ Get 3) (Instruction (Shift (-1)) Nop)
  Nop))))
