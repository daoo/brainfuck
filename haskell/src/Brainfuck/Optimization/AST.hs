{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.AST where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Expr
import Brainfuck.Optimization.Rewriting

expressions :: AST -> AST
expressions = \case
  Instruction (Assign d e) next -> Instruction (Assign d (simplify e)) next
  Instruction (PutChar e) next  -> Instruction (PutChar (simplify e)) next
  Flow (If e) inner next        -> Flow (If (simplify e)) inner next
  Flow (While e) inner next     -> Flow (While (simplify e)) inner next
  ast                           -> ast

reflectiveAssign :: AST -> Rule AST
reflectiveAssign (Instruction (Assign d1 (Var d2)) next) | d1 == d2 = return next
reflectiveAssign ast                                                = fail (show ast)

shiftZero :: AST -> Rule AST
shiftZero (Instruction (Shift 0) next) = return next
shiftZero ast                          = fail (show ast)

flowInnerNop :: AST -> Rule AST
flowInnerNop (Flow _ Nop next) = return next
flowInnerNop ast               = fail (show ast)

flowNever :: AST -> Rule AST
flowNever (Flow Never _ next) = return next
flowNever ast                 = fail (show ast)

flowOnce :: AST -> Rule AST
flowOnce (Flow Once inner next) = return $ inner `join` next
flowOnce ast                    = fail (show ast)

flowConst :: AST -> Rule AST
flowConst (Flow (While (Const i)) inner next)
  | i == 0    = return next
  | otherwise = return $ Flow Forever inner next
flowConst (Flow (If (Const i)) inner next)
  | i == 0    = return next
  | otherwise = return $ inner `join` next
flowConst ast = fail (show ast)

movePut :: AST -> Rule AST
movePut (Instruction s@(Assign d e1) (Instruction (PutChar e2) next)) =
  return $ Instruction (PutChar (inlineExpr d e1 e2)) (Instruction s next)
movePut ast = fail (show ast)

moveShifts :: AST -> Rule AST
moveShifts (Instruction (Shift s) next) = case next of
  Instruction fun next' -> return $ case fun of

    GetChar d  -> Instruction (GetChar (s + d))           $ shift next'
    PutChar e  -> Instruction (PutChar (expr s e))        $ shift next'
    Assign d e -> Instruction (Assign (s + d) (expr s e)) $ shift next'
    Shift s'   -> Instruction (Shift (s + s')) next'

  Flow ctrl inner next' -> return $
    Flow (control s ctrl) (mapAST (function s) (control s) inner) $
      Instruction (Shift s) next'

  Nop -> fail (show Nop)

  where
    shift = Instruction (Shift s)

    expr s' = modifyVars (Var . (+s'))

    function s' = \case
      GetChar d  -> GetChar (s' + d)
      PutChar e  -> PutChar (expr s' e)
      Assign d e -> Assign (s' + d) (expr s' e)
      Shift s''  -> Shift s''

    control s' = \case
      Forever -> Forever
      Once    -> Once
      Never   -> Never
      If e    -> If $ expr s' e
      While e -> While $ expr s' e

moveShifts ast = fail (show ast)

-- |Reduce multiplications and clear loops
reduceCopyLoops :: AST -> Rule AST
reduceCopyLoops (Flow (While (Var d)) inner next) = do
  x <- copyLoop d inner

  let instr = Instruction (Assign d $ Const 0) Nop

      f d' (ds, v) = Assign ds $ (Var ds) `Add` (v `Mul` (Var d'))

      x' = foldr Instruction instr $ map (f d) x

  return $ x' `join` next

reduceCopyLoops ast = fail (show ast)

-- |Convert while loops that are only run once to if statements
-- TODO: the (Assign d (Const 0)) operation could be moved out of the if for futher optimization
whileToIf :: AST -> Rule AST
whileToIf (Flow (While e) inner next)
  | whileOnce e inner = return $ Flow (If e) inner next
whileToIf ast = fail (show ast)
