{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.AST where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Expr
import Brainfuck.Optimization.Rewriting
import qualified Data.Set as S

astRules :: [AST -> Rule AST]
astRules = [ reflectiveSet
           , expressions
           , shiftZero
           , flowInnerNop
           , flowNever
           , flowOnce
           , flowConst
           , movePut
           , moveShifts
           , reduceCopyLoops
           -- , whileToIf
           ]

expressions :: AST -> Rule AST
expressions (Instruction (Set d e) next) = do
  e' <- rewrite exprRules e
  return $ Instruction (Set d e') next
expressions (Instruction (PutChar e) next) = do
  e' <- rewrite exprRules e
  return $ Instruction (PutChar e') next
expressions (Flow (If e) inner next) = do
  e' <- rewrite exprRules e
  return $ Flow (If e') inner next
expressions (Flow (While e) inner next) = do
  e' <- rewrite exprRules e
  return $ Flow (While e') inner next
expressions ast = fail (show ast)

reflectiveSet :: AST -> Rule AST
reflectiveSet (Instruction (Set d1 (Value (Get d2))) next) | d1 == d2 = return next
reflectiveSet ast                                                     = fail (show ast)

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
flowConst (Flow (While (Value (Const i))) inner next) | i == 0    = return next
                                                      | otherwise = return $ Flow Forever inner next
flowConst (Flow (If (Value (Const i))) inner next)    | i == 0    = return next
                                                      | otherwise = return $ inner `join` next
flowConst ast                                                     = fail (show ast)

movePut :: AST -> Rule AST
movePut (Instruction s@(Set d e1) (Instruction (PutChar e2) next)) =
  return $ Instruction (PutChar (inlineExpr d e1 e2)) (Instruction s next)
movePut ast = fail (show ast)

moveShifts :: AST -> Rule AST
moveShifts (Instruction (Shift s) (Instruction fun next)) = case fun of

  GetChar d -> return $ Instruction (GetChar (s + d))        $ Instruction (Shift s) next
  PutChar e -> return $ Instruction (PutChar (expr s e))     $ Instruction (Shift s) next
  Set d e   -> return $ Instruction (Set (s + d) (expr s e)) $ Instruction (Shift s) next
  Shift s'  -> return $ Instruction (Shift (s + s')) next

  where
    expr s' = modifyValues (\case
      Get d -> Value $ Get (s' + d)
      v     -> Value v)

moveShifts ast = fail (show ast)

-- |Reduce multiplications and clear loops
reduceCopyLoops :: AST -> Rule AST
reduceCopyLoops (Flow (While (Value (Get d))) inner next) = do
  x <- copyLoop d inner

  let instr = Instruction (Set d $ mkInt 0) Nop

      f d' (ds, v) = Set ds $ mkGet ds `add` (mkInt v `mul` mkGet d')

      x' = foldr Instruction instr $ map (f d) x

  return $ x' `join` next

reduceCopyLoops ast = fail (show ast)

-- |Convert while loops that are only run once to if statements
whileToIf :: AST -> Rule AST
whileToIf ast@(Flow (While e@(Value (Get d))) inner next) =
  if setToZero d inner
    then return $ Flow (If e) inner next
    else fail (show ast)

whileToIf ast = fail (show ast)

-- |Inline initial zeroes
inlineZeros :: AST -> AST
inlineZeros = go S.empty
  where
    go :: S.Set Int -> AST -> AST
    go s = \case
      Instruction fun next -> case fun of
        Set i e   -> Instruction (Set i (inl s e)) (go (S.insert i s) next)
        PutChar e -> Instruction (PutChar (inl s e)) (go s next)
        GetChar d -> Instruction fun (go (S.delete d s) next)
        Shift _   -> Instruction fun next

      ast -> ast

    inl :: S.Set Int -> Expr -> Expr
    inl s = modifyValues (\case
      Get i | S.member i s -> mkGet i
            | otherwise    -> mkInt 0
      e                    -> Value e)
