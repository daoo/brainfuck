{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.General where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis
import qualified Data.Set as S

-- |Optimize expressions
optimizeExpressions :: AST -> AST
optimizeExpressions = mapAST function control
  where
    function = \case
      Set d e   -> Set d $ optimizeExpr e
      PutChar e -> PutChar $ optimizeExpr e
      x         -> x

    control = \case
      If e    -> If $ optimizeExpr e
      While e -> While $ optimizeExpr e
      x       -> x

-- |Remove instructions that provides no side effects
cleanUp :: AST -> AST
cleanUp = \case
  Nop -> Nop

  Instruction fun next -> case fun of
    Set d1 (Get d2) | d1 == d2 -> cleanUp next
    Shift s         | s == 0   -> cleanUp next
    _                          -> Instruction fun (cleanUp next)

  Flow _ Nop next      -> cleanUp next
  Flow Never _ next    -> cleanUp next
  Flow Once inner next -> join inner next

  Flow (While (Const i)) inner next | i == 0    -> cleanUp next
                                    | otherwise -> Flow Forever inner next
  Flow (If (Const i)) inner next    | i == 0    -> cleanUp next
                                    | otherwise -> Flow Once inner next

  Flow ctrl inner next -> Flow ctrl (cleanUp inner) (cleanUp next)

movePutGet :: AST -> AST
movePutGet = \case
  Nop -> Nop

  Instruction s@(Set d e1) (Instruction (PutChar e2) next) ->
    Instruction (PutChar (inlineExpr d e1 e2)) (Instruction s (movePutGet next))

  Instruction fun next -> Instruction fun (movePutGet next)
  Flow ctrl inner next -> Flow ctrl (movePutGet inner) (movePutGet next)

-- |Move shift instructions towards the end.
-- This make most of the shift operations disappear
moveShifts :: AST -> AST
moveShifts = \case
  Nop -> Nop

  Instruction (Shift s) next -> case next of
    Nop -> Nop

    Instruction instr next' -> case instr of

      Shift s' -> moveShifts $ Instruction (Shift (s + s')) next'
      fun      -> Instruction (function s fun) (moveShifts (Instruction (Shift s) next'))

    Flow ctrl inner next' -> moveShifts $ Flow ctrl (mapAST (function s) (control s) inner) (Instruction (Shift s) next')

  Instruction fun next -> Instruction fun (moveShifts next)
  Flow ctrl inner next -> Flow ctrl (moveShifts inner) (moveShifts next)

  where
    function s = \case
      Set d e   -> Set (s + d) (expr s e)
      Shift s'  -> Shift (s + s')
      GetChar d -> GetChar (s + d)
      PutChar e -> PutChar (expr s e)

    control s = \case
      If e    -> If (expr s e)
      While e -> While (expr s e)
      ctrl    -> ctrl

    expr s = modifyLeaves (\case
      Get d -> Get (s + d)
      e     -> e)

-- |Reduce multiplications and clear loops
reduceCopyLoops :: AST -> AST
reduceCopyLoops = \case
  Nop                  -> Nop
  Instruction fun next -> Instruction fun (reduceCopyLoops next)

  Flow ctrl@(While (Get d)) inner next -> case copyLoop d inner of
    Nothing -> Flow ctrl (reduceCopyLoops inner) (reduceCopyLoops next)
    Just x  -> let instr = Instruction (Set d $ Const 0) Nop
                   x'    = foldr Instruction instr $ map (f d) x
                in x' `join` (reduceCopyLoops next)

  Flow ctrl inner next -> Flow ctrl (reduceCopyLoops inner) (reduceCopyLoops next)

  where
    f d (ds, v) = Set ds $ Get ds `Add` (Const v `Mul` Get d)

-- |Convert while loops that are only run once to if statements
whileToIf :: AST -> AST
whileToIf = \case
  Nop                  -> Nop
  Instruction fun next -> Instruction fun (whileToIf next)

  Flow ctrl@(While e@(Get d)) inner next -> if setToZero d inner
    then Flow (If e) inner (whileToIf next)
    else Flow ctrl (whileToIf inner) (whileToIf next)

  Flow ctrl inner next -> Flow ctrl (whileToIf inner) (whileToIf next)

-- |Remove side effect free instructions from the end
removeFromEnd :: AST -> AST
removeFromEnd = go
  where
    go = \case
      x@(Instruction fun next) | helper x -> Instruction fun (go next)
      x@(Flow ctrl inner next) | helper x -> Flow ctrl inner (go next)

      _ -> Nop

    helper = \case
      Nop                  -> False
      Instruction fun next -> sideEffect fun || helper next
      Flow _ _ _           -> True -- TODO: Investigate when flow control statements are side effect free

    sideEffect = \case
      PutChar _ -> True
      _         -> False

-- |Inline initial zeroes
inlineZeros :: AST -> AST
inlineZeros = go S.empty
  where
    go :: S.Set Int -> AST -> AST
    go s = \case
      Nop -> Nop

      flow@(Flow _ _ _) -> flow

      Instruction fun next -> case fun of
        Set i e   -> Instruction (Set i (inl s e)) (go (S.insert i s) next)
        PutChar e -> Instruction (PutChar (inl s e)) (go s next)
        GetChar d -> Instruction fun (go (S.delete d s) next)
        Shift _   -> Instruction fun next

    inl :: S.Set Int -> Expr -> Expr
    inl s = unfold Add Mul (\case
      Get i | S.member i s -> Get i
            | otherwise    -> Const 0
      e                    -> e)
