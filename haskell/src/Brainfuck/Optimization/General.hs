{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.General where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Expression
import Brainfuck.Optimization.Rule
import qualified Data.Set as S

-- |Optimize expressions
optimizeExpressions :: AST -> AST
optimizeExpressions = mapAST function control
  where
    function = \case
      Set d e   -> Set d $ perhaps (loop exprRules) e
      PutChar e -> PutChar $ perhaps (loop exprRules) e
      x         -> x

    control = \case
      If e    -> If $ perhaps (loop exprRules) e
      While e -> While $ perhaps (loop exprRules) e
      x       -> x

-- |Remove instructions that provides no side effects
cleanUp :: AST -> AST
cleanUp = \case
  Nop -> Nop

  Instruction fun next -> case fun of

    Set d1 (Value (Get d2)) | d1 == d2 -> cleanUp next
    Shift s                 | s == 0   -> cleanUp next
    _                                  -> Instruction fun (cleanUp next)

  Flow _ Nop next      -> cleanUp next
  Flow Never _ next    -> cleanUp next
  Flow Once inner next -> cleanUp inner `join` cleanUp next

  Flow (While (Value (Const i))) inner next | i == 0    -> cleanUp next
                                            | otherwise -> Flow Forever inner next

  Flow (If (Value (Const i))) inner next | i == 0    -> cleanUp next
                                         | otherwise -> cleanUp inner `join` cleanUp next

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

  ast@(Instruction (Shift s) next) -> case next of
    Nop -> ast

    Instruction fun next' -> case fun of

      GetChar d -> Instruction (GetChar (s + d)) (cont s next')
      PutChar e -> Instruction (PutChar (expr s e)) (cont s next')
      Set d e   -> Instruction (Set (s + d) (expr s e)) (cont s next')
      Shift s'  -> moveShifts $ Instruction (Shift (s + s')) next'

    Flow ctrl inner next' -> Flow (control s ctrl) (mapAST (function s) (control s) inner) (cont s next')

  Instruction fun next -> Instruction fun (moveShifts next)
  Flow ctrl inner next -> Flow ctrl (moveShifts inner) (moveShifts next)

  where
    cont s next = Instruction (Shift s) next

    function s = \case
      GetChar d     -> GetChar (s + d)
      PutChar e     -> PutChar (expr s e)
      Set d e       -> Set (s + d) (expr s e)
      fun@(Shift _) -> fun

    control s = \case
      If e    -> If (expr s e)
      While e -> While (expr s e)
      ctrl    -> ctrl

    expr s = modifyValues (\case
      Get d -> Value $ Get (s + d)
      e     -> Value e)

-- |Reduce multiplications and clear loops
reduceCopyLoops :: AST -> AST
reduceCopyLoops = \case
  Nop                  -> Nop
  Instruction fun next -> Instruction fun (reduceCopyLoops next)

  Flow ctrl@(While (Value (Get d))) inner next -> case copyLoop d inner of
    Nothing -> Flow ctrl (reduceCopyLoops inner) (reduceCopyLoops next)
    Just x  -> let instr = Instruction (Set d $ mkInt 0) Nop
                   x'    = foldr Instruction instr $ map (f d) x
                in x' `join` (reduceCopyLoops next)

  Flow ctrl inner next -> Flow ctrl (reduceCopyLoops inner) (reduceCopyLoops next)

  where
    f d (ds, v) = Set ds $ mkGet ds `add` (mkInt v `mul` mkGet d)

-- |Convert while loops that are only run once to if statements
whileToIf :: AST -> AST
whileToIf = \case
  Nop                  -> Nop
  Instruction fun next -> Instruction fun (whileToIf next)

  Flow ctrl@(While e@(Value (Get d))) inner next -> if setToZero d inner
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
    inl s = modifyValues (\case
      Get i | S.member i s -> mkGet i
            | otherwise    -> mkInt 0
      e                    -> Value e)
