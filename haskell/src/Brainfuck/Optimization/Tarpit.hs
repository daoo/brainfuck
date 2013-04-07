{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Brainfuck.Utility
import Data.Monoid

flowReduction :: Tarpit -> Tarpit
flowReduction = \case
  Flow _ Nop next      -> flowReduction next
  Flow Never _ next    -> flowReduction next
  Flow Once inner next -> flowReduction $ inner `mappend` next

  Flow (While (Const 0)) _ next     -> flowReduction next
  Flow (If (Const 0)) _ next        -> flowReduction next
  Flow (While (Const _)) inner next -> Flow Forever (flowReduction inner) (flowReduction next)
  Flow (If (Const _)) inner next    -> flowReduction $ inner `mappend` next

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (flowReduction next)
  Flow ctrl inner next -> Flow ctrl (flowReduction inner) (flowReduction next)

loopReduction :: Tarpit -> Tarpit
loopReduction = \case
  -- TODO: the (Assign d (Const 0)) operation could be moved out of the if for futher optimization
  Flow (While e) inner next | whileOnce e inner ->
    Flow (If e) (loopReduction inner) (loopReduction next)

  Flow ctrl@(While (Var 1 d (Const 0))) inner next -> case go inner of
    Just inner' -> mappend inner' (loopReduction next)
    Nothing     -> Flow ctrl (loopReduction inner) (loopReduction next)
    where
      go = fmap (foldr f zero) . copyLoop d

      zero = Instruction (Assign d $ Const 0) Nop

      f (n, ds) = Instruction . Assign ds $ Var 1 ds (Const 0) `add` Var n d (Const 0)

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (loopReduction next)
  Flow ctrl inner next -> Flow ctrl (loopReduction inner) (loopReduction next)

shiftReduction :: Tarpit -> Tarpit
shiftReduction = \case
  Nop -> Nop

  Instruction (Shift 0) next -> shiftReduction next

  Instruction (Shift s) next -> case next of

    Nop                          -> Instruction (Shift s) Nop
    Instruction (Shift s') next' -> shiftReduction $ Instruction (Shift (s + s')) $ next'
    Instruction fun next'        -> Instruction (function s fun) $ shiftReduction $ Instruction (Shift s) next'

    Flow ctrl inner next' -> Flow (control s ctrl)
      (shiftReduction $ mapTarpit (function s) (control s) inner)
      (shiftReduction $ Instruction (Shift s) next')

  Instruction fun next -> Instruction fun (shiftReduction next)
  Flow ctrl inner next -> Flow ctrl (shiftReduction inner) (shiftReduction next)

  where
    function s = \case
      GetChar d   -> GetChar (d + s)
      PutChar e   -> PutChar (expr s e)
      Assign d e  -> Assign (d + s) (expr s e)
      x@(Shift _) -> x

    control s' = \case
      Forever -> Forever
      Once    -> Once
      Never   -> Never
      If e    -> If $ expr s' e
      While e -> While $ expr s' e

    expr s = mapExpr (mapSnd (+s)) id
