{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Data.Monoid

flowReduction :: Tarpit -> Tarpit
flowReduction = \case
  Nop -> Nop

  Instruction fun next -> Instruction fun $ flowReduction next

  Flow _ Nop next      -> flowReduction next
  Flow Never _ next    -> flowReduction next
  Flow Once inner next -> flowReduction $ inner `mappend` next

  Flow (While (Expr 0 [])) _ next     -> flowReduction next
  Flow (If (Expr 0 [])) _ next        -> flowReduction next
  Flow (While (Expr _ [])) inner next -> Flow Forever (flowReduction inner) (flowReduction next)
  Flow (If (Expr _ [])) inner next    -> flowReduction $ inner `mappend` next

  Flow ctrl inner next -> Flow ctrl (flowReduction inner) (flowReduction next)

loopReduction :: Tarpit -> Tarpit
loopReduction = \case
  -- TODO: the (Assign d (Const 0)) operation could be moved out of the if for futher optimization
  Flow (While e) inner next | whileOnce e inner ->
    Flow (If e) (loopReduction inner) (loopReduction next)

  Flow ctrl@(While (Expr 0 [(1, Var d)])) inner next -> case go inner of
    Just inner' -> mappend inner' (loopReduction next)
    Nothing     -> Flow ctrl (loopReduction inner) (loopReduction next)
    where
      go = fmap (foldr f zero) . copyLoop d

      zero = Instruction (Assign d $ constant 0) Nop

      f (Mult n, Var ds) = Instruction . Assign ds $ variable ds `add` variable' n d

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (loopReduction next)
  Flow ctrl inner next -> Flow ctrl (loopReduction inner) (loopReduction next)

shiftReduction :: Tarpit -> Tarpit
shiftReduction = \case
  Nop -> Nop

  Instruction (Shift 0) next -> shiftReduction next

  Instruction (Shift s) next -> case next of
    Nop -> Nop

    Instruction fun next' -> case fun of

      GetChar d  -> Instruction (GetChar (s + d))           $ ins s next'
      PutChar e  -> Instruction (PutChar (expr s e))        $ ins s next'
      Assign d e -> Instruction (Assign (s + d) (expr s e)) $ ins s next'
      Shift s'   -> shiftReduction $ Instruction (Shift (s + s')) $ next'

    Flow ctrl inner next' -> Flow (control s ctrl)
      (shiftReduction $ mapTarpit (function s) (control s) inner)
      (shiftReduction $ Instruction (Shift s) next')

  Instruction fun next -> Instruction fun (shiftReduction next)
  Flow ctrl inner next -> Flow ctrl (shiftReduction inner) (shiftReduction next)

  where
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

    ins s = shiftReduction . Instruction (Shift s)

    expr s (Expr c v) = Expr c $ map (fmap ((+) (Var s))) v
