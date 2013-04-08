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
shiftReduction = go 0 0
  where
    expr s = mapExpr (mapSnd (+s)) id

    go s t = \case
      Nop -> if t /= 0 then Instruction (Shift t) Nop else Nop

      Instruction fun next -> case fun of

        GetChar d  -> Instruction (GetChar (d + s))           $ go s t next
        PutChar e  -> Instruction (PutChar (expr s e))        $ go s t next
        Assign d e -> Instruction (Assign (d + s) (expr s e)) $ go s t next
        Shift s'   -> go (s + s') (t + s') next

      Flow ctrl inner next -> case ctrl of
        Forever -> Flow Forever            (go s 0 inner) (go s t next)
        Once    -> Flow Once               (go s 0 inner) (go s t next)
        Never   -> Flow Never              (go s 0 inner) (go s t next)
        If e    -> Flow (If (expr s e))    (go s 0 inner) (go s t next)
        While e -> Flow (While (expr s e)) (go s 0 inner) (go s t next)
