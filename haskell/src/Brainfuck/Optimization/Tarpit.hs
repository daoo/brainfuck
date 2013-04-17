{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Brainfuck.Utility
import Data.Monoid
import qualified Data.IntMap as M

flowReduction :: Tarpit -> Tarpit
flowReduction = \case
  Flow _     Nop  next -> flowReduction next
  Flow Never _    next -> flowReduction next
  Flow Once inner next -> flowReduction $ inner `mappend` next

  Flow (While (Const 0)) _     next -> flowReduction next
  Flow (If    (Const 0)) _     next -> flowReduction next
  Flow (While (Const _)) inner next -> Flow Forever (flowReduction inner) (flowReduction next)
  Flow (If    (Const _)) inner next -> flowReduction $ inner `mappend` next

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (flowReduction next)
  Flow ctrl inner next -> Flow ctrl (flowReduction inner) (flowReduction next)

whileToIf :: Tarpit -> Tarpit
whileToIf = \case
  Flow ctrl@(While e@(Var 1 d (Const 0))) inner next -> case whileOnce d (whileToIf inner) of

    Just inner' -> Flow (If e) inner' (Instruction (Assign d $ Const 0) (whileToIf next))
    Nothing     -> Flow ctrl (whileToIf inner) (whileToIf next)

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (whileToIf next)
  Flow ctrl inner next -> Flow ctrl (whileToIf inner) (whileToIf next)

copyLoopReduction :: Tarpit -> Tarpit
copyLoopReduction = \case
  Flow ctrl@(While (Var 1 d (Const 0))) inner next -> case copyLoop d inner of

    Just inner'' -> inner'' `mappend` (copyLoopReduction next)
    Nothing      -> Flow ctrl (copyLoopReduction inner) (copyLoopReduction next)

  Nop                  -> Nop
  Instruction fun next -> Instruction fun (copyLoopReduction next)
  Flow ctrl inner next -> Flow ctrl (copyLoopReduction inner) (copyLoopReduction next)

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

unrollLoops :: Int -> Tarpit -> Tarpit
unrollLoops n = go
  where
    rep 0 _ b = b
    rep m f b = f $ rep (m - 1) f b

    go = \case
      Flow (While e) inner next -> rep n (Flow (If e) (go inner)) (go next)

      Nop                  -> Nop
      Flow ctrl inner next -> Flow ctrl (go inner) (go next)
      Instruction fun next -> Instruction fun $ go next

movePut :: Tarpit -> Tarpit
movePut = \case
  Nop -> Nop

  Instruction fun@(Assign d e1) next -> case movePut next of
    Instruction (PutChar e2) next' ->
      Instruction (PutChar $ insertExpression d e1 e2) $ movePut $ Instruction fun next'

    next' -> Instruction fun next'

  Instruction fun next -> Instruction fun (movePut next)
  Flow ctrl inner next -> Flow ctrl (movePut inner) (movePut next)

inlineConstants :: Tarpit -> Tarpit
inlineConstants = go M.empty
  where
    go m = \case
      Nop -> Nop

      Instruction fun next -> case fun of

        GetChar d          -> Instruction fun                   $ go (M.delete d m) next
        PutChar e          -> Instruction (PutChar $ expr e m)  $ go m next
        Assign d (Const c) -> Instruction fun                   $ go (M.insert d c m) next
        Shift _            -> Instruction fun                   $ go M.empty next

        Assign d e -> case expr e m of

          e'@(Const c) -> Instruction (Assign d e') $ go (M.insert d c m) next
          e'           -> Instruction (Assign d e') $ go (M.delete d m) next

      Flow ctrl inner next -> case ctrl of
        Forever -> Flow ctrl            (go M.empty inner) (go M.empty next)
        Once    -> Flow ctrl            (go m inner)       (go M.empty next)
        Never   -> Flow ctrl            (go m inner)       (go m next)
        If e    -> Flow (If (expr e m)) (go m inner)       (go M.empty next)
        While _ -> Flow ctrl            (go M.empty inner) (go M.empty next)

    expr = M.foldrWithKey' insertConstant
