{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Rewriting
import Data.Monoid
import qualified Data.IntMap as M

reflectiveAssign :: Tarpit -> Rule Tarpit
reflectiveAssign (Instruction (Assign d1 e) next) = case varAnalysis e of

  Just d2 | d1 == d2 -> return next
  _                  -> nope

reflectiveAssign _ = nope

shiftZero :: Tarpit -> Rule Tarpit
shiftZero (Instruction (Shift 0) next) = return next
shiftZero _                            = nope

flowInnerNop :: Tarpit -> Rule Tarpit
flowInnerNop (Flow _ Nop next) = return next
flowInnerNop _                 = nope

flowNever :: Tarpit -> Rule Tarpit
flowNever (Flow Never _ next) = return next
flowNever _                   = nope

flowOnce :: Tarpit -> Rule Tarpit
flowOnce (Flow Once inner next) = return $ inner `mappend` next
flowOnce _                      = nope

flowConst :: Tarpit -> Rule Tarpit
flowConst (Flow (While e) inner next) = case constAnalysis e of
  Just 0  -> return next
  Just _  -> return $ Flow Forever inner next
  Nothing -> nope
flowConst (Flow (If e) inner next) = case constAnalysis e of
  Just 0  -> return next
  Just _  -> return $ inner `mappend` next
  Nothing -> nope
flowConst _   = nope

movePut :: Tarpit -> Rule Tarpit
movePut (Instruction s@(Assign d e1) (Instruction (PutChar e2) next)) =
  return $ Instruction (PutChar (inlineExpr d e1 e2)) (Instruction s next)
movePut _   = nope

moveShifts :: Tarpit -> Rule Tarpit
moveShifts (Instruction (Shift s) next) = case next of
  Instruction fun next' -> return $ case fun of

    GetChar d  -> Instruction (GetChar (s + d))           $ shift next'
    PutChar e  -> Instruction (PutChar (expr s e))        $ shift next'
    Assign d e -> Instruction (Assign (s + d) (expr s e)) $ shift next'
    Shift s'   -> Instruction (Shift (s + s')) next'

  Flow ctrl inner next' -> return $
    Flow (control s ctrl) (mapTarpit (function s) (control s) inner) $
      Instruction (Shift s) next'

  Nop -> fail (show Nop)

  where
    shift = Instruction (Shift s)

    expr s' = mapVars (M.mapKeysMonotonic (+s'))

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

moveShifts _ = nope

-- |Reduce multiplications and clear loops
reduceCopyLoops :: Tarpit -> Rule Tarpit
reduceCopyLoops (Flow (While e) inner next) = do
  loop   <- varAnalysis e
  inner' <- copyLoop loop inner
  return $ mappend (foldr (f loop) (zero loop) inner') next
  where
    zero loop      = Instruction (Assign loop $ constant 0) Nop
    f loop (ds, n) = Instruction . Assign ds $ variables [(ds, 1), (loop, n)]

reduceCopyLoops _ = nope

-- |Convert while loops that are only run once to if statements
-- TODO: the (Assign d (Const 0)) operation could be moved out of the if for futher optimization
whileToIf :: Tarpit -> Rule Tarpit
whileToIf (Flow (While e) inner next)
  | whileOnce e inner = return $ Flow (If e) inner next
whileToIf _           = nope
