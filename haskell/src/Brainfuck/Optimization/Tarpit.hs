{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Tarpit where

import Brainfuck.Data.Expr
import Brainfuck.Data.Tarpit
import Brainfuck.Optimization.Analysis
import Brainfuck.Optimization.Rewriting
import Data.Monoid

reflectiveAssign :: Tarpit -> Rule Tarpit
reflectiveAssign (Instruction (Assign d1 (Expr 0 [(1, Var d2)])) next) | d1 == d2 = return next
reflectiveAssign _                                                                = nope

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
flowConst (Flow (While (Expr 0 [])) _ next)     = return next
flowConst (Flow (While (Expr _ [])) inner next) = return $ Flow Forever inner next
flowConst (Flow (If (Expr 0 [])) _ next)        = return next
flowConst (Flow (If (Expr _ [])) inner next)    = return $ inner `mappend` next
flowConst _                                     = nope

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

    expr s' (Expr c v) = Expr c $ map (fmap ((+) (Var s'))) v

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
reduceCopyLoops (Flow (While (Expr 0 [(1, Var d)])) inner next) = do
  inner' <- copyLoop d inner
  return $ mappend (foldr f zero inner') next
  where
    zero      = Instruction (Assign d $ constant 0) Nop
    f (ds, n) = Instruction . Assign ds $ variable ds `add` variable' n d

reduceCopyLoops _ = nope

-- |Convert while loops that are only run once to if statements
-- TODO: the (Assign d (Const 0)) operation could be moved out of the if for futher optimization
whileToIf :: Tarpit -> Rule Tarpit
whileToIf (Flow (While e) inner next)
  | whileOnce e inner = return $ Flow (If e) inner next
whileToIf _           = nope
