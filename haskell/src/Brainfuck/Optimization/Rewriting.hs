{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rewriting
  ( Rewritable (..)
  , Rule
  ) where

import Brainfuck.Data.AST
import Brainfuck.Data.Expr
import Control.Applicative
import Control.Monad.State

type Rule a = Maybe a

class Rewritable a where
  rewrite :: [a -> Rule a] -> a -> Rule a

instance Rewritable Expr where
  rewrite fs expr = toRule $ runState (go expr) False
    where
      go = loop $ \case
        e@(Value _) -> return e

        OperateUnary op a -> OperateUnary op <$> go a >>= applyRules fs

        OperateBinary op a b -> OperateBinary op <$> go a <*> go b >>= applyRules fs

instance Rewritable AST where
  rewrite fs ast = toRule $ runState (go ast) False
    where
      go = loop $ \case
        Nop -> return Nop

        Instruction fun next -> Instruction fun <$> go next >>= applyRules fs

        Flow ctrl inner next -> Flow ctrl <$> go inner <*> go next >>= applyRules fs

toRule :: (a, Bool) -> Rule a
toRule (a, True) = Just a
toRule _         = Nothing

applyRules :: [a -> Rule a] -> a -> State Bool a
applyRules [] e     = return e
applyRules (f:fs) e = case f e of
  Nothing -> applyRules fs e
  Just e' -> put True >> applyRules fs e'

loop :: (a -> State Bool a) -> a -> State Bool a
loop f a = case runState (f a) False of
  (a', True) -> put True >> loop f a'
  (_, False) -> return a
