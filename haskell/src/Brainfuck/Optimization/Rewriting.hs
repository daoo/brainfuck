{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rewriting
  ( Rewritable (..)
  , Rule
  ) where

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

        UnaryOp op a -> UnaryOp op <$> go a >>= applyRules fs

        BinaryOp op a b -> BinaryOp op <$> go a <*> go b >>= applyRules fs

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
