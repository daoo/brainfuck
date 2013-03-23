{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rewriting
  ( Rewritable (..)
  , Rule
  ) where

import Brainfuck.Data.AST
import Control.Applicative
import Control.Monad.State.Strict

type Rule a = Maybe a

class Rewritable a where
  rewrite :: [a -> Rule a] -> a -> Rule a
  once :: (a -> a) -> a -> a

instance Rewritable AST where
  rewrite fs ast = toRule $ runState (go ast) False
    where
      go = loop $ \case
        Nop -> return Nop

        Instruction fun next -> Instruction fun <$> go next >>= applyRules fs

        Flow ctrl inner next -> Flow ctrl <$> go inner <*> go next >>= applyRules fs

  once f = \case
    Nop                  -> Nop
    Instruction fun next -> f $ Instruction fun (once f next)
    Flow ctrl inner next -> f $ Flow ctrl (once f inner) (once f next)

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
