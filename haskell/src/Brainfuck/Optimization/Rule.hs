{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rule where

import Brainfuck.Data.Expr
import Control.Applicative
import Data.Maybe

try :: (Monad f, Alternative f) => (a -> f a) -> f a -> f a
try f a = (f =<< a) <|> a

tryMaybe :: (a -> Maybe a) -> a -> a
tryMaybe f a = fromMaybe a (f a)

toRule :: (Bool, a) -> Rule a
toRule (True, a) = Just a
toRule _         = Nothing

fromRule :: a -> Rule a -> (Bool, a)
fromRule _ (Just b) = (True, b)
fromRule a Nothing  = (False, a)

type Rule a = Maybe a

class Ruled a where
  rewrite :: [a -> Rule a] -> a -> Rule a

applyRules :: [a -> Rule a] -> a -> (Bool, a)
applyRules rules expr = go rules (False, expr)
  where
    go [] acc         = acc
    go (f:fs) (b, e') = let (b', e'') = fromRule e' (f e')
                         in go fs (b || b', e'')

loop :: Ruled a => [a -> Rule a] -> a -> Rule a
loop fs a = try (loop fs) (rewrite fs a)

instance Ruled Expr where
  rewrite fs = toRule . go

    where
      go = \case
        e@(Value _) -> (False, e)

        UnaryOp op a -> let (m1, a') = go a
                            (m2, e)  = applyRules fs (UnaryOp op a')
                         in (m1 || m2, e)

        BinaryOp op a b -> let (m1, a') = go a
                               (m2, b') = go b
                               (m3, e)  = applyRules fs (BinaryOp op a' b')
                            in (m1 || m2 || m3, e)
