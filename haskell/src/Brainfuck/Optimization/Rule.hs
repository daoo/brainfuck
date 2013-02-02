{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rule where

import Brainfuck.Data.Expr
import Control.Applicative
import Data.Maybe

try :: (Monad f, Alternative f) => (a -> f a) -> f a -> f a
try f a = (f =<< a) <|> a

tryMaybe :: (a -> Maybe a) -> a -> a
tryMaybe f a = fromMaybe a (f a)

type Rule a = Maybe a

class Ruled a where
  descend :: (a -> Rule a) -> a -> Rule a

rules :: Ruled a => [a -> Rule a] -> a -> Rule a
rules fs e = foldl (\acc f -> descend f (fromMaybe e acc) <|> acc) empty fs

loop :: Ruled a => [a -> Rule a] -> a -> Rule a
loop fs a = try (loop fs) (rules fs a)

instance Ruled Expr where
  descend f e = case e of
    Value _         -> Nothing
    UnaryOp op a    -> try f $ pure $ UnaryOp op (df a)
    BinaryOp op a b -> try f $ pure $ BinaryOp op (df a) (df b)
    where
      df = tryMaybe (descend f)
