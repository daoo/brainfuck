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
    Value _ -> Nothing

    UnaryOp op a -> case descend f a of
      Nothing -> f e
      Just a' -> try f (pure $ UnaryOp op a')

    BinaryOp op a b -> case (descend f a, descend f b) of
      (Nothing, Nothing) -> f e
      (Just a', Nothing) -> try f (pure $ BinaryOp op a' b)
      (Nothing, Just b') -> try f (pure $ BinaryOp op a b')
      (Just a', Just b') -> try f (pure $ BinaryOp op a' b')
