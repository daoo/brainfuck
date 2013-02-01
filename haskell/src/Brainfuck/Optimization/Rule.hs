{-# LANGUAGE LambdaCase #-}
module Brainfuck.Optimization.Rule where

import Brainfuck.Data.Expr
import Control.Applicative
import Data.Maybe

perhaps :: (a -> Maybe a) -> a -> a
perhaps f a = fromMaybe a (f a)

type Rule a = Maybe a

class Ruled a where
  descend :: (a -> Rule a) -> a -> Rule a

rules :: Ruled a => [a -> Rule a] -> a -> Rule a
rules fs e = go fs Nothing
  where
    go [] acc     = acc
    go (f:fs') acc = case acc of
      Nothing -> go fs' (descend f e)
      Just e' -> go fs' (descend f e' <|> acc)

loop :: Ruled a => [a -> Rule a] -> a -> Rule a
loop fs a = case rules fs a of
  Nothing -> Nothing
  Just a' -> loop fs a' <|> Just a'

instance Ruled Expr where
  descend f e = case e of
    Value _ -> Nothing

    UnaryOp op a -> case descend f a of
      Nothing -> f e
      Just a' -> f (UnaryOp op a') <|> Just (UnaryOp op a')

    BinaryOp op a b -> case (descend f a, descend f b) of
      (Nothing, Nothing) -> f e
      (Just a', Nothing) -> f (BinaryOp op a' b) <|> Just (BinaryOp op a' b)
      (Nothing, Just b') -> f (BinaryOp op a b') <|> Just (BinaryOp op a b')
      (Just a', Just b') -> f (BinaryOp op a' b') <|> Just (BinaryOp op a' b')
