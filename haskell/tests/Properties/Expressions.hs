{-# LANGUAGE LambdaCase #-}
module Properties.Expressions where

import Brainfuck.Data.Expr

propExprEvalConst :: Int -> Bool
propExprEvalConst c = eval undefined (constant c) == c
