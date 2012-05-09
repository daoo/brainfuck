module Brainfuck.Compiler.Analyzer where

import Data.List

import Brainfuck.Compiler.Expr
import Brainfuck.Compiler.IL

data Temp = TempLoop [Temp]
          | Delta Int
  deriving (Show)

memoryRequired :: [IL] -> [Temp]
memoryRequired = undefined

ilCount :: [IL] -> Int
ilCount = sum . map f
  where
    f (Loop _ ils) = 1 + ilCount ils
    f _            = 1

loopDepth :: IL -> Int
loopDepth (Loop _ ils) = (+1) $ maximum $ map loopDepth ils
loopDepth _            = 0

exprDepends :: Int -> Expr -> Bool
exprDepends i (Get d)    = i == d
exprDepends i (Add exs)  = any (exprDepends i) exs
exprDepends i (Mult exs) = any (exprDepends i) exs
exprDepends _ _          = False

copyLoop :: IL -> Maybe [(Int, Int)]
copyLoop (Loop o loop) = helper
  where
    helper | null a || not (null d) = Nothing
           | otherwise              = Just $ map getConst c
      where
        (a, b) = partition (isDec o) loop
        (c, d) = partition isConstAdd b

    getConst (Set d (Add [Get _, Const c])) = (d, c)
    getConst (Set d (Add [Const c, Get _])) = (d, c)
    getConst _ = error "Not a const add"

    isConstAdd (Set d1 (Add [Get d2, Const _])) = d1 == d2
    isConstAdd (Set d1 (Add [Const _, Get d2])) = d1 == d2
    isConstAdd _                                = False

    isDec d1 (Set d2 (Add [Get d3, Const (-1)])) = d1 == d2 && d1 == d3
    isDec d1 (Set d2 (Add [Const (-1), Get d3])) = d1 == d2 && d1 == d3
    isDec _ _                                    = False

copyLoop _ = Nothing
