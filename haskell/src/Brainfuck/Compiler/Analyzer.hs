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

exprDepends :: Expr -> Int -> Bool
exprDepends (Get o)      i = i == o
exprDepends (Plus e1 e2) i = exprDepends e1 i || exprDepends e2 i
exprDepends (Mult e1 e2) i = exprDepends e1 i || exprDepends e2 i
exprDepends _ _            = False

copyLoop :: IL -> Maybe [(Int, Int)]
copyLoop (Loop o loop) = helper
  where
    helper | null a || not (null d) = Nothing
           | otherwise              = Just $ map getConst c
      where
        (a, b) = partition (isDec o) loop
        (c, d) = partition isConstAdd b

    getConst (Set d (Get _ `Plus` Const v)) = (d, v)
    getConst (Set d (Const v `Plus` Get _)) = (d, v)
    getConst _ = error "Not a const add"

    isConstAdd (Set d1 (Get d2 `Plus` Const _)) = d1 == d2
    isConstAdd (Set d1 (Const _ `Plus` Get d2)) = d1 == d2
    isConstAdd _                                = False

    isDec d1 (Set d2 (Get d3 `Plus` Const (-1))) = d1 == d2 && d1 == d3
    isDec d1 (Set d2 (Const (-1) `Plus` Get d3)) = d1 == d2 && d1 == d3
    isDec _ _                                    = False

copyLoop _ = Nothing
