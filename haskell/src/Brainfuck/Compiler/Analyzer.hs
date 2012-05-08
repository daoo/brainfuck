module Brainfuck.Compiler.Analyzer where

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

copyLoop :: IL -> Maybe (Int, [(Int, Int)])
copyLoop (Loop _ [])  = Nothing
copyLoop (Loop o ils) = if isCopyLoop
  then Just (o, map (\(Add d (Const i)) -> (d, i)) $ filter otherAdd ils)
  else Nothing
  where
    isCopyLoop = nLen == 1 && oLen >= 1 && len - oLen - 1 == 0
      where
        len  = length ils
        nLen = length $ filter negAdd ils
        oLen = length $ filter otherAdd ils

    negAdd (Add _ (Const (-1))) = True
    negAdd _                    = False

    otherAdd (Add _ (Const n)) = n /= (-1)
    otherAdd _                 = False

copyLoop _            = Nothing

exprDepends :: Expr -> Int -> Bool
exprDepends (Get o)      i = i == o
exprDepends (Plus e1 e2) i = exprDepends e1 i || exprDepends e2 i
exprDepends (Mult e1 e2) i = exprDepends e1 i || exprDepends e2 i
exprDepends _ _            = False
