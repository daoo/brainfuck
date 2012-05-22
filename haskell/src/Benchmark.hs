module Main where

import Criterion.Main
import Data.List

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Ext
import Brainfuck.Parser.Parser

compFst :: Eq a => (a, b) -> (a, b) -> Bool
compFst a b = fst a == fst b

dedupBy :: (a -> a -> Bool) -> [a] -> [a]
dedupBy f (a : b : xs) | f a b     = dedupBy f (b : xs)
                       | otherwise = a : dedupBy f (b : xs)
dedupBy _ xs                       = xs

type Pipeline = [(String, [IL] -> [IL])]

showPipeline :: Pipeline -> String
showPipeline = intercalate " -> " . map fst

pipelines :: [Pipeline]
pipelines = map (dedupBy compFst) $ permutations
  [ optExpr
  , ("Inline Zeroes", inlineZeros)
  , ("Reduce Copy Loops", reduceCopyLoops)
  , ("Clean Up", filterIL clean)
  , ("Apply IL", applyIL)
  ]
  where
    optExpr = ("Optimize Expressions", mapIL optimizeExpressions)

testPipeLine :: Pipeline -> [IL] -> [IL]
testPipeLine pipeline ils = whileModified (pipe $ map snd pipeline) ils

main :: IO ()
main = do
  str <- getContents
  let Right bf = parseBrainfuck str
  let il = compile bf
  defaultMain $ map (f il) $ [head pipelines]
  where
    f il pipeline = bench (showPipeline pipeline) $ whnf (testPipeLine pipeline) il
