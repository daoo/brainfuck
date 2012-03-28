module Main where

import Data.List

import System.Environment
import System.Directory

import Brainfuck.Compiler.C
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizer
import Brainfuck.Parser.Parser
import Brainfuck.Run

main :: IO ()
main = do
  argv <- getArgs

  -- Decide what we're going to do
  case partition (== "-c") argv of
    (["-c"], str) -> do
      bf <- getBF $ concat str
      putStrLn $ makeC bf
    (_, str)      -> do
      inp <- getContents
      bf <- getBF $ concat str
      putStr $ brainfuck bf inp

  where
    getBF :: String -> IO String
    getBF str = do
      isfile <- doesFileExist str
      if isfile
        then readFile str
        else return str

    internal :: String -> [IL]
    internal = optimizeFully . compile . parse

    makeC :: String -> String
    makeC = showC . internal
