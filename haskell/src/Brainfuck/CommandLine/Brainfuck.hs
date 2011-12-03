module Main where

import Data.List

import System
import System.Directory

import Brainfuck.CommandLine.Run
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.C
import Brainfuck.Parser.Parser

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
    getBF :: String -> IO (String)
    getBF str = do
      isfile <- doesFileExist str
      case isfile of
        True  -> readFile str
        False -> return str

    internal :: String -> [IL]
    internal = compile . parse

    makeC :: String -> String
    makeC = showC . internal
