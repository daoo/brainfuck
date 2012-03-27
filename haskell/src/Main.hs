module Main where

import Data.Foldable (toList)
import Data.List
import Data.Word

import System.Environment
import System.Directory

import Brainfuck.Compiler.C
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimizer
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

brainfuck :: String -> String -> String
brainfuck str inp = map chrIntegral $ brainfuck1 str inp

brainfuck1 :: String -> String -> [Word8]
brainfuck1 str inp = toList out
  where
    il            = compile $ parse str
    State _ out _ = run (newState inp) il

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
    internal = optimize . compile . parse

    makeC :: String -> String
    makeC = showC . internal
