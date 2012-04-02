module Main where

import Data.Foldable (toList)
import Data.List
import Data.Word

import System.Environment
import System.Directory

import Brainfuck.Compiler.C.Optimize
import Brainfuck.Compiler.C.Show
import Brainfuck.Compiler.IL
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

data Action = ToC | Interpret

params :: [String] -> IO (Action, String)
params p = code (concat p') >>= (\bf -> return (action, bf))
  where
    (pc, p') = partition (== "-c") p
    action = case pc of
      []     -> Interpret
      ["-c"] -> ToC
      _      -> error "Unrecognized parameter"

    code str = do
      isfile <- doesFileExist str
      if isfile
        then readFile str
        else return str

main :: IO ()
main = do
  argv <- getArgs
  p    <- params argv
  inp  <- getContents

  putStr $ case p of
    (ToC, code)       -> makeC $ ilify code
    (Interpret, code) -> runBF inp $ ilify code

  where
    ilify :: String -> [IL]
    ilify = compile . parse

    makeC :: [IL] -> String
    makeC = showC . optimizeForC

    runBF :: String -> [IL] -> String
    runBF inp = map chrIntegral . toList . getOutput . run state
      where
        state :: State Word8
        state = newState inp
      
