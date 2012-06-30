module Main where

import Data.Foldable (toList)
import Data.List
import Data.Word

import System.Environment
import System.Directory

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Compiler.Target.C99
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
  argv         <- getArgs
  (what, code) <- params argv
  inp          <- getContents

  il <- case parseBrainfuck code of
    Left err -> error $ show err
    Right bf -> return $ compile bf

  putStrLn $ case what of
    ToC       -> makeC il
    Interpret -> runBF inp il

  where
    makeC :: [IL] -> String
    makeC = showC . optimizeAll

    runBF :: String -> [IL] -> String
    runBF inp = map chrIntegral . toList . getOutput . run state
      where
        state :: State Word8
        state = newState inp

