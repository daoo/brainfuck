module Main where

import Data.Foldable (toList)

import System.Console.GetOpt
import System.Environment

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.IL
import Brainfuck.Compiler.Optimize
import Brainfuck.Compiler.Target.C99
import Brainfuck.Compiler.Target.Indented
import Brainfuck.Ext
import Brainfuck.Interpreter.Interpreter
import Brainfuck.Interpreter.State
import Brainfuck.Parser.Parser

data Action = Compile | Interpret deriving (Show, Read)
data Target = Indented | C99 deriving (Show, Read)

data Options = Options
  { optAction :: Action
  , optTarget :: Target
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction = Interpret
  , optTarget = C99
  }

data Flag = Do Action | Target Target

options :: [OptDescr (Options -> Options)]
options =
  [ Option "c" ["compile"] (NoArg (\opt -> opt { optAction = Compile })) "compile input"
  , Option "t" ["target"] (ReqArg (\arg opt -> opt { optTarget = read arg })  "C99 | Indented") "target language"
  ]

main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- case getOpt RequireOrder options args of
    (flags , nonOpts , [])   -> return (flags, concat nonOpts)
    (_     , _       , msgs) -> error $ concat msgs

  let opts = pipe flags defaultOptions

  code <- readFile file

  ils <- case parseBrainfuck code of
    Left err -> error $ show err
    Right bf -> return $ compile bf

  let optimized = optimizeAll ils

  case optAction opts of
    Compile    -> case optTarget opts of
      C99      -> putStrLn $ showC optimized
      Indented -> putStrLn $ showIndented optimized
    Interpret  -> getContents >>= putStrLn . (`runBF` optimized)

  where
    runBF :: String -> [IL] -> String
    runBF inp = map chrIntegral . toList . getOutput . run (newState inp)
