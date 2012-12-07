module Main where

import Brainfuck.CodeGen.C99
import Brainfuck.CodeGen.Haskell
import Brainfuck.CodeGen.Indented
import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Parser
import Brainfuck.Data.AST
import Brainfuck.Interpreter
import Brainfuck.Optimization.Pipeline
import Data.Foldable (toList)
import Data.Word
import Ext
import System.Console.GetOpt
import System.Environment

data Action = Compile | Interpret deriving (Show, Read)
data Target = Indented | C99 | Haskell deriving (Show, Read)

data Options = Options
  { optAction :: Action
  , optTarget :: Target
  , optOptimize :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction = Interpret
  , optTarget = C99
  , optOptimize = 1
  }

data Flag = Do Action | Target Target

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['c'] ["compile"] (NoArg (\opt -> opt { optAction = Compile })) "compile input"
  , Option ['t'] ["target"] (ReqArg (\arg opt -> opt { optTarget = read arg }) "Indented | C99 | Haskell") "target language"
  , Option ['O'] ["optimize"] (ReqArg (\arg opt -> opt { optOptimize = read arg }) "Optimization Level 0 | 1") "optimizations"
  ]

main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- case getOpt RequireOrder options args of
    (flags , nonOpts , [])   -> return (flags, concat nonOpts)
    (_     , _       , msgs) -> error $ concat msgs

  let opts = pipe flags defaultOptions

  code <- readFile file

  ast <- case parseBrainfuck code of
    Left err -> error $ show err
    Right bf -> return $ compile bf

  let optimize =
        case optOptimize opts of
          0 -> id
          _ -> optimizeAll

  let codegen =
        case optTarget opts of
          Indented -> showIndented
          C99      -> showC
          Haskell  -> showHaskell

  case optAction opts of
    Compile   -> putStrLn $ codegen $ optimize ast
    Interpret -> getContents >>= putStr . (`runBF` (optimize ast))

  where
    runBF :: String -> AST -> String
    runBF inp = map chrIntegral . toList . output . run (newState inp :: State Word8)
