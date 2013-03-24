module Main where

import Brainfuck.CodeGen.C99 as C99
import Brainfuck.CodeGen.Dot as Dot
import Brainfuck.CodeGen.Haskell as Haskell
import Brainfuck.CodeGen.Indented as Indented
import Brainfuck.Compile
import Brainfuck.Interpret
import Brainfuck.Optimization.Pipeline
import Brainfuck.Parse
import Brainfuck.Utility
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Action = Compile | Interpret | Help deriving (Show, Read)
data Target = Indented | C99 | Haskell | Dot deriving (Show, Read)

data Options = Options
  { optAction :: Action
  , optTarget :: Target
  , optOptimize :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction   = Interpret
  , optTarget   = C99
  , optOptimize = 1
  }

data Flag = Do Action | Target Target

printHelp :: IO ()
printHelp = putStrLn (usageInfo header options)
  where
    header = "Usage: brainfuck [options] file..."

options :: [OptDescr (Options -> Options)]
options =
  [ Option "c" ["compile"] (NoArg (\opt -> opt { optAction = Compile })) "compile input"
  , Option "t" ["target"] (ReqArg (\arg opt -> opt { optTarget = read arg }) "{Indented|C99|Haskell|Dot}") "target language"
  , Option "O" ["optimize"] (ReqArg (\arg opt -> opt { optOptimize = read arg }) "{0|1}") "optimizations"
  , Option "h" ["help"] (NoArg (\opt -> opt { optAction = Help})) "show help"
  ]

main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- case getOpt RequireOrder options args of
    (flags , nonOpts , [])   -> return (flags, concat nonOpts)
    (_     , _       , errs) -> hPutStr stderr (concat errs) >> printHelp >> exitWith (ExitFailure 1)

  let opts = pipe flags defaultOptions

      optimize =
        case optOptimize opts of
          0 -> id
          1 -> simpleOptimizations
          _ -> fullOptimization

      codegen =
        case optTarget opts of
          Indented -> Indented.showAST
          C99      -> C99.showAST
          Haskell  -> Haskell.showAST
          Dot      -> Dot.showAST

  case optAction opts of
    Help      -> printHelp
    Compile   -> astFrom file >>= putStrLn . codegen . optimize
    Interpret -> do
      ast <- astFrom file
      inp <- getContents
      putStr $ run1 inp $ optimize ast

  where
    astFrom file = do
      code <- readFile file
      case parseBrainfuck code of
        Left err -> error $ show err
        Right bf -> return $ compile bf
