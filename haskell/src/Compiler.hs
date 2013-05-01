module Main where

import Brainfuck.CodeGen.C99
import Brainfuck.CodeGen.Dot
import Brainfuck.CodeGen.Haskell
import Brainfuck.CodeGen.Indented
import Brainfuck.Compile
import Brainfuck.Optimization.Pipeline
import Brainfuck.Parse
import Data.ByteString.Builder
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.CodeWriter
import qualified Data.ByteString.Char8 as BS

data Action = Compile | Help deriving (Show, Read)
data Target = Indented | C99 | Haskell | Dot deriving (Show, Read)

data Options = Options
  { optAction :: Action
  , optTarget :: Target
  , optOptimize :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction   = Compile
  , optTarget   = C99
  , optOptimize = 1
  }

printHelp :: IO ()
printHelp = putStrLn (usageInfo header options)
  where
    header = "Usage: brainfuck [options] file..."

options :: [OptDescr (Options -> Options)]
options =
  [ Option "c" ["compile"] (NoArg (\opt -> opt { optAction = Compile })) "compile input"
  , Option "t" ["target"] (ReqArg (\arg opt -> opt { optTarget = read arg }) "{Indented|C99|Haskell|Dot}") "target language"
  , Option "O" ["optimize"] (ReqArg (\arg opt -> opt { optOptimize = read arg }) "{0|1|2|3}") "optimizations"
  , Option "h" ["help"] (NoArg (\opt -> opt { optAction = Help})) "show help"
  ]

main :: IO ()
main = do
  args <- getArgs
  (flags, file) <- case getOpt RequireOrder options args of
    (flags , nonOpts , [])   -> return (flags, concat nonOpts)
    (_     , _       , errs) -> hPutStr stderr (concat errs) >> printHelp >> exitWith (ExitFailure 1)

  let opts = foldr ($) defaultOptions flags

      optimize =
        case optOptimize opts of
          0 -> id
          1 -> simpleOptimizations
          2 -> fullOptimization
          3 -> loopUnrolling
          _ -> undefined

      codegen =
        case optTarget opts of
          Indented -> writeIndented
          C99      -> writeC99
          Haskell  -> writeHaskell
          Dot      -> writeDot

  case optAction opts of
    Help    -> printHelp
    Compile -> codeFrom file >>= hPutBuilder stdout . writeCode . codegen . optimize

  where
    codeFrom = fmap (compile . parseBrainfuck) . BS.readFile
