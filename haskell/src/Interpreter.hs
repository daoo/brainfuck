module Main where

import Brainfuck.Compile
import Brainfuck.Interpret
import Brainfuck.Optimization.Pipeline
import Brainfuck.Parse
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString as B

data Action = Interpret | Help deriving (Show, Read)

data Options = Options
  { optAction :: Action
  , optOptimize :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { optAction   = Interpret
  , optOptimize = 1
  }

printHelp :: IO ()
printHelp = putStrLn (usageInfo header options)
  where
    header = "Usage: brainfuck [options] file..."

options :: [OptDescr (Options -> Options)]
options =
  [ Option "O" ["optimize"] (ReqArg (\arg opt -> opt { optOptimize = read arg }) "{0|1|2|3}") "optimizations"
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

  case optAction opts of
    Help      -> printHelp
    Interpret -> exec1 <$> B.getContents <*> (optimize <$> codeFrom file) >>= B.putStr

  where
    codeFrom = fmap (compile . parseBrainfuck) . B.readFile
