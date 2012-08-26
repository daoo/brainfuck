module Main where

import Brainfuck.Compiler.Brainfuck
import Brainfuck.Compiler.Optimize
import Brainfuck.Compiler.Target.C99
import Brainfuck.Compiler.Target.Haskell
import Brainfuck.Compiler.Target.Indented
import Brainfuck.Data.IL
import Brainfuck.Data.State
import Brainfuck.Ext
import Brainfuck.Interpreter
import Brainfuck.Parser
import Data.Foldable (toList)
import Data.Word
import System.Console.GetOpt
import System.Environment

data Action = Compile | Interpret deriving (Show, Read)
data Target = Indented | C99 | Haskell deriving (Show, Read)

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
  , Option "t" ["target"] (ReqArg (\arg opt -> opt { optTarget = read arg })  "Indented | C99 | Haskell") "target language"
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
      Indented -> putStrLn $ showIndented optimized
      C99      -> putStrLn $ showC optimized
      Haskell  -> putStrLn $ showHaskell optimized
    Interpret  -> getContents >>= putStr . (`runBF` optimized)

  where
    runBF :: String -> [IL] -> String
    runBF inp = map chrIntegral . toList . getOutput . run (newState inp :: State Word8)
