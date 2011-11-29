module Main where

import System
import System.Directory

import Interpreter

main :: IO ()
main = do
  argv <- getArgs
  let args = concat argv
  isfile <- doesFileExist args
  str <- case isfile of
    True  -> readFile args
    False -> return args
    
  inp <- getContents
  putStr $ interpret str inp
