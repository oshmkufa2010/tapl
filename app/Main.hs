module Main where

import Eval
import Types
import Parser
import Control.Monad.State
import System.Environment

main :: IO ()
main = do
  [filePath] <- getArgs
  content <- readFile filePath   
  let result = parse content >>= \term -> evalStateT (typeOf term >> eval term) []
  case result of
    Left err -> putStrLn err
    Right val -> putStrLn $ showTerm val
