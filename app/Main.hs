module Main where

import Data.Text (pack)
import Parser (parseProg)
import Solver
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case parse parseProg filename (pack contents) of
        Left err -> do
          putStrLn "Parsing failed"
          putStrLn $ errorBundlePretty err
        Right src -> 
          solve src >>= putStrLn

    ["--parse", filename] -> do
      contents <- readFile filename
      case parse parseProg filename (pack contents) of
        Left err -> do
          putStrLn "Parsing failed"
          putStrLn $ errorBundlePretty err
        Right src -> do
          putStrLn "Parsing succeeded"
          print src
    _ ->
      putStrLn
        "Usage: OatCereal <filename>\n\
        \OatCereal --parse <filename>\n"
