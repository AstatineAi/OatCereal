module Main where

import Parser
import System.Environment (getArgs)
import Text.Megaparsec (parse, errorBundlePretty, runParser)
import Data.Text (pack)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      putStrLn $ "Parsing file: " ++ filename
      putStrLn $ "Contents:\n" ++ contents
      case parse parseProg filename (pack contents) of
        Left err -> do
          putStrLn "Parsing failed"
          putStrLn $ errorBundlePretty err
        Right ast -> do
          putStrLn "Parsing succeeded"
          print ast
    _ -> putStrLn "Usage: OatCereal <filename>"
