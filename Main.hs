module Main where


import Tokenizer
import Parser
import MiniL


main :: IO ()
main = do
  putStrLn "Please, specify a file to be compiled."
  inputFilePath <- getLine
  inputProgram <- readFile inputFilePath
  let tokens = tokenize inputProgram
      syntaxTree = parse tokens
      outputText = show $ miniL syntaxTree
  putStrLn outputText
