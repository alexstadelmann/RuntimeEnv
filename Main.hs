module Main where


import Tokenizer
import Parser
import MiniL


main :: IO ()
main = do
  inputText <- getLine
  let tokens = tokenize inputText
      syntaxTree = parse tokens
      outputText = show $ miniL syntaxTree
  putStrLn outputText
