module Main where


import Tokenizer
import Parser
import MiniTranslator
import MiniL


main :: IO ()
main = do
  putStrLn "Please, specify a file to be compiled."
  inputFilePath <- getLine
  inputProgram <- readFile inputFilePath
  let tokens = tokenize inputProgram
      syntaxTree = parse tokens
      programCode = translate syntaxTree
      outputText = show $ miniL programCode
  putStrLn outputText
