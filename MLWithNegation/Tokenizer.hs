{-|
Module      : Tokenizer
Description : Program that categorizes characters into tokens. 
Copyright   : (c) Alexander Stadelmann,
                  Benjamin Sühling,
                  Mana Jakobi,
                  Benedikt Etmüller, 2021

The function tokenize recursively reads one character at a time
until it reaches the base case(empty string) or an error. 
Each symbol/token consists of a series of characters that belong together. 
There are two accumulators, one that saves the characters of a symbol and 
one that saves all the assembled symbols along with the line number
in which they appear in the source code.
-}

module Tokenizer
(
  tokenize
)
  where


import Data.Char

import Declarations


tokenize:: String -- ^ Source code.
  -> [(Symbol,Int)]-- ^ List of tuples, each with a Symbol and its line number.
tokenize "" = error "Cannot compile empty files."
tokenize xs = tokenize' (filter (\a -> a `notElem` [' ', '\r']) xs) "" 1 []

-- | Helper function.
tokenize' :: String -- ^ Source code without spaces or carriage returns.
  -> String -- ^ symbol accumulator.
  -> Int -- ^ line counter
  -> [(Symbol,Int)] -- ^ list accumulator
  -> [(Symbol,Int)] -- ^ list of tuples, each with a symbol and its line number

-- | Base case with empty symbol accumulator. If absent, append a "NewLine" symbol.
tokenize' "" "" line ((NewLine, _) : acc) = reverse acc
tokenize' "" "" line acc = reverse $ (NewLine, line) : acc

-- | Base case with non-empty symbol accumulator. If absent, append a "Newline" symbol.
tokenize' "" symacc line acc
  | isUpper $ last symacc =
    reverse $ (NewLine, line) : (Variable $ reverse symacc, line) : acc
  | otherwise =
    reverse $ (NewLine, line) : (Name $ reverse symacc, line) : acc

-- | Next three characters form "not"
tokenize' ('n' : 'o' : 't' : xs) "" line acc =
  tokenize' xs "" line $ (Not, line) : acc

-- | Next two characters form ":-" and empty symbol accumulator.
tokenize' (':' : '-' : xs) "" line acc =
  tokenize' xs "" line $ (If, line):acc

-- | Next two characters form ":-" and non-empty symbol accumulator.
tokenize' xxs@(':' : '-' : xs) symacc line acc
  | isUpper $ last symacc =
    tokenize' xxs "" line $ (Variable $ reverse symacc, line):acc
  | otherwise =
    tokenize' xxs "" line $ (Name $ reverse symacc, line):acc

-- Next symbol is a breakpoint and empty symbol accumulator.
tokenize' (x:xs) "" line acc
  | x == '.' = tokenize' xs "" line $ (Point, line) : acc
  | x == ',' = tokenize' xs "" line $ (And, line) : acc
  | x == '(' = tokenize' xs "" line $ (LBracket, line) : acc
  | x == ')' = tokenize' xs "" line $ (RBracket, line) : acc

-- |  If symbol is a new line then increase the line counter by one.
  | x == '\n' =
    let acc' = case acc of
                    (NewLine,_) : t -> acc
                    _ -> (NewLine, line) : acc
    in tokenize' xs "" (line + 1) acc'

-- | Next symbol is not a breakpoint or symbol accumulator non-empty.
tokenize' (x:xs) symacc line acc
  | not $ isValid x =
    error $ "Invalid character in line " ++ show line ++ ": " ++ show x
  | x == '.' || x == ',' || x == '(' || x == ')' || x == '\n' = 
    if isUpper $ last symacc
       then tokenize' (x:xs) "" line $ (Variable $ reverse symacc, line) : acc
       else tokenize' (x:xs) "" line $ (Name $ reverse symacc, line):acc
  | otherwise = tokenize' xs (x : symacc) line acc

-- | Function that checks if a given character is admissible.
isValid :: Char -- ^ 
  -> Bool
isValid x = isAlphaNum x
          || x == '('
          || x == ')'
          || x == ','
          || x == '.'
          || x == ' '
          || x == '\n'
