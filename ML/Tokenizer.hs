module Tokenizer
(
  tokenize
)
  where


import Data.Char

import Declarations


tokenize:: String -> [(Symbol,Int)]
tokenize "" = error "Cannot compile empty files."
tokenize xs = tokenize' (filter (\a -> a `notElem` [' ', '\r']) xs) "" 1 []


tokenize' :: String -> String -> Int -> [(Symbol,Int)] -> [(Symbol,Int)]

-- kein nächstes Zeichen:
tokenize' "" "" line ((NewLine, _) : acc) = reverse acc

tokenize' "" "" line acc = reverse $ (NewLine, line) : acc

tokenize' "" symacc line acc
  | isUpper $ last symacc =
    reverse $ (NewLine, line) : (Variable $ reverse symacc, line) : acc
  | otherwise =
    reverse $ (NewLine, line) : (Name $ reverse symacc, line) : acc

-- nächste drei Zeichen "not":
tokenize' ('n' : 'o' : 't' : xs) "" line acc =
  tokenize' xs "" line $ (Not, line) : acc

-- nächste zwei Zeichen ":-"
tokenize' (':' : '-' : xs) "" line acc =
  tokenize' xs "" line $ (If, line):acc

tokenize' xxs@(':' : '-' : xs) symacc line acc
  | isUpper $ last symacc =
    tokenize' xxs "" line $ (Variable $ reverse symacc, line):acc
  | otherwise =
    tokenize' xxs "" line $ (Name $ reverse symacc, line):acc

-- nächstes Zeichen Point, And, LBracket, RBracket oder NewLine
tokenize' (x:xs) "" line acc
  | x == '.' = tokenize' xs "" line $ (Point, line) : acc
  | x == ',' = tokenize' xs "" line $ (And, line) : acc
  | x == '(' = tokenize' xs "" line $ (LBracket, line) : acc
  | x == ')' = tokenize' xs "" line $ (RBracket, line) : acc
  | x == '\n' =
    let acc' = case acc of
                    (NewLine,_) : t -> acc
                    _ -> (NewLine, line) : acc
    in tokenize' xs "" (line + 1) acc'  -- Wert von acc' verwendet

tokenize' (x:xs) symacc line acc
  | not $ isValid x =
    error $ "Invalid input in line " ++ show line ++ ": " ++ show x
  | x == '.' || x == ',' || x == '(' || x == ')' || x == '\n' = 
    if isUpper $ last symacc
       then tokenize' (x:xs) "" line $ (Variable $ reverse symacc, line) : acc
       else tokenize' (x:xs) "" line $ (Name $ reverse symacc, line):acc
  | otherwise = tokenize' xs (x : symacc) line acc


isValid :: Char -> Bool
isValid x = isAlphaNum x
          || x == '('
          || x == ')'
          || x == ','
          || x == '.'
          || x == ' '
          || x == '\n'
