module Tokenizer
(
  tokenize
)
  where


import Data.Char

import Declarations


tokenize:: String -> [(Symbol,Int)]  --
tokenize "" = []
tokenize xs =  tokenize' (filter (\a -> a `notElem` [' ', '\r']) xs) "" 1 []

-- tokenize's parameters are: the input string, a word accumulator, a line counter and a result accumulator. 
tokenize' :: String -> String -> Int -> [(Symbol,Int)] -> [(Symbol,Int)]

-- no next character:
tokenize' "" "" _ listacc = reverse listacc --

tokenize' "" symbolacc lineacc listacc --
  | isUpper $ last symbolacc = reverse $ (Variable $ reverse symbolacc, lineacc):listacc
  | otherwise = reverse $ (Name $ reverse symbolacc, lineacc):listacc

-- next three characters form "not":
tokenize' ('n':'o':'t':xs) "" lineacc listacc = tokenize' xs "" lineacc $ (Not, lineacc):listacc

-- next two characters form ":-":
tokenize' (':':'-':xs) "" lineacc listacc = tokenize' xs "" lineacc $ (If, lineacc):listacc
tokenize' xxs@(':':'-':xs) symbolacc lineacc listacc 
  | isUpper $ last symbolacc = tokenize' xxs "" lineacc $ (Variable $ reverse symbolacc, lineacc):listacc
  | otherwise = tokenize' xxs "" lineacc $ (Name $ reverse symbolacc, lineacc):listacc

-- next character is a Point, And, LBracket or RBracket:
tokenize' (x:xs) "" lineacc listacc
  | x == '.' = tokenize' xs "" lineacc $ (Point, lineacc):listacc
  | x == ',' = tokenize' xs "" lineacc $ (And, lineacc):listacc
  | x == '(' = tokenize' xs "" lineacc $ (LBracket, lineacc):listacc
  | x == ')' = tokenize' xs "" lineacc $ (RBracket, lineacc):listacc
  | x == '\n' =
     let listacc' = case listacc of             -- each '\n'-character increments the line counter by one. A NewLine-Token is added.
                         (NewLine,_):t -> listacc
                         _ -> (NewLine, lineacc) : listacc
     in tokenize' xs "" (lineacc + 1) listacc'

tokenize' (x:xs) symbolacc lineacc listacc
  | not $ isValid x = error $ "Invalid character: " ++ show x ++ " on line " ++ show(lineacc)
  | x == '.' || x == ',' || x == '(' || x == ')' || x == '\n' = 
    if isUpper $ last symbolacc
       then tokenize' (x:xs) "" lineacc $ (Variable $ reverse symbolacc, lineacc):listacc
       else tokenize' (x:xs) "" lineacc $ (Name $ reverse symbolacc, lineacc):listacc
  | otherwise = tokenize' xs (x:symbolacc) lineacc listacc


-- test if character is accepted.
isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x==' ' || x=='\n' || x=='\r'