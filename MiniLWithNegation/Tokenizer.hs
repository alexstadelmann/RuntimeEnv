module Tokenizer
(
  tokenize
)
  where


import Data.Char

import Declarations


tokenize:: String -> [Symbol]
tokenize "" = []
tokenize xs =  tokenize' (filter (\a -> a `notElem` [' ', '\n', '\r']) xs) "" []


tokenize' :: String -> String -> [Symbol] -> [Symbol]

-- kein nächstes Zeichen:
tokenize' "" "" listacc = reverse listacc

tokenize' "" symbolacc listacc
  | isUpper $ last symbolacc = reverse $ (Variable $ reverse symbolacc):listacc
  | otherwise = reverse $ (Name $ reverse symbolacc):listacc

-- nächste drei Zeichen "not":
tokenize' ('n':'o':'t':xs) "" listacc = tokenize' xs "" $ Not:listacc

-- nächste zwei Zeichen ":-"
tokenize' (':':'-':xs) "" listacc = tokenize' xs "" $ If:listacc
tokenize' xxs@(':':'-':xs) symbolacc listacc
  | isUpper $ last symbolacc = tokenize' xxs "" $ (Variable $ reverse symbolacc):listacc
  | otherwise = tokenize' xxs "" $ (Name $ reverse symbolacc):listacc

-- nächstes Zeichen Point, And, LBracket oder RBracket
tokenize' (x:xs) "" listacc
  | x == '.' = tokenize' xs "" $ Point:listacc
  | x == ',' = tokenize' xs "" $ And:listacc
  | x == '(' = tokenize' xs "" $ LBracket:listacc
  | x == ')' = tokenize' xs "" $ RBracket:listacc

tokenize' (x:xs) symbolacc listacc
  | not $ isValid x = error $ "unerlaubes Zeichen: " ++ show x
  | x == '.' || x == ',' || x == '(' || x == ')' =
    if isUpper $ last symbolacc
       then tokenize' (x:xs) "" $ (Variable $ reverse symbolacc):listacc
       else tokenize' (x:xs) "" $ (Name $ reverse symbolacc):listacc
  | otherwise = tokenize' xs (x:symbolacc) listacc


isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x==' ' || x=='\n' || x=='\r'
