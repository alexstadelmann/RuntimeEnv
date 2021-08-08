module Tokenizer
(
  tokenize
)
  where


import Data.Char

import Declarations


tokenize:: String -> [(Symbol,Int)]  --
tokenize "" = []
tokenize xs =  tokenize' (filter (\a -> a `notElem` [' ', '\r']) xs) "" 1 []   --


tokenize' :: String -> String -> Int -> [(Symbol,Int)] -> [(Symbol,Int)]  --

-- kein nächstes Zeichen:
tokenize' "" "" _ listacc = reverse listacc --

tokenize' "" symbolacc lineacc listacc --
  | isUpper $ last symbolacc = reverse $ (Variable $ reverse symbolacc, lineacc):listacc
  | otherwise = reverse $ (Name $ reverse symbolacc, lineacc):listacc

-- nächste drei Zeichen "not":
tokenize' ('n':'o':'t':xs) "" lineacc listacc = tokenize' xs "" lineacc $ (Not, lineacc):listacc

-- nächste zwei Zeichen ":-"
tokenize' (':':'-':xs) "" lineacc listacc = tokenize' xs "" lineacc $ (If, lineacc):listacc
tokenize' xxs@(':':'-':xs) symbolacc lineacc listacc 
  | isUpper $ last symbolacc = tokenize' xxs "" lineacc $ (Variable $ reverse symbolacc, lineacc):listacc
  | otherwise = tokenize' xxs "" lineacc $ (Name $ reverse symbolacc, lineacc):listacc

-- nächstes Zeichen Point, And, LBracket oder RBracket
tokenize' (x:xs) "" lineacc listacc
  | x == '.' = tokenize' xs "" lineacc $ (Point, lineacc):listacc
  | x == ',' = tokenize' xs "" lineacc $ (And, lineacc):listacc
  | x == '(' = tokenize' xs "" lineacc $ (LBracket, lineacc):listacc
  | x == ')' = tokenize' xs "" lineacc $ (RBracket, lineacc):listacc
  | x == '\n' =
     let listacc' = case listacc of             -- Wert von listacc' zugewiesen (neue Zeile hinzufügen, falls noch keine vorhanden)
                         (NewLine,_):t -> listacc
                         _ -> (NewLine, lineacc) : listacc
     in tokenize' xs "" (lineacc + 1) listacc'  -- Wert von listacc' verwendet

tokenize' (x:xs) symbolacc lineacc listacc
  | not $ isValid x = error $ "unerlaubtes Zeichen: " ++ show x
  | x == '.' || x == ',' || x == '(' || x == ')' || x == '\n' = 
    if isUpper $ last symbolacc
       then tokenize' (x:xs) "" lineacc $ (Variable $ reverse symbolacc, lineacc):listacc
       else tokenize' (x:xs) "" lineacc $ (Name $ reverse symbolacc, lineacc):listacc
  | otherwise = tokenize' xs (x:symbolacc) lineacc listacc



isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x==' ' || x=='\n' || x=='\r'
