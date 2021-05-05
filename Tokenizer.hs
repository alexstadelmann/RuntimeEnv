module Tokenizer
( tokenizerZiele
, tokenizerLTerme
, tokenizerKlauseln
) where

import Data.List

tokenizerZiele:: String -> [String]
tokenizerZiele xs =  tokenizerZiele' (filter(\a -> a /= '\n' && a /= '\r') (delete ':'(delete '-' xs))) [] []


--Hilfsfunktion:
tokenizerZiele' :: String -> String -> [String] -> [String]
tokenizerZiele' [] wacc lacc = reverse((reverse wacc):lacc)
tokenizerZiele' (x:xs) wacc lacc
    | x == ' ' && wacc == "" = tokenizerZiele' xs "" lacc
    | x == ' ' = error "Lehrzeichen wo es nicht sein darf!"
    | x == ',' && (head wacc) == ')' = tokenizerZiele' xs "" ((reverse (wacc)):lacc)
    | otherwise = tokenizerZiele' xs (x:wacc) lacc


tokenizerLTerme:: String -> [String]
tokenizerLTerme xs
                | xs == "" = []
                | head xs /= '(' || last xs /= ')' = error "Terme kommen immer in Klammern vor!"
                | otherwise = tokenizerLTerme' (filter(\a -> a /= '\n' && a /= '\r') $ init $ tail xs) [] []


--Hilfsfunktion:
tokenizerLTerme' :: String -> String -> [String] -> [String]
tokenizerLTerme' [] wacc lacc
    | wacc == [] && lacc == [] = []
    | otherwise = reverse((reverse wacc):lacc)
tokenizerLTerme' (x:xs) wacc lacc
    | x == ' ' && wacc == "" = tokenizerLTerme' xs "" lacc
    | x == ' ' = error "Lehrzeichen wo es nicht sein darf!"
    | x == ',' && '(' `notElem` wacc = tokenizerLTerme' xs "" ((reverse (wacc)):lacc)
    | otherwise = tokenizerLTerme' xs (x:wacc) lacc



tokenizerKlauseln:: String -> [String]
tokenizerKlauseln "" = error "Klausel leer!"
tokenizerKlauseln xs
    | (last $ (filter(\a -> a /= '\n' && a /= '\r') xs)) /= '.' = error "Jede Klausel endet mit einem Punkt!"
    | otherwise = tokenizerKlauseln' (filter(\a -> a /= '\n' && a /= '\r') xs) [] []



--Hilfsfunktion:
tokenizerKlauseln' :: String -> String -> [String] -> [String]
tokenizerKlauseln' [] wacc lacc = if wacc /= "" then reverse((reverse wacc):lacc) else reverse lacc
tokenizerKlauseln' (x:xs) wacc lacc
    | x == '.' = tokenizerKlauseln' xs "" ((reverse (wacc)):lacc)
    | otherwise = tokenizerKlauseln' xs (x:wacc) lacc
