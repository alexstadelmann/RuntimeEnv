module Tokenizer
(nextSymbol,
) where


import Data.Char
import Deklarationen

nextSymbol:: String -> [Symbol]
nextSymbol "" = []
nextSymbol xs =  nextSymbol' (filter(\a -> a `notElem` [' ', '\n', '\r']) xs) "" []


nextSymbol' :: String -> String -> [Symbol] -> [Symbol]
--kein nächstes Zeichen:
nextSymbol' "" "" listenacc = reverse (listenacc)
nextSymbol' "" symbolacc listenacc
    | isUpper $ last symbolacc = reverse ((Variable (reverse symbolacc)):listenacc)
    | otherwise = reverse ((Name (reverse symbolacc)):listenacc)

--nächste drei Zeichen "not":
nextSymbol' ('n':'o':'t':xs) "" listenacc= nextSymbol' xs "" (Not:listenacc)

--nächste zwei Zeichen ":-"
nextSymbol' (':':'-':xs) "" listenacc= nextSymbol' xs "" (If: listenacc)
nextSymbol' xxs@(':':'-':xs) symbolacc listenacc
    | isUpper $ last symbolacc = nextSymbol' xxs "" ((Variable (reverse symbolacc)):listenacc)
    | otherwise = nextSymbol' xxs "" ((Name (reverse symbolacc)):listenacc)

--nächstes Zeichen Point, Komma, LBracket oder RBracket
nextSymbol' (x:xs) "" listenacc
    | x == '.' = nextSymbol' xs "" (Point:listenacc)
    | x == ',' = nextSymbol' xs "" (And:listenacc)
    | x == '(' = nextSymbol' xs "" (LBracket:listenacc)
    | x == ')' = nextSymbol' xs "" (RBracket:listenacc)
nextSymbol' (x:xs) symbolacc listenacc
    | not $ isValid x = error ("unerlaubes Zeichen: " ++ show x)
    | x == '.' || x == ',' || x == '(' || x == ')' = if (isUpper $ last symbolacc) then nextSymbol' (x:xs) "" ((Variable (reverse symbolacc)):listenacc) else nextSymbol' (x:xs) "" ((Name (reverse symbolacc)):listenacc)
    | otherwise = nextSymbol' xs (x:symbolacc) listenacc




isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x==' ' || x=='\n' || x=='\r'
