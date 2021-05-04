module Syntaxbaum
( syntaxmaker
) where


syntaxmaker :: String -> String
syntaxmaker xs =  syntaxmaker' xs 3



syntaxmaker' :: String -> Int -> String
syntaxmaker' "" _ = ""
syntaxmaker' (x:xs) n
            | x == '(' = '(':'\n':((replicate n ' ') ++ (syntaxmaker' xs (n+3)))
            | x == '[' = '[':'\n':((replicate n ' ') ++ (syntaxmaker' xs (n+3)))
            | x == ')' = ')':'\n':(replicate n ' ') ++ (syntaxmaker' xs (n -3))
            | x == ']' = '\n':(replicate n ' ') ++ "]" ++ (syntaxmaker' xs (n -3))
            | x == ',' = ',':'\n':(replicate (n-3) ' ')++ (syntaxmaker' xs n)
            | otherwise = x: syntaxmaker' xs n
