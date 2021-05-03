import System.IO
import System.Directory
import Data.List
import Data.Char

isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x ==':' || x == '-' || x==' ' || x=='\n' || x=='\r'


isValidString :: String -> Bool
isValidString xs = foldr (\x acc -> if isValid x then acc else False) True xs

tokeniser:: String -> [String]
tokeniser xs = tokeniser' (filter(\a -> a /= '\n' && a /= '\r') xs) [] []


tokeniser' :: String -> String -> [String] -> [String]
tokeniser' [] wacc lacc = reverse((reverse wacc):lacc)
tokeniser' (':':'-':z) wacc lacc = tokeniser' z "-:" ((reverse wacc):lacc)
tokeniser' (x:xs) wacc lacc
        | x == ' ' || x == '\n' || x == '\r' = tokeniser' xs "" ((reverse wacc):lacc)
--        | x == '(' = tokeniser (xs) "(" ((reverse wacc):lacc)
--        | x == ')' = tokeniser  xs "" ((reverse (x:wacc)):lacc)
        | otherwise = tokeniser' xs (x:wacc) lacc


main = do
  contents <- readFile "testprogramm1.txt"
  putStrLn contents
  if isValidString contents then writeFile "ergebnis.txt" $ show (tokeniser contents)
  else writeFile "ergebnis.txt" ""
