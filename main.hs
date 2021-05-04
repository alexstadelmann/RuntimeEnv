import Tokenizer
import Syntaxbaum
import Data.Char
import Data.List
type If = String
type Not = String
type Pt = String --ist noch unklar ob sinnvoll
type Name = String
type Variable = String



data Programm = P [Klausel] Ziel  deriving (Show)

data Klausel = K1 NVLT Pt |K2 NVLT Ziel deriving (Show)

data Term = TV Variable |T NVLT | Empty deriving (Show)

data NVLT = NVLT Name [Term]  deriving (Show)

data Literal = LNot Not Term |L Term deriving (Show)

data Ziel = Z If [Literal] Pt deriving (Show)



literalMaker :: String -> Literal
literalMaker ('n':'o':'t':xs) = LNot "not" (termMaker xs)
literalMaker xs = L (termMaker xs)


--was ist mit dem leeren String?
termMaker :: String -> Term
termMaker "" = Empty
termMaker (x:xs)
    | isUpper x = TV (x:xs)
    | otherwise = T (nvltMaker (x:xs))



nvltMaker :: String -> NVLT

nvltMaker xs = NVLT (fst(break (\a -> a == '(') xs)) (map termMaker $ tokenizerLTerme $ snd(break (\a -> a =='(') xs))



zielMaker :: String -> Ziel
zielMaker (x:y:zs)
                | not (x==':' && y=='-') = error "Programme enthalten immer ein Ziel als letzte Klausel und Ziele fangen mit ':-' an"
                | otherwise = Z ":-" (map literalMaker $ tokenizerZiele zs) "."



klauselMaker :: String -> Klausel
klauselMaker (x:xs)
                | not (isAlpha x) || isUpper x = error "Programmklauseln fangen mit einem Kleinbuchstaben an"
klauselMaker xs
                | ':' `elem` xs = K2 ((nvltMaker $ fst(break (\a -> a == ':') xs))) (zielMaker $ snd(break (\a -> a == ':') xs))
                | otherwise = K1 (nvltMaker xs) "."


--tokenizerKlauseln generiert eine Liste von Strings.
--Das letzte Element ist ein Ziel und wird mit last ausgewählt.
--Die Vorgänger-Klauseln werden mit init ausgewählt.
programmMaker :: String -> Programm
programmMaker xs = P (map klauselMaker $ init $ tokenizerKlauseln xs ) (zielMaker $ last $ tokenizerKlauseln xs)




isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x ==':' || x == '-' || x==' ' || x=='\n' || x=='\r'


isValidString :: String -> Bool
isValidString xs = foldr (\x acc -> if isValid x then acc else False) True xs

main = do
  contents <- readFile "testprogramm1.txt"
  putStrLn contents
  if isValidString contents then writeFile "ergebnis.txt" {-$ syntaxmaker-} $ show (programmMaker contents)
  else writeFile "ergebnis.txt" ""
