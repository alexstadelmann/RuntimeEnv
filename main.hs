import Tokenizer
import Data.Char
import Data.List
type If = String
type Not = String
type Pt = String --ist noch unklar ob sinnvoll
type Name = String
type Variable = String


-- | NV3 Name (LTerm, LTerm) | NV4 Name (LTerm, LTerm, LTerm) deriving (Show)
-- | P2 Programmklausel Ziel | P3 Programmklausel Programmklausel Ziel| P4 Programmklausel Programmklausel Programmklausel Ziel deriving (Show)
--  |Z3 If Literal K Literal Pt K Literal Pt deriving (Show)
data Programm = P1 Ziel | P2 Programmklausel Ziel  deriving (Show)
data Programmklausel = PK1 NVLT Pt |PK2 NVLT Ziel deriving (Show)

data LTerm = LT1 Variable |LT2 NVLT deriving (Show)

data NVLT = NVLT1 Name | NVLT2 Name (LTerm) | NVLT3 Name (LTerm, LTerm) deriving (Show)

data Literal =L1 Not LTerm |L2 LTerm deriving (Show)

data Ziel =Z1 If Literal Pt |Z2 If (Literal, Literal) Pt deriving (Show)


literalMaker :: String -> Literal
literalMaker ('n':'o':'t':xs) = L1 "not" (lTermMaker xs)
literalMaker xs = L2 (lTermMaker xs)


lTermMaker :: String -> LTerm
lTermMaker (x:xs)
    | isUpper x = LT1 (x:xs)
    | x == '(' = LT2 (nvltMaker $ filter (\a -> a /= '(' &&  a /= ')') (x:xs))
    | otherwise = LT2 (nvltMaker (x:xs))



nvltMaker :: String -> NVLT
nvltMaker xs
          | '(' `notElem` xs = NVLT1 xs
          | length lTermliste == 1 = NVLT2 (fst(break (\a -> a == '(') xs)) (lTermMaker $ head lTermliste)
          | length lTermliste == 2 = NVLT3 (fst(break (\a -> a == '(') xs)) ((lTermMaker $ head lTermliste), (lTermMaker $ lTermliste !! 1))
          | otherwise = error "noch nicht implementiert"
            where lTermliste = tokenizerLTerme $ snd(break (\a -> a =='(') xs)



zielMaker :: String -> Ziel
zielMaker xs
          | length zieleListe == 1 = Z1 ":-" (literalMaker $ head zieleListe) "."
          | length zieleListe == 2 = Z2 ":-" ((literalMaker $ head zieleListe), (literalMaker $ zieleListe !! 1)) "."
            where zieleListe = tokenizerZiele xs


programmklauselMaker :: String -> Programmklausel
programmklauselMaker xs
                | ':' `elem` xs = PK2 (NVLT1 (fst(break (\a -> a == ':') xs))) (zielMaker $ snd(break (\a -> a == ':') xs))
                | otherwise = PK1 (nvltMaker xs) "."

programmMaker :: String -> Programm
programmMaker (x:xs)
            | x == ':' = P1 (zielMaker (x:xs))
            | length klauselliste == 1 = P2 (programmklauselMaker $ head klauselliste) (zielMaker $ snd(break (\a -> a == ':') (x:xs)))
            | otherwise = P1 (zielMaker (x:xs))

              where klauselliste = tokenizerKlauseln (fst(break (\a -> a == ':') (x:xs)))

isValid:: Char -> Bool
isValid x = isAlphaNum x || x == '(' || x == ')'|| x == ',' ||x== '.' || x ==':' || x == '-' || x==' ' || x=='\n' || x=='\r'


isValidString :: String -> Bool
isValidString xs = foldr (\x acc -> if isValid x then acc else False) True xs

main = do
  contents <- readFile "testprogramm1.txt"
  putStrLn contents
  if isValidString contents then writeFile "ergebnis.txt" $ show (programmMaker contents)
  else writeFile "ergebnis.txt" ""
