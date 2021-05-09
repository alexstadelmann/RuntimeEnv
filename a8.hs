import System.IO
import Tokenizer
{-
data Symbol = Point
            | If
            | And
            | Not
            | LBracket
            | RBracket
            | Name String
            | Variable String deriving (Show)

-}

main = do
  content <-readFile "input.txt"
  writeFile "output.xml" ("<Programm>"++(parse content)++"</Programm>")

parse :: String -> String
parse xs = snd $ programm (nextSymbol xs, "")


programm :: ([Symbol], String)-> ([Symbol], String)
programm (symbols@((Name _):t), xs) = programm $ (fst $ programmklausel (symbols, ""), xs ++ "<Programmklausel>" ++ (snd $ programmklausel (symbols,""))++"</Programmklausel>")
programm (symbols, xs) = (fst $ ziel (symbols, ""), xs ++ "<Ziel>" ++ (snd $ ziel (symbols,"")) ++ "</Ziel>")


programmklausel :: ([Symbol], String) -> ([Symbol], String)
programmklausel (symbols, xs) = (fst $ programmklausel' $ nVarLTerm (symbols, ""), xs ++ "<NichtVariableLTerm>" ++ (snd $ programmklausel' $ nVarLTerm (symbols, "")) ++"</NichtVariableLTerm>")

programmklausel' :: ([Symbol], String) -> ([Symbol], String)
programmklausel' ((Point:t), xs) = (t,  xs ++ "<Symbol>Point</Symbol>")
programmklausel' (symbols@(If:t), xs) = (fst $ ziel (symbols, ""), xs ++ (snd $ ziel (symbols, "")))



ziel :: ([Symbol], String) -> ([Symbol], String)
ziel ((If:t), xs) = (fst $ ziel' (t, ""), xs ++ "<Symbol>If</Symbol>" ++( snd $ ziel' (t, "")))
ziel (_,_) = error "Fehler1"

ziel' :: ([Symbol], String) -> ([Symbol], String)
ziel' (symbols, xs) = ziel'' (fst $ literal (symbols, xs), (snd $ literal (symbols, xs)))


ziel'' :: ([Symbol], String) -> ([Symbol], String)
ziel'' ((And:t), xs) = (fst$ ziel' (t, ""),xs ++ "<Symbol>And</Symbol>" ++ (snd $ ziel' (t, "")))
ziel'' ((Point:t),xs) = (t, xs ++ "<Symbol>Point</Symbol>")
ziel'' (_,_) = error "Fehler2"




literal :: ([Symbol], String) -> ([Symbol], String)
literal ((Not:t), xs) = (fst $ lTerm (t, ""), xs ++ "<Symbol>Not</Symbol>" ++ (snd $ lTerm(t, "")))
literal (symbols, xs) = (fst$ lTerm (symbols, xs), snd $ lTerm (symbols, xs))


nVarLTerm :: ([Symbol], String) -> ([Symbol], String)
nVarLTerm (((Name a):t), xs) = (fst $ nVarLTerm' (t, ""), xs ++ "<Name>" ++ a ++ "</Name>" ++ (snd $ nVarLTerm'(t, "")))
nVarLTerm _ = error "Fehler3"

nVarLTerm' :: ([Symbol], String) -> ([Symbol], String)
nVarLTerm' ((LBracket:t), _) = (fst $ nVarLTerm'' (t, ""), "<Symbol>LBracket</Symbol>" ++ (snd $ nVarLTerm''(t, "")))
nVarLTerm' (symbols, _) = (symbols, "")

nVarLTerm'' :: ([Symbol], String) -> ([Symbol], String)
nVarLTerm'' (symbols, xs) = (fst $ nVarLTerm''' $ lTerm (symbols, ""),xs++ (snd $ nVarLTerm''' $ lTerm(symbols, "")))

nVarLTerm''' :: ([Symbol], String) -> ([Symbol], String)
nVarLTerm''' ((And:t), xs) = (fst $ nVarLTerm'' (t, ""), xs ++ "<Symbol>And</Symbol>" ++ (snd $ nVarLTerm'' (t, "")))
nVarLTerm''' ((RBracket:t), xs) = (t, xs ++ "<Symbol>RBracket</Symbol>")
nVarLTerm''' _ = error "Fehler4"


lTerm :: ([Symbol], String) -> ([Symbol], String)
lTerm (((Variable a):t), xs) = (t, xs ++ "<LTerm><Variable>"++ a++"</Variable></LTerm>")
lTerm (symbols, xs) = (fst $ nVarLTerm (symbols, ""), xs++ "<LTerm>" ++ (snd $ nVarLTerm (symbols, "")) ++"</LTerm>")
