data Symbol = Point
            | If
            | And
            | Not
            | LBracket
            | RBracket
            | Name String
            | Variable String


parse :: [Symbol] -> Bool
parse symbs = case programm symbs of
  [] -> True
  _ -> error "Parsing went wrong."


programm :: [Symbol] -> [Symbol]
programm symbs@((Name _):t) = programm $ programmklausel symbs
programm symbs = ziel symbs


programmklausel :: [Symbol] -> [Symbol]
programmklausel symbs = programmklausel' $ nVarLTerm symbs where

  programmklausel' :: [Symbol] -> [Symbol]
  programmklausel' (Point:t) = t
  programmKlausel' = ziel


ziel :: [Symbol] -> [Symbol]
ziel (If:t) = ziel' t where

  ziel' :: [Symbol] -> [Symbol]
  ziel' symbs = ziel'' $ literal symbs

  ziel'' :: [Symbol] -> [Symbol]
  ziel'' (And:t) = ziel' t
  ziel'' (Point:t) = t
  ziel'' _ = error "Parsing went wrong."

ziel _ = error "Parsing went wrong."


literal :: [Symbol] -> [Symbol]
literal (Not:t) = lTerm t
literal symbs = lTerm symbs


nVarLTerm :: [Symbol] -> [Symbol]
nVarLTerm ((Name _):t) = nVarLTerm' t where

  nVarLTerm' :: [Symbol] -> [Symbol]
  nVarLTerm' (LBracket:t) = nVarLTerm'' t
  nVarLTerm' symbs = symbs

  nVarLTerm'' :: [Symbol] -> [Symbol]
  nVarLTerm'' symbs = nVarLTerm''' $ lTerm symbs

  nVarLTerm''' :: [Symbol] -> [Symbol]
  nVarLTerm''' (And:t) = nVarLTerm'' t
  nVarLTerm''' (RBracket:t) = t
  nVarLTerm''' _ = error "Parsing went wrong"

nVarLTerm _ = error "Parsing went wrong"


lTerm :: [Symbol] -> [Symbol]
lTerm ((Variable _):t) = t
lTerm symbs = nVarLTerm symbs
