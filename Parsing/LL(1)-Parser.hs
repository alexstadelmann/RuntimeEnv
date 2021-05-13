data Symbol = Point
            | If
            | And
            | Not
            | LBracket
            | RBracket
            | Name String
            | Variable String

data Programm = Programm [PKlausel] Ziel

data PKlausel = PKlausel NVLTerm (Maybe Ziel)

data Ziel = Ziel [Literal]

data Literal = Literal IstNegiert LTerm

type IstNegiert = Bool

data NVLTerm = NVLTerm String [LTerm]

data LTerm = Var String
           | NVar NVLTerm



parse :: [Symbol] -> Programm
parse s = case programm s [] of
               ([], p) -> p
               _ -> error "Parsing went wrong."


programm :: [Symbol] -> [PKlausel] -> ([Symbol], Programm)
programm s@((Name _):t) pks =
    let erg = pklausel s
    in programm (fst erg) $ (snd erg):pks
programm s pks =
    let z = ziel s
    in (fst z, Programm (reverse pks) (snd z))


pklausel :: [Symbol] -> ([Symbol], PKlausel)
pklausel s =
    let nt = nvlterm s
    in uncurry pklausel' nt where

    pklausel' :: [Symbol] -> NVLTerm -> ([Symbol], PKlausel)
    pklausel' (Point:t) nt = (t, PKlausel nt Nothing)
    pklausel' s nt =
        let z = ziel s
        in (fst z, PKlausel nt (Just (snd z)))


ziel :: [Symbol] -> ([Symbol], Ziel)
ziel (If:t) = ziel' t [] where

    ziel' :: [Symbol] -> [Literal] -> ([Symbol], Ziel)
    ziel' s ls =
        let l = literal s
        in ziel'' (fst l) $ (snd l):ls

    ziel'' :: [Symbol] -> [Literal] -> ([Symbol], Ziel)
    ziel'' (And:t) ls = ziel' t ls
    ziel'' (Point:t) ls = (t, Ziel ls)
    ziel'' _ _ = error "Parsing went wrong."

ziel _ = error "Parsing went wrong."


literal :: [Symbol] -> ([Symbol], Literal)
literal (Not:t) =
    let lt = lterm t
    in (fst lt, Literal True (snd lt))
literal s =
    let lt = lterm s
    in (fst lt, Literal False (snd lt))


nvlterm :: [Symbol] -> ([Symbol], NVLTerm)
nvlterm ((Name n):t) = nvlterm' t n where

    nvlterm' :: [Symbol] -> String -> ([Symbol], NVLTerm)
    nvlterm' (LBracket:t) n = nvlterm'' t n []
    nvlterm' s n = (s, NVLTerm n [])

    nvlterm'' :: [Symbol] -> String -> [LTerm] -> ([Symbol], NVLTerm)
    nvlterm'' s n lts =
        let l = lterm s
        in nvlterm''' (fst l) n $ (snd l):lts

    nvlterm''' :: [Symbol] -> String -> [LTerm] -> ([Symbol], NVLTerm)
    nvlterm''' (And:t) n lts = nvlterm'' t n lts
    nvlterm''' (RBracket:t) n lts = (t, NVLTerm n lts)
    nvlterm''' _ _ _ = error "Parsing went wrong"

nvlterm _ = error "Parsing went wrong"


lterm :: [Symbol] -> ([Symbol], LTerm)
lterm ((Variable v):t) = (t, Var v)
lterm s =
    let nt = nvlterm s
    in (fst nt, NVar (snd nt))
