type PCode = [Command]

data Command = Push String
             | Unify String
             | Call
             | Return
             | Backtrack
             | Prompt
               deriving (Show)

{-
data Mix = Pklausel' PKlausel
         | Literal' Literal
         | Int Int
         | Bool Bool
         | LTerm' LTerm
         | String String
         | Empty deriving (Show)
-}

data Register = Register{inst:: Int, back:: Bool, top:: Int, choice :: Int, ret:: Int, pcounter:: Int}
    deriving (Show)

type Substitution = [(String, String)]

-- im Folgenden Deklarationen insbesondere für den Tokenizer und für den Parser
data Symbol = Variable String
            | Name String
            | LBracket
            | RBracket
            | Not
            | If
            | Point
            | And
    deriving (Show)

data Programm = Programm [PKlausel] Ziel
    deriving (Show)

data PKlausel = PKlausel NVLTerm (Maybe Ziel)
    deriving (Show)

data Ziel = Ziel [Literal]
    deriving (Show)

data Literal = Literal IstNegiert LTerm
    deriving (Show)

type IstNegiert = Bool

data NVLTerm = NVLTerm String [LTerm]
    deriving (Show, Eq)

data LTerm = Var String
           | NVar NVLTerm
             deriving (Show, Eq)


evaluate :: Programm -> Either String Substitution
evaluate _ = error "TODO"


translate :: Programm -> [Command]
translate (Programm pks z) =
    (concat (map translate' pks)) ++ (translateBody (Just z)) ++ [Prompt] where

    translate' :: PKlausel -> [Command]
    translate' (PKlausel nvlt z) =
        (translateHead nvlt) ++ (translateBody z) ++ [Return]


translateHead :: NVLTerm -> [Command]
translateHead (NVLTerm s _) = [Unify s, Backtrack]


translateBody :: Maybe Ziel -> [Command]
translateBody Nothing = []
translateBody (Just (Ziel ls)) = concat (map translateBody' ls) where

    translateBody' :: Literal -> [Command]
    translateBody' (Literal _ lt) =
        case lt of NVar nvlt -> translateBody'' nvlt

    translateBody'' :: NVLTerm -> [Command]
    translateBody'' (NVLTerm s _) = [Push s, Call, Backtrack]




testProgramm :: Programm
testProgramm = Programm [k1, k2, k3] z3

k1 :: PKlausel
k1 = PKlausel p (Just z1)

k2 :: PKlausel
k2 = PKlausel q (Just z2)

k3 :: PKlausel
k3 = PKlausel r Nothing

z1 :: Ziel
z1 = Ziel [Literal False (NVar q)]

z2 :: Ziel
z2 = Ziel [Literal False (NVar r)]

z3 :: Ziel
z3 = Ziel [Literal False (NVar p), Literal False (NVar r)]

p :: NVLTerm
p = NVLTerm "p" []

q :: NVLTerm
q = NVLTerm "q" []

r :: NVLTerm
r = NVLTerm "r" []
