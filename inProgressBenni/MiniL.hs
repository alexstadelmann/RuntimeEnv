type PCode = [Command]

data Command = Push String
             | Unify String
             | Call
             | Return
             | Backtrack
             | Prompt
               deriving (Show)

data Env = Env {klauseln :: [Int], ziel :: Int, letzte :: Int}
    deriving (Show)

data StackElem = Zahl Int
               | Atom NVLTerm
                 deriving (Show)

type Stack = [StackElem]

data Register = Register {i :: Int, b :: Bool, t :: Int, c :: Int, r :: Int, p :: Int}
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


translate :: Programm -> PCode
translate (Programm pks z) =
    concat (map translate' pks) ++ translateBody (Just z) ++ [Prompt] where

    translate' :: PKlausel -> PCode
    translate' (PKlausel nvlt z) =
        translateHead nvlt ++ translateBody z ++ [Return]


translateHead :: NVLTerm -> PCode
translateHead (NVLTerm s _) = [Unify s, Backtrack]


translateBody :: Maybe Ziel -> PCode
translateBody Nothing = []
translateBody (Just (Ziel ls)) = concat $ map translateBody' ls where

    translateBody' :: Literal -> PCode
    translateBody' (Literal _ lt) =
        case lt of NVar nvlt -> translateBody'' nvlt

    translateBody'' :: NVLTerm -> PCode
    translateBody'' (NVLTerm s _) = [Push s, Call, Backtrack]


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

    createEnv' :: [Int] -> Int -> Int -> PCode -> Env
    createEnv' ks _ c ((Unify _):t) = createEnv' (c:ks) 0 (c+1) t
    createEnv' ks _ c (Return:(Push s):t) = createEnv' ks (c+1) (c+2) t
    createEnv' ks z c (Prompt:_) = Env (reverse ks) z c
    createEnv' ks z c (_:t) = createEnv' ks z (c+1) t


c_first :: Env -> Int
c_first env = case klauseln env of
                   [] -> -1
                   ks -> head ks


c_next :: Env -> Int -> Int
c_next env = c_next' $ klauseln env where

    c_next' :: [Int] -> Int -> Int
    c_next' (h:t) i
        | h /= i = c_next' t i
        | otherwise = case t of
                           (x:_) -> x
                           _ -> -1


c_goal :: Env -> Int
c_goal = ziel


c_last :: Env -> Int
c_last = letzte


push :: NVLTerm -> Stack -> Env ->  Register -> (Stack, Register)
push at st env reg =
    ((Atom at):(Zahl $ (p reg) + 3):(Zahl $ c reg):(Zahl $ c_first env):st,
        reg {c = (t reg)+1, r = (t reg)+2, t = (t reg)+4, p = (p reg) + 1})


unify :: NVLTerm -> Stack -> Register -> Register
unify at st reg = reg {b = (getAtomAt st $ (c reg) + 3) /= at, p = (p reg) + 1}


getAt :: Stack -> Int -> StackElem
getAt [] _ = error "Index out of bounds"
getAt (h:t) i
    | i == 0 = h
    | otherwise = getAt t $ i-1


getNumAt :: Stack -> Int -> Int
getNumAt st i = case getAt st i of
                     Zahl z -> z
                     _ -> error "No number  saved at this stack position"


getAtomAt :: Stack -> Int -> NVLTerm
getAtomAt st i = case getAt st i of
                     Atom a -> a
                     _ -> error "No number  saved at this stack position"





testProgramm :: Programm
testProgramm = Programm [k1, k2, k3] z3

k1 :: PKlausel
k1 = PKlausel pt (Just z1)

k2 :: PKlausel
k2 = PKlausel qt (Just z2)

k3 :: PKlausel
k3 = PKlausel rt Nothing

z1 :: Ziel
z1 = Ziel [Literal False (NVar qt)]

z2 :: Ziel
z2 = Ziel [Literal False (NVar rt)]

z3 :: Ziel
z3 = Ziel [Literal False (NVar pt), Literal False (NVar rt)]

pt :: NVLTerm
pt = NVLTerm "p" []

qt :: NVLTerm
qt = NVLTerm "q" []

rt :: NVLTerm
rt = NVLTerm "r" []
