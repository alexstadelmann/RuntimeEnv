module Declarations' (
    Stack,
    LTermElem(..),
    StackElem(..),
    PCode,
    Env(..),
    Command(..),
    Register(..),
    Symbol(..),
    Programm(..),
    PKlausel(..),
    Ziel(..),
    Literal(..),
    IstNegiert,
    NVLTerm(..),
    LTerm(..),
) where
type Stack = [StackElem]

data StackElem = Zahl Int | Atom String deriving (Show, Eq)
data LTermElem = STR String Int | VAR String Int deriving (Show, Eq)

type PCode = [Command]

data Env = Env{klauseln :: [Int], goal :: Int, letzte :: Int}
    deriving (Show)

data Command = Push LTermElem
             | PushCHP
             | PushEND
             | Unify LTermElem
             | Call
             | Return
             | BacktrackQ
             | Prompt
               deriving (Show, Eq)


data Register = Register{inst:: Int, backQ:: Bool, top:: Int, choice :: Int, ret:: Int, pcounter:: Int} 
    deriving (Show)

data Register' = Register' Int Bool Int Int Int Int
    deriving (Show)


--Deklarationen (insbesondere) für den Tokenizer und für den Parser
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

data LTerm = Var String | NVar NVLTerm 
        deriving (Show, Eq)
