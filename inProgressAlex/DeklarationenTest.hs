module DeklarationenTest (
PCode,
Command(..),
Mix(..),
Register(..),
Symbol(..),
Programm(..),
PKlausel(..),
Ziel(..),
Literal(..),
IstNegiert,
NVLTerm(..),
LTerm(..),
Substitution,
) where

type PCode = [Command]

data Command = Push String | Unify String | Call | Return | Backtrack | Promt deriving (Show)

data Mix = Pklausel' PKlausel | Literal' Literal | Int Int | Bool Bool | LTerm' LTerm | String String | Empty deriving (Show)

data Register = Register{i:: Int, b:: Bool, t:: Int, c :: Int, r:: Int, p:: Int} deriving (Show)

type Substitution = [(String, String)]

--im Folgenden Deklarationen insbesondere für den Tokenizer und für den Parser
data Symbol = Variable String | Name String | LBracket | RBracket | Not | If | Point | And deriving (Show)

data Programm = Programm [PKlausel] Ziel deriving (Show)

data PKlausel = PKlausel NVLTerm (Maybe Ziel) deriving (Show)

data Ziel = Ziel [Literal] deriving (Show)

data Literal = Literal IstNegiert LTerm deriving (Show)

type IstNegiert = Bool

data NVLTerm = NVLTerm String [LTerm] deriving (Show, Eq)

data LTerm = Var String | NVar NVLTerm deriving (Show, Eq)
