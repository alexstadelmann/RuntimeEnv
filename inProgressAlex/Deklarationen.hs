module Deklarationen (
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




data Symbol = Variable String | Name String | LBracket | RBracket | Not | If | Point | And deriving (Show)

data Programm = Programm [PKlausel] Ziel deriving (Show)

data PKlausel = PKlausel NVLTerm (Maybe Ziel) deriving (Show)

data Ziel = Ziel [Literal] deriving (Show)

data Literal = Literal IstNegiert LTerm deriving (Show)

type IstNegiert = Bool

data NVLTerm = NVLTerm String [LTerm] deriving (Show)

data LTerm = Var String | NVar NVLTerm deriving (Show)

type Substitution = [(String, String)]
