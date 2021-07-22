module Declarations
(
  Stack,
  StackElem(..),
  LTermElem(..),
  PCode,
  Env(..),
  Command(..),
  Register(..),
  Symbol(..),
  SyntaxTree(..),
  PKlausel(..),
  Ziel(..),
  Literal(..),
  IstNegiert,
  NVLTerm(..),
  LTerm(..),
  Storage(..)
)
  where


type Stack = [StackElem]

data StackElem = Zahl Int
               | Atom String
                 deriving (Show, Eq)

data LTermElem = STR String Int
               | VAR String Int
                 deriving (Show)

type PCode = [Command]

data Env = Env {klauseln :: [Int], goal :: Int, letzte :: Int}
  deriving (Show)

data Command = Push StackElem
             | Unify StackElem
             | Call
             | Return
             | Backtrack
             | Prompt
               deriving (Show, Eq)

data Register = Register {b :: Bool, t :: Int, c :: Int, r :: Int, p :: Int}
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

data SyntaxTree = SyntaxTree [PKlausel] Ziel
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

type Storage = (Stack, PCode, Env, Register)
