module Declarations
(
  Stack,
  StackElem(..),
  PCode,
  Env(..),
  Command(..),
  Register(..),
  Symbol(..),
  SyntaxTree(..),
  PClause(..),
  Goal(..),
  Literal(..),
  IsNegated,
  NVLTerm(..),
  LTerm(..),
  Storage(..),
  VarSeq
)
  where

import qualified Data.Set as Set

type Stack = [StackElem]

data StackElem = NUM Int
               | STR String
                 deriving (Show, Eq)

type PCode = [Command]

data Env = Env {clauses :: [Int], cGoal :: Int, cLast :: Int}
  deriving (Show)

data Command = Push StackElem
             | Unify StackElem
             | Call
             | Return
             | Backtrack
             | Prompt
               deriving (Show, Eq)

-- l: level of current CHP in the proof tree
data Register = Register {b :: Bool,
                          c :: Int,
                          r :: Int,
                          p :: Int,
                          l :: Int}
    deriving (Show)


-- Deklarationen (insbesondere) für den Tokenizer und für den Parser
data Symbol = Variable String
            | Name String
            | LBracket
            | RBracket
            | Not
            | If
            | Point
            | And
            | NewLine

              
instance Show Symbol where
  show (Variable s) = s      
  show (Name s) = s 
  show LBracket = "("
  show RBracket = ")"
  show Not = "not"
  show If = ":-"
  show Point = "."
  show And = ","
  show NewLine = "new line"


data SyntaxTree = SyntaxTree [PClause] Goal
  deriving (Show)

data PClause = PClause NVLTerm (Maybe Goal)
  deriving (Show)

data Goal = Goal [Literal]
  deriving (Show)

data Literal = Literal IsNegated LTerm
  deriving (Show)

type IsNegated = Bool

data NVLTerm = NVLTerm String [LTerm]
  deriving (Show, Eq)

data LTerm = Var String | NVar NVLTerm
  deriving (Show, Eq)

type Storage = (Stack, PCode, Env, Register)

type VarSeq = Set.Set String 
