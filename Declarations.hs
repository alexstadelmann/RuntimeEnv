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
  Storage(..)
)
  where


type Stack = [StackElem]

data StackElem = NUM Int
               | RET Int Int -- ReturnAddress Old New
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

data Register = Register {b :: Bool, c :: Int, r :: Int, p :: Int}
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
              deriving (Show)

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
