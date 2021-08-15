module Declarations
(
  Stack,
  StackElem(..),
  Code,
  Env(..),
  Arg(..),
  Command(..),
  Register(..),
  Trail,
  US,
  Symbol(..),
  SyntaxTree(..),
  PClause(..),
  Goal,
  Literal(..),
  IsNegated,
  NVLTerm(..),
  LTerm(..),
  Storage(..),
  VarSeq
)
  where


import qualified Data.Set as Set


type Storage = (Stack, Code, Env, Register, Trail, US)


type Stack = [StackElem]


data StackElem = NUM Int
               | STR String Int
               | VAR String Int
               | EndEnv
  deriving (Show, Eq)


type Code = [Command]


data Command = Push Arg
             | Unify Arg
             | Call
             | Return
             | Backtrack
             | Prompt
  deriving (Show, Eq)


data Arg = STR' String Int
         -- Bool indicates whether variable is part of local env:
         | VAR' String Bool
         | CHP Int
         | EndEnv' Int
  deriving (Show, Eq)


data Env = Env {clauses :: [Int],
                cGoal :: Int,
                cLast :: Int}
  deriving (Show)


data Register = Reg {b :: Bool, -- backtrack flag
                     c :: Int, -- last CHP
                     r :: Int, -- CHP to return to
                     p :: Int, -- program counter
                     l :: Int, -- level of current CHP
                     e :: Int, -- local environment
                     up :: Int, -- unification pointer
                     pc :: Int, -- push counter
                     sc :: Int, -- skip counter
                     ac :: Int} -- argument counter
  deriving (Show)


type Trail = [Int]


type US = [Int]


type VarSeq = Set.Set String


-- declarations for Tokenizer and Parser

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


data SyntaxTree = SyntaxTree [(VarSeq, PClause)] (VarSeq, Goal)
  deriving (Show)


data PClause = PClause NVLTerm Goal
  deriving (Show)


type Goal = [Literal]


data Literal = Literal IsNegated LTerm
  deriving (Show)


type IsNegated = Bool


data NVLTerm = NVLTerm String [LTerm]
  deriving (Show, Eq)


data LTerm = Var String
           | NVar NVLTerm
  deriving (Show, Eq)
