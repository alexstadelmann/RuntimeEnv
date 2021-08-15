{- |
Module : Declarations

This module contains all data type declarations used by ML.
-}
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


-- | All "memory" that is needed for ML.
type Storage = (Stack, Code, Env, Register, Trail, US)


-- | Choice Points and local environments are stored here.
type Stack = [StackElem]


-- | All possible elements of a stack cell
data StackElem
  -- | Stack and code addresses, other ints (arg: the actual variable).
  = NUM Int
  -- | A structure cell (args: name, arity).
  | STR String Int
  -- | Variables (args: name, binding).
  | VAR String Int
  -- | Indicates the end of a local environment.
  | EndEnv
  deriving (Show, Eq)


-- | The internal ML program.
type Code = [Command]


-- | All commands that ML needs.
data Command
  -- | Something is pushed to the stack.
  = Push Arg
  -- | Unification of Arg and a term on the stack is tried.
  | Unify Arg
  -- | Try to unify the current CHP with a clause.
  | Call
  -- | Proof of a clause is finished.
  | Return
  -- | Undo earlier unifications, if necessary.
  | Backtrack
  -- | Proof of goal (un-)successfully terminates.
  | Prompt
  deriving (Show, Eq)


-- | All possible arguments for push or unify commands.
data Arg
  -- | A structure cell (args: name, arity).
  = STR' String Int
  -- | A variable (args: name, is part of a local env).
  | VAR' String Bool
  -- | Only for push: a CHP is pushed.
  | CHP
  -- | Only for push: end of local env is pushed (arg: size of env).
  | EndEnv' Int
  deriving (Show, Eq)



-- | The code environment stores information about the internal ML program
data Env = Env {clauses :: [Int], -- ^ The first code address of every clause.
                cGoal :: Int, -- ^ The first code address of the goal.
                cLast :: Int} -- ^ The code address of the command prompt.
  deriving (Show)


-- | Single values that ML needs.
data Register = Reg {b :: Bool, -- ^ Backtrack flag.
                     c :: Int, -- ^ Last CHP.
                     r :: Int, -- ^ CHP to return to.
                     p :: Int, -- ^ Program counter.
                     l :: Int, -- ^ Level of current CHP.
                     e :: Int, -- ^ Local environment.
                     up :: Int, -- ^ Unification pointer.
                     pc :: Int, -- ^ Push counter.
                     sc :: Int, -- ^ Skip counter.
                     ac :: Int} -- ^ Argument counter.
  deriving (Show)


-- | Variable bindings are stored here for backtracking purposes.
type Trail = [Int]


-- | The unification stack saves jumps of the unification pointer.
type US = [Int]


-- | All variables that occur in one clause.
type VarSeq = Set.Set String


-- | All possible tokens that the tokenizer may create.
data Symbol
  -- | A variable.
  = Variable String
  -- | A predicate symbol.
  | Name String
  -- | An opening bracket.
  | LBracket
  -- | A closing bracket.
  | RBracket
  -- | A negation.
  | Not
  -- | A reversed implication.
  | If
  -- | The end of a program clause or the goal.
  | Point
  -- | The logical and.
  | And
  -- | A new line in the source code.
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



-- | A syntax tree created by the parser.
data SyntaxTree =
  SyntaxTree [(VarSeq, PClause)] (VarSeq, Goal)
  deriving (Show)


-- | A program clause, part of a syntax tree.
data PClause = PClause NVLTerm Goal
  deriving (Show)


-- | A goal, part of a syntax tree.
type Goal = [Literal]


-- | A literal, part of a syntax tree.
data Literal = Literal IsNegated LTerm
  deriving (Show)


-- | Indicates whether a literal is negated.
type IsNegated = Bool


-- | An atom, part of a syntax tree.
data NVLTerm = NVLTerm String [LTerm]
  deriving (Show, Eq)


-- | A term, part of a syntax tree.
data LTerm
  = Var String -- ^ A Variable.
  | NVar NVLTerm -- ^ An atom.
  deriving (Show, Eq)
