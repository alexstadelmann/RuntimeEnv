{- |
Module      : Translator
Description : Takes an L5 syntax tree as input and creates an internal ML program out of it.

The translator takes an L5 syntax tree as input and creates an internal ML program out of it.
The internal ML program is a sequence of commands that tell the compiler what to do in the next step.
-}
module Translator
(
  translate,
  createEnv
)
  where


import qualified Data.Set as Set

import Declarations


-- | Translates a complete syntax tree to ML code.
translate :: SyntaxTree -- ^ The syntax tree.
  -> Code -- ^ The ML code.
translate (SyntaxTree cs (varseq, g)) =
  concatMap translate' cs ++ transEnv varseq
  ++ transBody g ++ [Backtrack, Prompt] where

  translate' :: (VarSeq, PClause) -> Code
  translate' (varSeq, PClause nvlt g) =
    transEnv varSeq ++ transHead nvlt ++ transBody g ++ [Return]


-- | Translates the head of a program clause to ML code.
transHead :: NVLTerm -- ^ The head of the program clause.
  -> Code -- ^ The ML code.
transHead nvlt = concatMap transHead' $ linLTerm $ NVar nvlt where

  transHead' :: Arg -> Code
  transHead' str = [Unify str, Backtrack]


-- | Translates the body of a program clause to ML code.
transBody :: Goal -- ^ The body of the program clause.
  -> Code -- ^ The ML code.
transBody = concatMap transBody' where

  transBody' :: Literal -> Code
  transBody' (Literal _ lt) =
    Push CHP : map (\x -> Push x) (linLTerm lt) ++ [Call]


-- | Translates a variable sequence to ML code.
transEnv :: VarSeq -- ^ The variable sequence.
  -> Code -- ^ The ML code.
transEnv v = foldl (\xs x -> Push (VAR' x True) : xs)
                   [Push $ EndEnv' $ Set.size v]
                   $ Set.toDescList v


-- | Linearizes a term, e.g. p(a, X) to p 1, a 0, X.
linLTerm :: LTerm -- ^ The term.
  -> [Arg] -- ^ The linearization.
linLTerm (Var s) = [VAR' s False]
linLTerm (NVar (NVLTerm a xs)) =
  STR' a (length xs) : concatMap linLTerm xs


-- | Creates a code environment out of ML code.
createEnv :: Code -- ^ The ML code.
  -> Env -- ^ The code environment.
createEnv = createEnv' [0] 0 where

  createEnv' :: [Int] -> Int -> Code -> Env
  createEnv' cs i (Return : _ : t) =
    createEnv' ((i + 1) : cs) (i + 2) t
  createEnv' (h : t) i (Prompt : _) =
    Env {clauses = reverse t, cGoal = h, cLast = i}
  createEnv' cs i (_ : t) =
    createEnv' cs (i + 1) t
