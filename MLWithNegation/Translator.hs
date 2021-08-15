module Translator
(
  translate,
  createEnv
)
  where


import qualified Data.Set as Set

import Declarations


translate :: SyntaxTree -> Code
translate (SyntaxTree cs (varseq, g)) =
  concatMap translate' cs ++ transEnv varseq
  ++ transBody g ++ [Backtrack, Prompt] where

  translate' :: (VarSeq, PClause) -> Code
  translate' (varSeq, PClause nvlt g) =
    transEnv varSeq ++ transHead nvlt ++ transBody g ++ [Return, Backtrack]

transHead :: NVLTerm -> Code
transHead nvlt = concatMap transHead' $ linLTerm $ NVar nvlt

transHead' :: Arg -> Code
transHead' str = [Unify str, Backtrack]


transBody :: Goal -> Code
transBody = concatMap transBody' where

  transBody' :: Literal -> Code
  transBody' (Literal False lt) =
    Push (CHP 0) : map (\x -> Push x) (linLTerm lt) ++ [Call]
  transBody' (Literal True lt) =
    Push (CHP 1) : map (\x -> Push x) (linLTerm lt) ++ [Call]

transEnv :: VarSeq -> Code
transEnv v = foldl (\xs x -> Push (VAR' x True) : xs)
                   [Push $ EndEnv' $ Set.size v]
                   $ Set.toDescList v


linLTerm :: LTerm -> [Arg]
linLTerm (Var s) = [VAR' s False]
linLTerm (NVar (NVLTerm a xs)) =
  STR' a (length xs) : concatMap linLTerm xs


createEnv :: Code -> Env
createEnv = createEnv' [0] 0 where

  createEnv' :: [Int] -> Int -> Code -> Env
  createEnv' cs i (Return : Backtrack : _ : t) =
    createEnv' ((i + 2) : cs) (i + 3) t
  createEnv' (h : t) i (Prompt : _) =
    Env {clauses = reverse t, cGoal = h, cLast = i}
  createEnv' cs i (_ : t) =
    createEnv' cs (i + 1) t
