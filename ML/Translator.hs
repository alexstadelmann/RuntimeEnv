module Translator
(
  translate,
  createEnv,
  cNext,
  cFirst
)
  where


import qualified Data.Set as Set

import Declarations


translate :: SyntaxTree -> PCode
translate (SyntaxTree cs (varseq, g)) =
  concatMap translate' cs ++ Push BegEnv : transEnv varseq
  ++ transBody (Just g) ++ [Backtrack, Prompt] where

  translate' :: (VarSeq, PClause) -> PCode
  translate' (varSeq, PClause nvlt g) =
    Push BegEnv : transEnv varSeq
    ++ transHead nvlt ++ transBody g ++ [Return]


transHead :: NVLTerm -> PCode
transHead nvlt = concatMap transHead' $ linLTerm $ NVar nvlt

transHead' :: Arg -> PCode
transHead' str = [Unify str, Backtrack]


transBody :: Maybe Goal -> PCode
transBody Nothing = []
transBody (Just (Goal ls)) = concatMap transBody' ls where

  transBody' :: Literal -> PCode
  transBody' (Literal _ lt) =
    Push CHP : map (\x -> Push x) (linLTerm lt) ++ [Call]


transEnv :: VarSeq -> PCode
transEnv v =
  foldl (\xs x -> Push (VAR' x) : xs) [Push $ EndEnv $ Set.size v] v


linLTerm :: LTerm -> [Arg]
linLTerm (Var s) = [VAR' s]
linLTerm (NVar (NVLTerm a xs)) =
  STR' a (length xs) : concatMap linLTerm xs


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 where

  createEnv' :: [Int] -> Int -> PCode -> Env
  createEnv' cs i (Push BegEnv : t) =
    createEnv' (i : cs) (i + 1) t
  createEnv' (h : t) i (Prompt : _) =
    Env {clauses = reverse t, cGoal = h, cLast = i}
  createEnv' cs i (_ : t) =
    createEnv' cs (i + 1) t


cNext :: Env -> Int -> Int
cNext env = cNext' (clauses env) where

  cNext' :: [Int] -> Int -> Int
  cNext' [] _ = -1
  cNext' (h : t) i
    | h <= i = cNext' t i
    | otherwise = h


cFirst :: Env -> Int
cFirst Env {clauses = []} = -1
cFirst _ = 0
