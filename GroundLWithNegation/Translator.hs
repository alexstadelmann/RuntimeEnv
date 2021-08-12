{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Translator
(
  translate,
  createEnv,
  cNext,
  cFirst
)
  where


import Declarations


translate :: SyntaxTree -> PCode
translate (SyntaxTree cs g) =
  concatMap translate' cs ++ transBody (Just g)
  ++ [Backtrack, Prompt] where

  translate' :: PClause -> PCode
  translate' (PClause nvlt g) =
    transHead nvlt ++ transBody g ++ [Return, Backtrack]


transHead :: NVLTerm -> PCode
transHead nvlt = concatMap transHead' $ linLTerm $ NVar nvlt

transHead' :: StackElem -> PCode
transHead' str = [Unify str, Backtrack]


transBody :: Maybe Goal -> PCode
transBody Nothing = []
transBody (Just (Goal ls)) = concatMap transBody' ls where

  transBody' :: Literal -> PCode
  transBody' (Literal False lt) = Push (CHP 0) : map transBody'' (linLTerm lt) ++ [Call]
  transBody' (Literal True lt) = Push (CHP 1): map transBody'' (linLTerm lt) ++ [Call]

  transBody'' :: StackElem -> Command
  transBody'' (STR s i) = Push $ Atom s i


linLTerm :: LTerm -> [StackElem]
linLTerm (NVar (NVLTerm a xs)) = STR a (length xs) : concatMap linLTerm xs





createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

  createEnv' :: [Int] -> Int -> Int -> PCode -> Env
  createEnv' cs _ 0 ((Unify _) : t)= --case: first command of pcode is unify
    createEnv' (0:cs) 0 1 t
  createEnv' cs _ i (Return:Backtrack:(Unify _) : t) =
    createEnv' ((i+2):cs) 0 (i + 3) t
  createEnv' cs _ i (Return:Backtrack:(Push _) : t) =
    createEnv' cs (i + 2) (i + 3) t
  createEnv' cs g i (Prompt : _) =
    Env {clauses = reverse cs, cGoal = g, cLast = i}
  createEnv' cs g i (_ : t) =
    createEnv' cs g (i + 1) t


cNext :: Env -> Int -> Int
cNext env = cNext' (clauses env) where

  cNext' :: [Int] -> Int -> Int
  cNext' [] _ = -1
  cNext' (h:t) i
    | h <= i = cNext' t i
    | otherwise = h


cFirst :: Env -> Int
cFirst Env {clauses = []} = -1
cFirst _ = 0
