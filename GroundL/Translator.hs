module Translator(
  translate,
  createEnv,
  cNext,
  cFirst
)
  where

import Declarations

linLTerm :: LTerm -> [StackElem]
linLTerm (NVar (NVLTerm a xs)) = (STR a (length xs)) : concatMap linLTerm xs

translate :: SyntaxTree -> PCode
translate (SyntaxTree cs g) = concatMap translate' cs
  ++ transBody (Just g)
  ++ [Backtrack, Prompt] where

  translate' :: PClause -> PCode
  translate' (PClause nvlt g) = transHead nvlt
    ++ transBody g
    ++ [Return]


transHead :: NVLTerm -> PCode
transHead nvlt = concatMap transHead' $ linLTerm $ NVar nvlt

transHead' :: StackElem -> PCode
transHead' str = [Unify str, Backtrack]


transBody :: Maybe Goal -> PCode
transBody Nothing = []
transBody (Just (Goal ls)) = concatMap transBody' ls where

  transBody' :: Literal -> PCode
  transBody' (Literal _ lt) = (Push CHP) : (map transBody'' $ linLTerm lt) ++ [Call]

  transBody'' :: StackElem -> Command
  transBody'' (STR s i) = Push $ Atom s i


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

  createEnv' :: [Int] -> Int -> Int -> PCode -> Env
  createEnv' cs _ 0 ((Unify _):t)=
    createEnv' (0:cs) 0 1 t
  createEnv' cs _ i (Return:(Unify _):t) =
    createEnv' ((i+1):cs) 0 (i + 2) t
  createEnv' cs _ i (Return:(Push _):t) =
    createEnv' cs (i + 1) (i + 2) t
  createEnv' cs g i (Prompt:_) =
    Env {clauses = reverse cs, cGoal = g, cLast = i}
  createEnv' cs g i (_:t) =
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
