module MiniTranslator(
  translate,
  createEnv,
  cNext,
  cFirst
)
  where

import Declarations


translate :: SyntaxTree -> PCode
translate (SyntaxTree cs g) = concatMap translate' cs
  ++ transBody (Just g)
  ++ [Backtrack, Prompt] where

  translate' :: PClause -> PCode
  translate' (PClause nvlt g) = transHead nvlt
    ++ transBody g
    ++ [Return]


transHead :: NVLTerm -> PCode
transHead (NVLTerm s _) = [Unify $ STR s, Backtrack]


transBody :: Maybe Goal -> PCode
transBody Nothing = []
transBody (Just (Goal ls)) = concatMap transBody' ls where

  transBody' :: Literal -> PCode
  transBody' (Literal _ (NVar nvlt)) = transBody'' nvlt

  transBody'' :: NVLTerm -> PCode
  transBody'' (NVLTerm s _) = [Push $ STR s, Call]


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

  createEnv' :: [Int] -> Int -> Int -> PCode -> Env
  createEnv' cs _ c ((Unify _):t) =
    createEnv' (c:cs) 0 (c + 1) t
  createEnv' cs _ c (Return:(Push s):t) =
    createEnv' cs (c + 1) (c + 2) t
  createEnv' cs g c (Prompt:_) =
    Env (reverse cs) g c
  createEnv' cs g c (_:t) =
    createEnv' cs g (c + 1) t


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
