module MiniTranslator(
  translate,
  createEnv,
  cNext
)
  where

import Declarations


translate :: SyntaxTree -> PCode
translate (SyntaxTree pks z) =
  concatMap translate' pks ++ translateBody (Just z) ++ [Prompt] where

  translate' :: PKlausel -> PCode
  translate' (PKlausel nvlt z) =
    translateHead nvlt ++ translateBody z ++ [Return]


translateHead :: NVLTerm -> PCode
translateHead (NVLTerm s _) = [Unify $ Atom s, Backtrack]


translateBody :: Maybe Ziel -> PCode
translateBody Nothing = []
translateBody (Just (Ziel ls)) = concatMap translateBody' ls where

  translateBody' :: Literal -> PCode
  translateBody' (Literal _ lt) =
    case lt of
         NVar nvlt -> translateBody'' nvlt

  translateBody'' :: NVLTerm -> PCode
  translateBody'' (NVLTerm s _) = [Push $ Atom s, Call, Backtrack]


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

  createEnv' :: [Int] -> Int -> Int -> PCode -> Env
  createEnv' ks _ c ((Unify _):t) = createEnv' (c:ks) 0 (c + 1) t
  createEnv' ks _ c (Return:(Push s):t) = createEnv' ks (c + 1) (c + 2) t
  createEnv' ks z c (Prompt:_) = Env (reverse ks) z c
  createEnv' ks z c (_:t) = createEnv' ks z (c + 1) t


cNext :: Env -> Int ->  Int
cNext env = cNext' (clauses env) where

  cNext' :: [Int] -> Int -> Int
  cNext' (h:t) i
    | h /= i = cNext' t i
    | otherwise = case t of
                       x:_ -> x
                       _ -> -1
