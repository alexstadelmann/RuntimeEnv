module Translator(
   translate,
   createEnv,
   c_first,
   c_next,
   c_goal,
   c_last,
   -- testProgramm 
) where

import Declarations

linNVLTerm :: NVLTerm -> [LTermElem]
linNVLTerm (NVLTerm a []) = [STR a 0]
linNVLTerm (NVLTerm a xs) = (STR a (length xs)): concatMap linLTerm xs

linLTerm :: LTerm-> [LTermElem]
linLTerm (Var a) = [VAR a (-1)]
linLTerm (NVar (NVLTerm a [])) = [STR a 0]
linLTerm (NVar (NVLTerm a xs)) = (STR a (length xs)) : concatMap linLTerm xs



translate :: Programm -> PCode
translate (Programm pks z) =
    concatMap translate' pks ++ translateBody (Just z) ++ [Prompt] where

    translate' :: PKlausel -> PCode
    translate' (PKlausel nvlt z) =
        translateHead nvlt ++ translateBody z ++ [Return]


translateHead :: NVLTerm -> PCode
translateHead (NVLTerm s _) = [Unify (Atom s), Backtrack]


translateBody :: Maybe Ziel -> PCode
translateBody Nothing = []
translateBody (Just (Ziel ls)) = concatMap translateBody' ls where

    translateBody' :: Literal -> PCode
    translateBody' (Literal _ lt) =
        case lt of NVar nvlt -> translateBody'' nvlt

    translateBody'' :: NVLTerm -> PCode
    translateBody'' (NVLTerm s _) = [Push (Atom s), Call, Backtrack]


createEnv :: PCode -> Env
createEnv = createEnv' [] 0 0 where

    createEnv' :: [Int] -> Int -> Int -> PCode -> Env
    createEnv' ks _ c ((Unify _):t) = createEnv' (c:ks) 0 (c+1) t
    createEnv' ks _ c (Return:(Push s):t) = createEnv' ks (c+1) (c+2) t
    createEnv' ks z c (Prompt:_) = Env (reverse ks) z c
    createEnv' ks z c (_:t) = createEnv' ks z (c+1) t


c_first :: Env -> Int
c_first env = case klauseln env of
                   [] -> (-1)
                   ks -> 0


c_next :: Env -> Int ->  Int
c_next env = c_next' (klauseln env) where

    c_next' :: [Int] -> Int -> Int
    c_next' (h:t) i
        | h /= i = c_next' t i
        | otherwise = case t of
                           (x:_) -> x
                           _ -> (-1)


c_goal :: Env -> Int
c_goal = goal


c_last :: Env -> Int
c_last = letzte



-- testProgramm :: Programm
-- testProgramm = Programm [k1, k2, k3] z3
-- 
-- k1 :: PKlausel
-- k1 = PKlausel p (Just z1)
-- 
-- k2 :: PKlausel
-- k2 = PKlausel q (Just z2)
-- 
-- k3 :: PKlausel
-- k3 = PKlausel r Nothing
-- 
-- z1 :: Ziel
-- z1 = Ziel [Literal False (NVar q)]
-- 
-- z2 :: Ziel
-- z2 = Ziel [Literal False (NVar r)]
-- 
-- z3 :: Ziel
-- z3 = Ziel [Literal False (NVar p), Literal False (NVar r)]
-- 
-- z4 :: Ziel
-- z4 = Ziel [Literal False (NVar p)]
-- 
-- p :: NVLTerm
-- p = NVLTerm "p" []
-- 
-- q :: NVLTerm
-- q = NVLTerm "q" []
-- 
-- r :: NVLTerm
-- r = NVLTerm "r" []
